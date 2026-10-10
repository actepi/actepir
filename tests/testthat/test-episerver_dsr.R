# tests/testthat/test-episerver_dsr.R
#
# Offline tests for the DSR Calculator: settings, diagnosis codes, SQL, year
# ranges, the code and tables it copies, the map and the app's server logic. The app runs against a fake query function
# that answers each SQL statement with synthetic data, so nothing here needs
# EpiServer. The live queries are tested in test-episerver_dsr_live.R.

library(testthat)
library(actepir)

# ── Synthetic EpiServer ─────────────────────────────────────────────────────

fake_standard <- data.frame(
  AgeGroup     = 1:18,
  AgeGroupName = c(sprintf("%02d-%02d", seq(0, 80, 5), seq(4, 84, 5)), "85+"),
  StdPopValue  = c(1282357, 1351664, 1353177, 1352745, 1302412, 1407081,
                   1466615, 1492204, 1479257, 1358594, 1300777, 1008799,
                   822024, 682513, 638380, 519356, 330050, 265235)
)

fake_population <- local({
  p <- expand.grid(AgeGroup = 1:18, Sex = 1:2)
  data.frame(
    Year         = 2023L,
    Sex          = p$Sex,
    AgeGroup     = p$AgeGroup,
    AgeGroupName = fake_standard$AgeGroupName[p$AgeGroup],
    Area         = "ACT",
    Population   = 10000L + 100L * p$AgeGroup + 50L * p$Sex
  )
})

fake_events <- local({
  e <- expand.grid(AgeGroup = c(1:18, NA), Sex = c(1L, 2L, 9L), Area = c("ACT", NA),
                   stringsAsFactors = FALSE)
  e$Events <- with(e, ifelse(is.na(Area), 3L, ifelse(Sex == 9, 1L, 20L + AgeGroup)))
  e$Events[is.na(e$Events)] <- 2L
  data.frame(Year = 2023L, e[c("Sex", "AgeGroup", "Area", "Events")])
})

fake_episerver <- function() {
  calls <- character(0)
  query <- function(sql) {
    calls <<- c(calls, sql)
    if (grepl("GROUP BY StdPopCode", sql, fixed = TRUE)) {
      return(data.frame(StdPopCode = c(101L, 300L),
                        StdPopName = c("Australia (2001)", "European (1976)"),
                        Note       = c("ABS Census", "Superceded")))
    }
    if (grepl("FROM Analysis.dbo.StandardPops", sql, fixed = TRUE)) {
      return(fake_standard)
    }
    if (grepl("MIN(PresentationDateTime)", sql, fixed = TRUE)) {
      return(data.frame(FirstDate = as.POSIXct("2004-07-01 00:12:00", tz = "UTC"),
                        LastDate  = as.POSIXct("2025-06-30 23:50:00", tz = "UTC")))
    }
    if (grepl("MIN(SeparationDate)", sql, fixed = TRUE)) {
      return(data.frame(FirstDate = as.Date("1991-07-01"),
                        LastDate  = as.Date("2025-06-30")))
    }
    if (grepl("MIN(ERPYear)", sql, fixed = TRUE)) {
      return(data.frame(FirstYear = 1971L, LastYear = 2024L))
    }
    if (grepl("AS Events", sql, fixed = TRUE)) return(fake_events)
    if (grepl("AS Population", sql, fixed = TRUE)) return(fake_population)
    stop("Unexpected SQL: ", sql)
  }
  list(query = query, calls = function() calls)
}

all_specs <- function(years = 2023, codes = NULL, diagnosis = "principal",
                      pool = 1) {
  grid <- expand.grid(dataset = c("ED", "APC"),
                      scope = c("ACT", "SURROUNDS", "AUS"),
                      level = c("state", "sa3", "sa2"), stringsAsFactors = FALSE)
  lapply(seq_len(nrow(grid)), function(i) {
    .dsr_spec(grid$dataset[i], grid$scope[i], grid$level[i], years,
              codes = codes, diagnosis = diagnosis, pool = pool)
  })
}

# The same events and populations for 2021 to 2023, a little higher each year
fake_years <- function(df, col, step) {
  do.call(rbind, lapply(2021:2023, function(y) {
    df$Year  <- y
    df[[col]] <- df[[col]] + step * (y - 2021L)
    df
  }))
}
events_3y     <- fake_years(fake_events, "Events", 1L)
population_3y <- fake_years(fake_population, "Population", 100L)

# The NSW SA3s of the ACT and surrounds, as the SQL lists them
surrounds_sql <- "('10102', '10103', '10106', '11302')"

# One-degree squares side by side from 149E 35S, one per area, as
# .dsr_boundaries() returns them
square_polys <- function(areas) {
  do.call(rbind, lapply(seq_along(areas), function(i) {
    data.frame(Area = areas[i], Name = paste("Area", areas[i]),
               x = 148 + i + c(0, 1, 1, 0, 0), y = -35 + c(0, 0, 1, 1, 0),
               group = paste(areas[i], 1), subgroup = 1L)
  }))
}

strip_html <- function(x) gsub("<[^>]+>", "", x)

# ── Settings ────────────────────────────────────────────────────────────────

test_that(".dsr_spec validates and normalises the settings", {

  s <- .dsr_spec("APC", "AUS", "sa2", 2023, "300")
  expect_equal(s, list(dataset = "APC", scope = "AUS", level = "sa2",
                       years = c(2023L, 2023L), standard = 300L,
                       codes = .dsr_parse_codes(NULL), diagnosis = "principal",
                       pool = 1L))
  expect_equal(nrow(s$codes), 0)
  expect_equal(.dsr_spec(scope = "SURROUNDS", years = 2023)$scope, "SURROUNDS")

  # ED has no principal diagnosis field, so it always searches every field
  expect_equal(.dsr_spec("ED", years = 2023, diagnosis = "principal")$diagnosis,
               "all")
  expect_equal(.dsr_spec("APC", years = 2023, diagnosis = "principal")$diagnosis,
               "principal")

  s <- .dsr_spec(years = 2023, codes = c("j45", "C13-C15.45"), diagnosis = "all")
  expect_equal(s$codes$From, c("J45", "C13"))
  expect_equal(s$diagnosis, "all")

  expect_equal(.dsr_spec(years = c(2019, 2023))$years, c(2019L, 2023L))

  expect_error(.dsr_spec("XX", years = 2023))
  expect_error(.dsr_spec(scope = "NSW", years = 2023))
  expect_error(.dsr_spec(level = "sa1", years = 2023))
  expect_error(.dsr_spec(years = c(2023, 2019)), "earliest first")
  expect_error(.dsr_spec(years = "2023; DROP TABLE x"), "calendar year")
  expect_error(.dsr_spec(years = 2023, standard = "abc"), "StdPopCode")
  expect_error(.dsr_spec(years = 2023, diagnosis = "secondary"))
  expect_error(.dsr_spec(years = 2023, codes = "J45' OR 1=1 --"),
               "is not an ICD-10 code")

})

test_that("pooled years must divide the range into equal periods", {

  expect_equal(.dsr_spec(years = c(2015, 2023), pool = "3")$pool, 3L)
  expect_equal(.dsr_spec(years = c(2019, 2023), pool = 5)$pool, 5L)
  expect_error(.dsr_spec(years = c(2016, 2023), pool = 3),
               paste("3-year periods need a number of years that is a multiple",
                     "of 3: 2016 to 2023 is 8 years."), fixed = TRUE)
  expect_error(.dsr_spec(years = 2023, pool = 2), "2023 to 2023 is 1 year.",
               fixed = TRUE)
  expect_error(.dsr_spec(years = 2023, pool = 0), "whole number of years")
  expect_error(.dsr_spec(years = 2023, pool = "x"), "whole number of years")

})

test_that("pooled years fall into periods counted from the first year", {

  expect_equal(.dsr_period(2015:2023, 2015, 3),
               rep(c("2015-2017", "2018-2020", "2021-2023"), each = 3))
  expect_equal(.dsr_period(2019:2023, 2019, 5), rep("2019-2023", 5))

  s <- .dsr_spec(years = c(2021, 2024), pool = 2)
  expect_equal(.dsr_time(s), "Period")
  expect_equal(.dsr_pool(data.frame(Year = 2021:2024), s)$Period,
               c("2021-2022", "2021-2022", "2023-2024", "2023-2024"))

  s <- .dsr_spec(years = 2023)
  expect_equal(.dsr_time(s), "Year")
  expect_null(.dsr_pool(data.frame(Year = 2023), s)$Period)

})

test_that("the ACT and surrounds add the NSW SA3s on the ACT border", {

  expect_equal(names(.dsr_surrounds), c("10102", "10103", "10106", "11302"))
  expect_equal(unname(.dsr_surrounds),
               c("Queanbeyan", "Snowy Mountains", "Young - Yass",
                 "Tumut - Tumbarumba"))

})

# ── Diagnosis codes ─────────────────────────────────────────────────────────

test_that(".dsr_parse_codes reads codes, partial codes and ranges", {

  p <- .dsr_parse_codes(c("C13-C15.45", "e18.3 \u2013 E18.78", "J45",
                          "I21.*", "K70 to K77", "C13 - C15.45"))
  expect_equal(p$Label, c("C13-C15.45", "E18.3-E18.78", "J45", "I21", "K70-K77"))
  expect_equal(p$From,  c("C13", "E183", "J45", "I21", "K70"))
  expect_equal(p$To,    c("C1545", "E1878", "J45", "I21", "K77"))

  # Several to an entry, separated by commas or semicolons
  expect_equal(.dsr_parse_codes("J45, J46; j45.")$Label, c("J45", "J46"))

  expect_equal(nrow(.dsr_parse_codes(NULL)), 0)
  expect_equal(nrow(.dsr_parse_codes(c("", " , "))), 0)
  expect_named(.dsr_parse_codes(NULL), c("Label", "From", "To"))

})

test_that(".dsr_parse_codes rejects anything but codes and ranges", {

  bad <- c("C13-", "-C15", "C13-C14-C15", "13", "CC13", "C1X", "C13.4567",
           "M8140/3", "C13 OR 1=1", "J45'", "C13_")
  for (b in bad) {
    expect_error(.dsr_parse_codes(b), "is not an ICD-10 code", label = b)
  }
  expect_error(.dsr_parse_codes("C15-C13"), "comes after the second")
  expect_error(.dsr_parse_codes("E18.78-E18.3"), "comes after the second")

})

test_that("a range takes in the codes between its ends and under its last", {

  codes <- .dsr_parse_codes(c("C13-C15.45", "E18.3-E18.78", "J45"))
  inside <- c("C13", "C13.0", "C14.9", "C15", "C15.4", "C15.45", "C15.459",
              "E18.3", "E18.31", "E18.5", "E18.78", "E18.789", "J45",
              "J45.9", " j45.0 ")
  outside <- c("C12.9", "C15.46", "C15.5", "C16", "E18.2", "E18.79", "E18.8",
               "J44.9", "J46", "M8140/3", "", NA)
  expect_true(all(dsr_code_matches(inside, codes)))
  expect_false(any(dsr_code_matches(outside, codes)))

})

test_that(".dsr_code_cmp orders codes as SQL Server orders letters and digits", {

  expect_equal(.dsr_code_cmp("C13", "C13"), 0)
  expect_equal(.dsr_code_cmp("C13", "C134"), -1)
  expect_equal(.dsr_code_cmp("C159", "C1545"), 1)
  expect_equal(.dsr_code_cmp("C19", "CA"), -1)

})

test_that(".dsr_pop_table names the ERP5 table for the settings", {

  expect_equal(.dsr_pop_table(.dsr_spec("ED", "ACT", "state", 2023)), "ERP5_STE_ACT")
  expect_equal(.dsr_pop_table(.dsr_spec("ED", "AUS", "sa3", 2023)), "ERP5_SA3_AUS")
  expect_equal(.dsr_pop_table(.dsr_spec("APC", "ACT", "sa2", 2023)), "ERP5_SA2_ACT")

  # The ACT and surrounds use the Australian tables, with states from SA3s
  expect_equal(.dsr_pop_table(.dsr_spec("ED", "SURROUNDS", "state", 2023)),
               "ERP5_SA3_AUS")
  expect_equal(.dsr_pop_table(.dsr_spec("ED", "SURROUNDS", "sa3", 2023)),
               "ERP5_SA3_AUS")
  expect_equal(.dsr_pop_table(.dsr_spec("ED", "SURROUNDS", "sa2", 2023)),
               "ERP5_SA2_AUS")

})

# ── SQL ─────────────────────────────────────────────────────────────────────

test_that("events SQL counts by calendar year of the dataset's date", {

  ed <- .dsr_sql_events(.dsr_spec("ED", "ACT", "state", c(2019, 2023)))
  expect_match(ed, "FROM Analysis.dbo.ED", fixed = TRUE)
  expect_match(ed, "YEAR(PresentationDateTime) AS Year", fixed = TRUE)
  expect_match(ed, "PresentationDateTime >= '20190101'", fixed = TRUE)
  expect_match(ed, "PresentationDateTime < '20240101'", fixed = TRUE)
  expect_match(ed, "WHEN AgeYrs >= 85 THEN 18", fixed = TRUE)
  expect_match(ed, "WHEN AgeYrs >= 0 THEN AgeYrs / 5 + 1", fixed = TRUE)

  apc <- .dsr_sql_events(.dsr_spec("APC", "ACT", "state", 2023))
  expect_match(apc, "FROM Analysis.dbo.APC", fixed = TRUE)
  expect_match(apc, "YEAR(SeparationDate) AS Year", fixed = TRUE)
  expect_match(apc, "AgeYears / 5 + 1", fixed = TRUE)

})

test_that("events SQL takes the area from the geo_ column of the level", {

  expect_match(.dsr_sql_events(.dsr_spec("ED", "AUS", "state", 2023)),
               "geo_STATE AS Area", fixed = TRUE)
  expect_match(.dsr_sql_events(.dsr_spec("ED", "AUS", "sa3", 2023)),
               "geo_SA3_2021 AS Area", fixed = TRUE)
  expect_match(.dsr_sql_events(.dsr_spec("ED", "AUS", "sa2", 2023)),
               "geo_SA2_2021 AS Area", fixed = TRUE)

})

test_that("events SQL keeps ACT residents and unrecorded residence for the ACT", {

  expect_match(.dsr_sql_events(.dsr_spec("ED", "ACT", "state", 2023)),
               "(geo_STATE = 'ACT' OR geo_STATE IS NULL)", fixed = TRUE)
  expect_match(.dsr_sql_events(.dsr_spec("ED", "ACT", "sa2", 2023)),
               "(geo_SA2_2021 LIKE '8%' OR geo_SA2_2021 IS NULL)", fixed = TRUE)

  # Australian residents: no residence filter at all
  aus <- .dsr_sql_events(.dsr_spec("ED", "AUS", "sa3", 2023))
  expect_no_match(aus, "LIKE", fixed = TRUE)
  expect_no_match(aus, "IS NULL", fixed = TRUE)

})

test_that("events SQL keeps the ACT and surrounds, states from the SA3", {

  sa3 <- .dsr_sql_events(.dsr_spec("ED", "SURROUNDS", "sa3", 2023))
  expect_match(sa3, "geo_SA3_2021 AS Area", fixed = TRUE)
  expect_match(sa3, paste0("(geo_SA3_2021 LIKE '8%' OR geo_SA3_2021 IN ",
                           surrounds_sql, " OR geo_SA3_2021 IS NULL)"),
               fixed = TRUE)

  sa2 <- .dsr_sql_events(.dsr_spec("ED", "SURROUNDS", "sa2", 2023))
  expect_match(sa2, "geo_SA2_2021 AS Area", fixed = TRUE)
  expect_match(sa2, paste0("(geo_SA2_2021 LIKE '8%' OR LEFT(geo_SA2_2021, 5) IN ",
                           surrounds_sql, " OR geo_SA2_2021 IS NULL)"),
               fixed = TRUE)

  st <- .dsr_sql_events(.dsr_spec("ED", "SURROUNDS", "state", 2023))
  expect_match(st, paste0(
    "CASE WHEN geo_SA3_2021 LIKE '8%' THEN 'ACT'\n",
    "                WHEN geo_SA3_2021 IN ", surrounds_sql,
    " THEN 'NSW (surrounds)'\n           END AS Area"), fixed = TRUE)
  expect_no_match(st, "geo_STATE", fixed = TRUE)

})

test_that("diagnosis SQL searches the principal or every diagnosis field", {

  # ED searches its four diagnosis fields even when asked for the principal
  ed <- .dsr_sql_events(.dsr_spec("ED", "ACT", "state", 2023, codes = "J45",
                                  diagnosis = "principal"))
  expect_match(ed, "AND COALESCE(ICD10AMEdition, 0) <> 99", fixed = TRUE)
  expect_match(ed, paste0("FROM (VALUES (EDShortListCode), (Diagnosis1), ",
                          "(Diagnosis2), (Diagnosis3)) AS d (Code)"),
               fixed = TRUE)
  expect_match(ed, "c.Code LIKE 'J45%'", fixed = TRUE)
  expect_match(ed, "c.Code NOT LIKE '%/%'", fixed = TRUE)

  apc <- .dsr_sql_events(.dsr_spec("APC", "ACT", "state", 2023, codes = "J45",
                                   diagnosis = "principal"))
  expect_match(apc, "FROM (VALUES (Diagnosis1)) AS d (Code)", fixed = TRUE)

  apc <- .dsr_sql_codes(.dsr_spec("APC", years = 2023,
                                  codes = c("C13-C15.45", "J45"),
                                  diagnosis = "all"))
  expect_match(apc, "AND ICDVersion = 10", fixed = TRUE)
  fields <- regmatches(apc, gregexpr("\\(Diagnosis[0-9]+\\)", apc))[[1]]
  expect_equal(fields, paste0("(Diagnosis", 1:100, ")"))
  expect_match(
    apc,
    "(c.Code >= 'C13' AND (c.Code <= 'C1545' OR c.Code LIKE 'C1545%'))\n                 OR c.Code LIKE 'J45%')",
    fixed = TRUE
  )

  # No codes, no condition
  expect_equal(.dsr_sql_codes(.dsr_spec(years = 2023)), "")
  expect_no_match(.dsr_sql_events(.dsr_spec(years = 2023)), "Diagnosis",
                  fixed = TRUE)

})

test_that("population SQL reads the ERP5 table for the same years", {

  st <- .dsr_sql_population(.dsr_spec("ED", "AUS", "state", c(2019, 2023)))
  expect_match(st, "FROM Analysis.dbo.ERP5_STE_AUS", fixed = TRUE)
  expect_match(st, "WHERE ERPYear BETWEEN 2019 AND 2023", fixed = TRUE)
  expect_match(st, "AND EntityCode BETWEEN 1 AND 9", fixed = TRUE)
  expect_match(st, "WHEN 8 THEN 'ACT'", fixed = TRUE)
  expect_match(st, "WHEN 1 THEN 'NSW'", fixed = TRUE)

  sa3 <- .dsr_sql_population(.dsr_spec("ED", "ACT", "sa3", 2023))
  expect_match(sa3, "FROM Analysis.dbo.ERP5_SA3_ACT", fixed = TRUE)
  expect_match(sa3, "CAST(EntityCode AS varchar(9)) AS Area", fixed = TRUE)
  expect_no_match(sa3, "EntityCode BETWEEN", fixed = TRUE)

})

test_that("population SQL selects the ACT and surrounds from the Australian tables", {

  code <- "CAST(EntityCode AS varchar(9))"
  sa2 <- .dsr_sql_population(.dsr_spec("ED", "SURROUNDS", "sa2", 2023))
  expect_match(sa2, "FROM Analysis.dbo.ERP5_SA2_AUS", fixed = TRUE)
  expect_match(sa2, paste0("AND (", code, " LIKE '8%' OR LEFT(", code, ", 5) IN ",
                           surrounds_sql, ")"), fixed = TRUE)
  expect_no_match(sa2, "GROUP BY", fixed = TRUE)

  # States: the ACT and the NSW surrounds, each summed from its SA3s
  st <- .dsr_sql_population(.dsr_spec("ED", "SURROUNDS", "state", 2023))
  area <- paste0("CASE WHEN ", code, " LIKE '8%' THEN 'ACT' ELSE ",
                 "'NSW (surrounds)' END")
  expect_match(st, "FROM Analysis.dbo.ERP5_SA3_AUS", fixed = TRUE)
  expect_match(st, paste0(area, " AS Area"), fixed = TRUE)
  expect_match(st, "SUM(ERPCount) AS Population", fixed = TRUE)
  expect_match(st, paste0("GROUP BY ERPYear, SexCode, AgeGroup05Code, ",
                          "AgeGroup05Name,\n         ", area), fixed = TRUE)

})

test_that("standard SQL selects the 5-year age groups of one standard", {

  sql <- .dsr_sql_standard(.dsr_spec(years = 2023, standard = 300))
  expect_match(sql, "WHERE StdPopCode = 300", fixed = TRUE)
  expect_match(sql, "AgeGroupTypeName = 'AgeGroup05Code'", fixed = TRUE)

})

test_that("every SQL statement is complete and safe to embed in R code", {

  coded <- c(all_specs(codes = c("C13-C15.45", "E18.3 to E18.78", "J45"),
                       diagnosis = "all"),
             all_specs(codes = "J45", diagnosis = "principal"))
  sqls <- c(
    unlist(lapply(all_specs(), function(s) {
      c(.dsr_sql_events(s), .dsr_sql_population(s), .dsr_sql_standard(s),
        .dsr_sql_erp_years(s))
    })),
    vapply(coded, .dsr_sql_events, character(1)),
    .dsr_sql_standards(), .dsr_sql_dates("ED"), .dsr_sql_dates("APC")
  )

  # No placeholder left unfilled, and nothing that would end or escape the
  # double-quoted R string the copied code wraps the SQL in
  expect_false(any(grepl("{", sqls, fixed = TRUE)))
  expect_false(any(grepl("\"", sqls, fixed = TRUE)))
  expect_false(any(grepl("\\", sqls, fixed = TRUE)))

})

# ── Years ───────────────────────────────────────────────────────────────────

test_that(".dsr_year_range offers years covered by events and population", {

  r <- .dsr_year_range(as.Date("2004-07-01"), as.Date("2025-06-30"), c(1971, 2024))
  expect_equal(r$min, 2004)
  expect_equal(r$max, 2024)
  expect_equal(r$complete, c(2005L, 2024L))
  expect_equal(r$default, 2024)

  # SA3 populations end a year earlier
  r <- .dsr_year_range(as.Date("2004-07-01"), as.Date("2025-06-30"), c(2001, 2023))
  expect_equal(c(r$min, r$max, r$default), c(2004, 2023, 2023))

  # Whole calendar years at both ends
  r <- .dsr_year_range(as.Date("2010-01-01"), as.Date("2020-12-31"), c(2001, 2024))
  expect_equal(r$complete, c(2010L, 2020L))

})

test_that(".dsr_year_range reads date-times by their recorded calendar date", {

  r <- .dsr_year_range(as.POSIXct("2004-07-01 00:12:00", tz = "UTC"),
                       as.POSIXct("2024-12-31 23:50:00", tz = "UTC"),
                       c(1971, 2024))
  expect_equal(r$complete, c(2005L, 2024L))

})

test_that(".dsr_year_range copes with unknown dates and no overlap", {

  r <- .dsr_year_range(NA, NA, c(2001, 2023))
  expect_equal(c(r$min, r$max, r$default), c(2001, 2023, 2023))
  expect_equal(.dsr_partial_years(c(2001L, 2023L), r), integer(0))

  expect_null(.dsr_year_range(as.Date("2024-07-01"), as.Date("2025-06-30"),
                              c(2001, 2023)))
  expect_null(.dsr_year_range(as.Date("2004-07-01"), as.Date("2025-06-30"),
                              c(NA, NA)))

})

test_that(".dsr_partial_years flags years the events only partly cover", {

  r <- .dsr_year_range(as.Date("2004-07-01"), as.Date("2025-06-30"), c(1971, 2024))
  expect_equal(.dsr_partial_years(c(2004L, 2006L), r), 2004L)
  expect_equal(.dsr_partial_years(c(2005L, 2024L), r), integer(0))

})

# ── Copied code ─────────────────────────────────────────────────────────────

test_that("the copied code parses and embeds the calculator's SQL", {

  specs <- c(all_specs(c(2019, 2023)),
             all_specs(c(2015, 2023), pool = 3),
             all_specs(2023, codes = c("C13-C15.45", "J45"), diagnosis = "all"))
  for (s in specs) {
    code <- .dsr_code(s, by_sex = TRUE, age_specific = TRUE, suppress = 5)
    expect_no_error(parse(text = code))
    expect_true(grepl(.dsr_sql_events(s), code, fixed = TRUE))
    expect_true(grepl(.dsr_sql_population(s), code, fixed = TRUE))
    expect_true(grepl(.dsr_sql_standard(s), code, fixed = TRUE))
  }

})

test_that("the copied code follows the display options", {

  s <- .dsr_spec("ED", "ACT", "sa3", 2023)

  plain <- .dsr_code(s, standard_name = "Australia (2001)")
  expect_match(plain, "by_sex = FALSE, multiplier = 100000", fixed = TRUE)
  expect_no_match(plain, "dsr_age_specific", fixed = TRUE)
  expect_match(plain, "^# DSR Calculator: ED presentations, ACT residents, by SA3")

  full <- .dsr_code(s, by_sex = TRUE, age_specific = TRUE, multiplier = 1000)
  expect_match(full, "by_sex = TRUE, multiplier = 1000\n", fixed = TRUE)
  expect_match(full, "age_rates <- actepir::dsr_age_specific(", fixed = TRUE)

  supp <- .dsr_code(s, age_specific = TRUE, suppress = 5)
  expect_equal(lengths(regmatches(supp, gregexpr(
    "multiplier = 100000, suppress = 5\n", supp, fixed = TRUE))), 2)

  coded <- .dsr_code(.dsr_spec("APC", "ACT", "sa3", 2023, codes = "J45",
                               diagnosis = "all"))
  expect_match(coded, "^# DSR Calculator: APC separations, any diagnosis J45,")

  pooled <- .dsr_code(.dsr_spec("ED", "SURROUNDS", "sa3", c(2015, 2023),
                                pool = 3))
  expect_match(pooled, paste0(
    "^# DSR Calculator: ED presentations, residents of the ACT and surrounds, ",
    "by SA3, 2015 to 2023 in 3-year periods"))
  expect_match(pooled, paste0(
    "period <- function(year) {\n",
    "  start <- year - (year - 2015) %% 3\n",
    "  paste0(start, \"-\", start + 2)\n",
    "}\n",
    "events$Period     <- period(events$Year)\n",
    "population$Period <- period(population$Year)\n"), fixed = TRUE)
  expect_match(pooled, "by = c(\"Period\", \"Area\")", fixed = TRUE)
  expect_no_match(plain, "Period", fixed = TRUE)

})

test_that("running the copied code reproduces dsr_calculate()", {

  fake <- fake_episerver()
  s <- .dsr_spec("ED", "ACT", "state", 2023)
  code <- .dsr_code(s, by_sex = TRUE, age_specific = TRUE, suppress = 25)

  # Swap the EpiServer calls for the fake; the rest runs as written
  code <- gsub("actepir::episerver_connect", "fake_connect", code, fixed = TRUE)
  code <- gsub("DBI::dbGetQuery", "fake_get", code, fixed = TRUE)
  code <- gsub("DBI::dbDisconnect", "fake_disconnect", code, fixed = TRUE)

  env <- new.env()
  env$fake_connect    <- function() "conn"
  env$fake_get        <- function(conn, sql) fake$query(sql)
  env$fake_disconnect <- function(conn) invisible(TRUE)
  suppressMessages(eval(parse(text = code), envir = env))

  expected <- suppressMessages(dsr_calculate(
    fake_events, fake_population, fake_standard,
    by = c("Year", "Area"), by_sex = TRUE, suppress = 25
  ))
  expect_equal(env$rates, expected)
  expect_equal(nrow(env$age_rates), 18 * 3)
  expect_equal(sum(env$age_rates$Suppressed), 8)

})

test_that("running pooled copied code gives rates per person-year", {

  query <- function(sql) {
    if (grepl("AS Events", sql, fixed = TRUE)) return(events_3y)
    if (grepl("AS Population", sql, fixed = TRUE)) return(population_3y)
    fake_episerver()$query(sql)
  }
  code <- .dsr_code(.dsr_spec("ED", "ACT", "state", c(2021, 2023), pool = 3),
                    by_sex = TRUE, age_specific = TRUE)
  code <- gsub("actepir::episerver_connect", "fake_connect", code, fixed = TRUE)
  code <- gsub("DBI::dbGetQuery", "fake_get", code, fixed = TRUE)
  code <- gsub("DBI::dbDisconnect", "fake_disconnect", code, fixed = TRUE)

  env <- new.env()
  env$fake_connect    <- function() "conn"
  env$fake_get        <- function(conn, sql) query(sql)
  env$fake_disconnect <- function(conn) invisible(TRUE)
  suppressMessages(eval(parse(text = code), envir = env))

  expect_equal(env$rates$Period, rep("2021-2023", 3))
  expect_equal(unique(env$age_rates$Period), "2021-2023")

  # Each year's events meet that year's population, then events and
  # populations are summed over the period: the same as leaving Year out
  pooled <- suppressMessages(dsr_calculate(events_3y, population_3y,
                                           fake_standard, by = "Area",
                                           by_sex = TRUE))
  expect_equal(env$rates[names(pooled)[-1]], pooled[-1], ignore_attr = TRUE)
  expect_equal(env$rates$Population[3], sum(population_3y$Population))

})

# ── Display ─────────────────────────────────────────────────────────────────

rates_df <- data.frame(
  Year = 2023L, Area = c("80101", "80103"), Sex = "Persons",
  Events = c(NA, 1234), Population = c(1500, 250000),
  Crude = c(NA, 493.6), CrudeLower = c(NA, 466.4), CrudeUpper = c(NA, 522),
  DSR = c(NA, 480.26), DSRLower = c(NA, 450), DSRUpper = c(NA, 511.1),
  Suppressed = c(TRUE, FALSE)
)

test_that(".dsr_describe can leave out the years", {

  s <- .dsr_spec("ED", "ACT", "sa3", c(2019, 2023))
  expect_equal(.dsr_describe(s), "ED presentations, ACT residents, by SA3, 2019 to 2023")
  expect_equal(.dsr_describe(s, "Australia (2001)", years = FALSE),
               "ED presentations, ACT residents, by SA3, standardised to Australia (2001)")

  expect_equal(
    .dsr_describe(.dsr_spec("ED", "SURROUNDS", "sa2", c(2015, 2023), pool = 3)),
    "ED presentations, residents of the ACT and surrounds, by SA2, 2015 to 2023 in 3-year periods"
  )
  expect_equal(.dsr_describe(.dsr_spec(years = c(2019, 2023), pool = 5)),
               "ED presentations, ACT residents, by state or territory, 2019 to 2023 pooled")

})

test_that("tables copy as tab-separated text with suppressed counts marked", {

  tsv <- .dsr_tsv(rates_df, "SA3", 100000, suppress = 5)
  lines <- strsplit(tsv, "\n", fixed = TRUE)[[1]]
  expect_equal(lines[1], paste(
    "Year", "SA3", "Sex", "Events", "Population", "Crude rate",
    "Crude lower 95% CI", "Crude upper 95% CI", "Age-standardised rate",
    "DSR lower 95% CI", "DSR upper 95% CI", sep = "\t"))
  expect_equal(lines[2], "2023\t80101\tPersons\t<5\t1500\t\t\t\t\t\t")
  expect_equal(lines[3], paste(
    "2023", "80103", "Persons", "1234", "250000", "493.6", "466.4", "522.0",
    "480.3", "450.0", "511.1", sep = "\t"))

  # Reads back as a table of the same shape
  back <- utils::read.delim(text = tsv, check.names = FALSE,
                            colClasses = "character")
  expect_equal(dim(back), c(2, 11))

  # Age-specific rates have an age group column and two decimals per 1,000
  age <- data.frame(Year = 2023L, Area = "ACT", Sex = "Male",
                    AgeGroupName = "00-04", Events = 3, Population = 20000,
                    Rate = 0.15, RateLower = 0.0309, RateUpper = 0.4384,
                    Suppressed = FALSE)
  lines <- strsplit(.dsr_tsv(age, "State or territory", 1000, age = TRUE),
                    "\n", fixed = TRUE)[[1]]
  expect_equal(lines, c(
    paste("Year", "State or territory", "Sex", "Age group", "Events",
          "Population", "Age-specific rate", "Lower 95% CI", "Upper 95% CI",
          sep = "\t"),
    paste("2023", "ACT", "Male", "00\u201304", "3", "20000", "0.15", "0.03",
          "0.44", sep = "\t")
  ))

  # Pooled results have a Period column, also written with an en dash
  pooled <- data.frame(Period = "2021-2023", rates_df[-1])
  lines <- strsplit(.dsr_tsv(pooled, "SA3", 100000, suppress = 5), "\n",
                    fixed = TRUE)[[1]]
  expect_match(lines[1], "^Period\tSA3\tSex\t")
  expect_match(lines[3], "^2021\u20132023\t80103\tPersons\t1234\t")
  expect_equal(names(.dsr_datatable(pooled, "SA3", 100000)$x$data)[1], "Period")

})

test_that("tables show suppressed counts below the threshold", {

  tbl <- .dsr_datatable(rates_df, "SA3", 100000, suppress = 5)
  expect_false("Suppressed" %in% names(tbl$x$data))

  defs <- tbl$x$options$columnDefs
  events <- Filter(function(d) identical(d$targets, 3L) && !is.null(d$render),
                   defs)
  expect_length(events, 1)
  expect_match(events[[1]]$render, "'&lt;5'", fixed = TRUE)

})

# ── App ─────────────────────────────────────────────────────────────────────

test_that("the app calculates from the queried counts", {

  fake <- fake_episerver()
  app <- episerver_dsr_app(query = fake$query)

  shiny::testServer(app, {
    session$setInputs(dataset = "ED", scope = "ACT", level = "state",
                      standard = "101", years = c(2023, 2023),
                      multiplier = "100000", by_sex = FALSE,
                      age_specific = FALSE)
    session$setInputs(calculate = 1)

    r <- results()
    expected <- suppressMessages(dsr_calculate(
      fake_events, fake_population, fake_standard, by = c("Year", "Area")
    ))
    expect_equal(r$summary, expected)
    expect_equal(r$spec$years, c(2023L, 2023L))

    # Unrecorded residence and age are counted for the info bar
    ex <- attr(r$summary, "excluded")
    expect_equal(.dsr_reasons(ex$Reason),
                 c("age not recorded", "residence not recorded"))

    # Display options recalculate without querying the events again
    n_events <- sum(grepl("AS Events", fake$calls(), fixed = TRUE))
    session$setInputs(by_sex = TRUE, multiplier = "1000")
    r <- results()
    expect_equal(r$summary$Sex, c("Male", "Female", "Persons"))
    expect_equal(r$multiplier, 1000)
    expect_equal(sum(grepl("AS Events", fake$calls(), fixed = TRUE)), n_events)

    # Age-specific rates use the same settings: 18 age groups for each sex
    a <- age_results()
    expect_equal(nrow(a), 18 * 3)
    expect_equal(a$AgeGroupName[1:2], c("00-04", "05-09"))

    # Suppression applies without querying again. Males and females have 21
    # to 24 events in the four youngest age groups; a fractional threshold
    # rounds up.
    session$setInputs(suppress = 24.2)
    expect_equal(results()$suppress, 25)
    expect_false(any(results()$summary$Suppressed))
    expect_equal(sum(age_results()$Suppressed), 8)
    expect_match(strip_html(output$info_bar$html),
                 "fewer than 25 events are suppressed: 0 of 3 rows")
    expect_equal(sum(grepl("AS Events", fake$calls(), fixed = TRUE)), n_events)

    session$setInputs(suppress = NA)
    expect_equal(results()$suppress, 0)
    expect_match(strip_html(output$info_bar$html), "No rows are suppressed")
  })

})

test_that("the app copies the code for the current settings", {

  fake <- fake_episerver()
  app <- episerver_dsr_app(query = fake$query)
  sent <- NULL
  local_mocked_bindings(.epi_send_code = function(session, code) sent <<- code)

  shiny::testServer(app, {
    session$setInputs(dataset = "APC", scope = "AUS", level = "sa3",
                      standard = "300", years = c(2019, 2022),
                      multiplier = "10000", by_sex = TRUE,
                      age_specific = TRUE, suppress = 5,
                      codes = c("C13-C15.45", "J45"), diagnosis = "all")
    session$setInputs(insert_code = 1)
  })

  expect_equal(sent, .dsr_code(
    .dsr_spec("APC", "AUS", "sa3", c(2019, 2022), 300,
              codes = c("C13-C15.45", "J45"), diagnosis = "all"),
    by_sex = TRUE, age_specific = TRUE, multiplier = 10000,
    standard_name = "European (1976) (superseded)", suppress = 5
  ))

})

test_that("the app reports a failed query instead of stopping", {

  fake <- fake_episerver()
  failing <- function(sql) {
    if (grepl("AS Events", sql, fixed = TRUE)) stop("Login timeout expired")
    fake$query(sql)
  }
  app <- episerver_dsr_app(query = failing)

  shiny::testServer(app, {
    session$setInputs(dataset = "ED", scope = "ACT", level = "state",
                      standard = "101", years = c(2023, 2023),
                      multiplier = "100000", by_sex = FALSE,
                      age_specific = FALSE)
    session$setInputs(calculate = 1)
    expect_match(rv$error, "Login timeout expired")
    expect_null(rv$fetched)
  })

})

test_that("the app searches diagnosis codes and checks them first", {

  fake <- fake_episerver()
  app <- episerver_dsr_app(query = fake$query)

  shiny::testServer(app, {
    session$setInputs(dataset = "APC", scope = "ACT", level = "state",
                      standard = "101", years = c(2023, 2023),
                      multiplier = "100000", suppress = 5,
                      codes = c("C13-C15.45", "e18.3 to e18.78"),
                      diagnosis = "all")
    session$setInputs(calculate = 1)

    sql <- grep("AS Events", fake$calls(), value = TRUE, fixed = TRUE)
    expect_length(sql, 1)
    expect_match(sql, "(Diagnosis100)", fixed = TRUE)
    expect_match(sql, "c.Code >= 'E183'", fixed = TRUE)
    info <- strip_html(output$info_bar$html)
    expect_match(info, "any diagnosis C13-C15.45 or E18.3-E18.78", fixed = TRUE)
    expect_match(info, "ICD-10 coded records only")

    session$setInputs(diagnosis = "principal")
    expect_match(strip_html(output$info_bar$html), "settings have changed")

    # An entry that is not a code stops the calculation and says why
    session$setInputs(codes = c("C13-C15.45", "C13-"))
    expect_null(spec())
    expect_match(strip_html(output$codes_note$html), "is not an ICD-10 code")
    session$setInputs(calculate = 2)
    expect_length(grep("AS Events", fake$calls(), fixed = TRUE), 1)
  })

})

test_that("the app copies each table for pasting into a spreadsheet", {

  fake <- fake_episerver()
  app <- episerver_dsr_app(query = fake$query)
  copied <- NULL
  local_mocked_bindings(
    .epi_copy_text = function(session, text, message) copied <<- text
  )

  shiny::testServer(app, {
    session$setInputs(dataset = "ED", scope = "ACT", level = "state",
                      standard = "101", years = c(2023, 2023),
                      multiplier = "100000", suppress = 25, by_sex = TRUE,
                      age_specific = TRUE)

    # Nothing to copy before the first calculation
    session$setInputs(copy_summary = 1)
    expect_null(copied)

    session$setInputs(calculate = 1)

    session$setInputs(copy_summary = 2)
    expect_equal(copied, .dsr_tsv(results()$summary, "State or territory",
                                  100000, suppress = 25))
    session$setInputs(copy_age = 1)
    expect_equal(copied, .dsr_tsv(age_results(), "State or territory",
                                  100000, age = TRUE, suppress = 25))
    expect_match(copied, "\t<25\t", fixed = TRUE)
  })

})

test_that("the app maps the rates and identifies the area clicked", {

  fake <- fake_episerver()
  app <- episerver_dsr_app(query = fake$query)
  requested <- NULL
  local_mocked_bindings(.dsr_boundaries = function(level, scope, ...) {
    requested <<- c(requested, paste(level, scope))
    square_polys(c("ACT", "NSW"))
  })

  shiny::testServer(app, {
    session$setInputs(dataset = "ED", scope = "AUS", level = "state",
                      standard = "101", years = c(2023, 2023),
                      multiplier = "100000", suppress = 5, by_sex = FALSE)
    session$setInputs(calculate = 1)
    session$setInputs(map_value = "DSR", map_year = "2023", map_sex = "Persons")

    p <- map_plot()
    expect_s3_class(p, "ggplot")
    labs <- p$labels
    expect_equal(labs$title, "Age-standardised rate per 100,000, 2023, persons")
    expect_match(gsub("\n", " ", labs$subtitle),
                 "Australian residents, by state or territory, standardised")
    expect_match(labs$caption, "fewer than 5 events (suppressed)", fixed = TRUE)

    act <- results()$summary
    expect_equal(unique(p$data$Value[p$data$Area == "ACT"]), act$DSR)
    expect_true(all(is.na(p$data$Value[p$data$Area == "NSW"])))

    session$setInputs(map_value = "Events")
    expect_equal(unique(map_plot()$data$Value[map_plot()$data$Area == "ACT"]),
                 act$Events)

    # Clicking an area reports its figures; outside any area, nothing
    session$setInputs(map_click = list(x = 149.5, y = -34.5))
    info <- strip_html(output$map_click_info$html)
    expect_match(info, "State or territory ACT Area ACT:", fixed = TRUE)
    expect_match(info, paste0(.fmt_count(act$Events), " events"), fixed = TRUE)
    session$setInputs(map_click = list(x = 150.5, y = -34.5))
    expect_match(strip_html(output$map_click_info$html), "no results")

    # Boundaries are fetched once for the level and population
    session$setInputs(by_sex = TRUE)
    invisible(map_plot())
    expect_equal(requested, "state AUS")
  })

})

test_that("the app explains a map it cannot draw", {

  fake <- fake_episerver()
  app <- episerver_dsr_app(query = fake$query)
  local_mocked_bindings(.dsr_boundaries = function(level, scope, ...) {
    stop("Could not resolve host: geo.abs.gov.au", call. = FALSE)
  })

  shiny::testServer(app, {
    session$setInputs(dataset = "ED", scope = "ACT", level = "state",
                      standard = "101", years = c(2023, 2023),
                      multiplier = "100000", suppress = 5)
    expect_match(strip_html(output$map_note$html), "Press Calculate")

    session$setInputs(calculate = 1)
    expect_match(strip_html(output$map_note$html), "Choose SA3 or SA2")

    session$setInputs(scope = "AUS", calculate = 2)
    note <- strip_html(output$map_note$html)
    expect_match(note, "could not be downloaded from the ABS")
    expect_match(note, "Could not resolve host: geo.abs.gov.au", fixed = TRUE)
  })

})

test_that("the app pools years and reuses the counts when only pooling changes", {

  calls <- character(0)
  query <- function(sql) {
    calls <<- c(calls, sql)
    if (grepl("AS Events", sql, fixed = TRUE)) return(events_3y)
    if (grepl("AS Population", sql, fixed = TRUE)) return(population_3y)
    fake_episerver()$query(sql)
  }
  n_events <- function() sum(grepl("AS Events", calls, fixed = TRUE))
  app <- episerver_dsr_app(query = query)

  shiny::testServer(app, {
    session$setInputs(dataset = "ED", scope = "ACT", level = "state",
                      standard = "101", years = c(2021, 2023),
                      multiplier = "100000", pool = "3")
    expect_match(strip_html(output$years_note$html), "One period: 2021-2023.",
                 fixed = TRUE)
    session$setInputs(calculate = 1)

    r <- results()
    expect_equal(r$by, c("Period", "Area"))
    expect_equal(r$summary$Period, "2021-2023")
    expect_match(strip_html(output$info_bar$html), "2021 to 2023 pooled")
    expect_equal(n_events(), 1)

    # Back to single years: the same queries, so the counts are reused
    session$setInputs(pool = "1")
    expect_match(strip_html(output$info_bar$html), "settings have changed")
    session$setInputs(calculate = 2)
    expect_equal(results()$summary$Year, 2021:2023)
    expect_equal(n_events(), 1)

    # Three years do not make 2-year periods
    session$setInputs(pool = "2")
    expect_null(spec())
    expect_match(strip_html(output$years_note$html),
                 "2-year periods need a number of years that is a multiple of 2")
    session$setInputs(calculate = 3)
    expect_equal(results()$summary$Year, 2021:2023)

    session$setInputs(pool = "all", calculate = 4)
    expect_equal(results()$summary$Period, "2021-2023")
    expect_equal(n_events(), 1)
  })

})

test_that("the app searches every ED diagnosis field", {

  fake <- fake_episerver()
  app <- episerver_dsr_app(query = fake$query)

  shiny::testServer(app, {
    session$setInputs(dataset = "ED", scope = "ACT", level = "state",
                      standard = "101", years = c(2023, 2023),
                      multiplier = "100000", codes = "J45",
                      diagnosis = "principal")
    expect_match(strip_html(output$codes_note$html), paste0(
      "ED searches all its diagnosis fields (EDShortListCode, Diagnosis1, ",
      "Diagnosis2, Diagnosis3)"), fixed = TRUE)
    session$setInputs(calculate = 1)
    sql <- grep("AS Events", fake$calls(), value = TRUE, fixed = TRUE)
    expect_match(sql, "(EDShortListCode), (Diagnosis1), (Diagnosis2), (Diagnosis3)",
                 fixed = TRUE)
    expect_match(strip_html(output$info_bar$html), "any diagnosis J45")

    session$setInputs(dataset = "APC")
    expect_no_match(strip_html(output$codes_note$html), "ED searches")
  })

})

test_that("the app maps the ACT and surrounds by state", {

  fake <- fake_episerver()
  app <- episerver_dsr_app(query = fake$query)
  requested <- NULL
  local_mocked_bindings(.dsr_boundaries = function(level, scope, ...) {
    requested <<- c(requested, paste(level, scope))
    square_polys(c("ACT", "NSW (surrounds)"))
  })

  shiny::testServer(app, {
    session$setInputs(dataset = "ED", scope = "SURROUNDS", level = "state",
                      standard = "101", years = c(2023, 2023),
                      multiplier = "100000", suppress = 5)
    session$setInputs(calculate = 1)
    session$setInputs(map_value = "DSR", map_year = "2023", map_sex = "Persons")
    expect_match(strip_html(output$map_note$html), "Click an area")
    p <- map_plot()
    expect_equal(sort(unique(p$data$Area)), c("ACT", "NSW (surrounds)"))
    expect_null(p$coordinates$limits$x)
    expect_equal(requested, "state SURROUNDS")
  })

})
