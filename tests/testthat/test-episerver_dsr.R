# tests/testthat/test-episerver_dsr.R
#
# Offline tests for the DSR Calculator: settings, diagnosis codes, SQL, year
# ranges, the code and tables it copies, the Explore tab and the app's server logic. The app runs against a fake query function
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

# ICD-10-AM diagnoses and ACHI procedures as .dsr_sql_icd() returns them,
# with invented titles and blocks. The table holds editions 3 to 11; X99.9
# and 90004-00 left after edition 9 and U07.1 came in edition 10. M8140/3 is
# a morphology code, " h81.0 " has outer spaces and a title in Windows-1252,
# as the ODBC driver can return it, and 90010-00 has no block.
fake_icd <- local({
  diagnoses <- data.frame(
    VarType = "Diagnosis",
    Code = c("I27", "I21.4", "I21", "I21.0", "I20", "I22", "I25", "I25.2",
             "J45", "J45.0", "J45.9", "J46", "A41.50", "A41.51", " h81.0 ",
             "M8140/3", "X99.9", "U07.1"),
    Description = c(
      "Other pulmonary heart test condition",
      "Acute subendocardial myocardial test condition",
      "Acute myocardial test condition",
      "Acute myocardial test condition of anterior wall",
      "Test chest pain condition", "Subsequent myocardial test condition",
      "Chronic heart test condition", "Old myocardial test condition",
      "Test wheeze condition", "Allergic test wheeze condition",
      "Test wheeze condition, unspecified", "Severe test wheeze condition",
      "Test sepsis, first organism", "Test sepsis due to E. coli",
      iconv("M\u00e9ni\u00e8re test condition", "UTF-8", "CP1252"),
      "Test morphology", "Retired test condition", "New test condition"),
    ProcedureBlock = NA_integer_, BlockDescription = NA_character_,
    FirstEdition = c(rep(3L, 17), 10L),
    LastEdition  = c(rep(11L, 16), 9L, 11L),
    stringsAsFactors = FALSE
  )
  procedures <- data.frame(
    VarType = "Procedure",
    Code = c("90003-00", "90001-01", "90001-00", "90002-00", "90004-00",
             "90010-00"),
    Description = c("Test hearing check",
                    "Test nerve conduction study with muscle recording",
                    "Test nerve conduction study, one nerve",
                    "Test brain recording", "Retired test procedure",
                    "Test procedure, unassigned"),
    ProcedureBlock = c(1905L, 1901L, 1901L, 1900L, 1905L, NA),
    BlockDescription = c("Test hearing checks", "Test nerve studies",
                         "Test nerve studies", "Test brain recordings",
                         "Old test hearing checks", NA),
    FirstEdition = 3L, LastEdition = c(11L, 11L, 11L, 11L, 9L, 11L),
    stringsAsFactors = FALSE
  )
  rbind(diagnoses, procedures)
})

fake_episerver <- function() {
  calls <- character(0)
  query <- function(sql) {
    calls <<- c(calls, sql)
    if (grepl("FROM Analysis.dbo.ICD10AM", sql, fixed = TRUE)) return(fake_icd)
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
                      pool = 1, ages = c(1, 18)) {
  grid <- expand.grid(dataset = c("ED", "APC"),
                      scope = c("ACT", "SURROUNDS"),
                      level = c("state", "sa3", "sa2"), stringsAsFactors = FALSE)
  lapply(seq_len(nrow(grid)), function(i) {
    .dsr_spec(grid$dataset[i], grid$scope[i], grid$level[i], years,
              codes = codes, diagnosis = diagnosis, pool = pool, ages = ages)
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

  s <- .dsr_spec("APC", "SURROUNDS", "sa2", 2023, "300")
  expect_equal(s, list(dataset = "APC", scope = "SURROUNDS", level = "sa2",
                       years = c(2023L, 2023L), standard = 300L,
                       codes = .dsr_parse_codes(NULL), diagnosis = "principal",
                       pool = 1L, ages = c(1L, 18L),
                       procedures = .dsr_parse_procedures(NULL),
                       procedure = "principal", combine = "and"))
  expect_equal(nrow(s$codes), 0)
  expect_equal(.dsr_spec(years = 2023)$scope, "ACT")

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
  # Australian residents are not offered
  expect_error(.dsr_spec(scope = "AUS", years = 2023))
  expect_named(.dsr_scopes, c("ACT", "SURROUNDS"))
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

test_that("SA3s and SA2s are labelled with the ASGS edition of the tables", {

  expect_equal(.dsr_asgs$year, 2021L)
  expect_equal(.dsr_levels$sa3$column, "geo_SA3_2021")
  expect_equal(.dsr_levels$sa2$column, "geo_SA2_2021")
  expect_equal(.dsr_area_label("state"), "State or territory")
  expect_equal(.dsr_area_label("sa3"), "SA3 (ASGS 2021)")
  expect_equal(.dsr_area_label("sa2"), "SA2 (ASGS 2021)")

})

# ── Ages ────────────────────────────────────────────────────────────────────

test_that("ages are a range of 5-year age groups, all ages by default", {

  expect_equal(.dsr_spec(years = 2023)$ages, c(1L, 18L))
  expect_equal(.dsr_spec(years = 2023, ages = c("8", "15"))$ages, c(8L, 15L))
  expect_equal(.dsr_spec(years = 2023, ages = c(18, 18))$ages, c(18L, 18L))

  msg <- "'ages' must be two age group codes from 1 (0-4) to 18 (85+), lowest first."
  expect_error(.dsr_spec(years = 2023, ages = c(15, 8)), msg, fixed = TRUE)
  expect_error(.dsr_spec(years = 2023, ages = c(0, 18)), msg, fixed = TRUE)
  expect_error(.dsr_spec(years = 2023, ages = c(1, 19)), msg, fixed = TRUE)
  expect_error(.dsr_spec(years = 2023, ages = 8), msg, fixed = TRUE)
  expect_error(.dsr_spec(years = 2023, ages = c("x", "18")), msg, fixed = TRUE)

})

test_that("an age range keeps its age groups and events of unknown age", {

  df <- data.frame(AgeGroup = c(1:18, NA), Events = 1)
  expect_identical(.dsr_truncate(df, c(1L, 18L)), df)
  expect_equal(.dsr_truncate(df, c(8L, 15L))$AgeGroup, c(8:15, NA))
  expect_equal(.dsr_truncate(df, c(18L, 18L))$AgeGroup, c(18L, NA))

  expect_null(.dsr_ages_text(c(1L, 18L)))
  expect_equal(.dsr_ages_text(c(1L, 13L)), "under 65")
  expect_equal(.dsr_ages_text(c(1L, 15L)), "under 75")
  expect_equal(.dsr_ages_text(c(1L, 1L)), "under 5")
  expect_equal(.dsr_ages_text(c(8L, 15L)), "35 to 74")
  expect_equal(.dsr_ages_text(c(5L, 9L)), "20 to 44")
  expect_equal(.dsr_ages_text(c(14L, 18L)), "65 and over")
  expect_equal(.dsr_ages_text(c(18L, 18L)), "85 and over")

})

test_that("rates for an age range are standardised within the range", {

  s <- .dsr_spec(years = 2023, ages = c(8, 15))
  r <- suppressMessages(dsr_calculate(
    .dsr_truncate(fake_events, s$ages), .dsr_truncate(fake_population, s$ages),
    fake_standard, by = c("Year", "Area")
  ))

  # Each age group from 35-39 to 70-74 is weighted by its share of the
  # standard population of those ages, so the weights sum to 1
  g   <- 8:15
  w   <- fake_standard$StdPopValue[g] / sum(fake_standard$StdPopValue[g])
  ev  <- fake_events[!is.na(fake_events$Area) & fake_events$AgeGroup %in% g, ]
  pop <- fake_population[fake_population$AgeGroup %in% g, ]
  events     <- tapply(ev$Events, ev$AgeGroup, sum)
  population <- tapply(pop$Population, pop$AgeGroup, sum)

  expect_equal(r$DSR, sum(w * events / population) * 100000)
  expect_equal(r$Crude, sum(events) / sum(population) * 100000)
  expect_equal(r$Population, sum(population))

  # Events of unknown age are still reported as left out
  expect_true("Missing AgeGroup" %in% attr(r, "excluded")$Reason)

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

test_that(".dsr_parse_procedures reads codes, blocks and ranges", {

  p <- .dsr_parse_procedures(c(
    "11012-00", "1101200", "11012", "11012-0*", "1826", "[1826]", "block 18",
    "1820-1830", "[1820] to [1830]", "11000-00 to 11027-00", "11000-11027",
    "11000-00-11027-00", paste0("13100-00 ", intToUtf8(0x2013), " 13100-09")
  ))
  expect_equal(p$Label, c("11012-00", "11012", "11012-0", "[1826]", "[18]",
                          "[1820] to [1830]", "11000-00 to 11027-00",
                          "11000 to 11027", "13100-00 to 13100-09"))
  expect_equal(p$Type, rep(c("code", "block", "code"), c(3, 3, 3)))
  expect_equal(p$From, c("1101200", "11012", "110120", "1826", "18", "1820",
                         "1100000", "11000", "1310000"))
  expect_equal(p$To, c("1101200", "11012", "110120", "1826", "18", "1830",
                       "1102700", "11027", "1310009"))

  # Several to an entry, separated by commas or semicolons
  expect_equal(.dsr_parse_procedures("11012-00, 1826; 0042")$Label,
               c("11012-00", "[1826]", "[42]"))
  expect_equal(nrow(.dsr_parse_procedures(NULL)), 0)
  expect_named(.dsr_parse_procedures(NULL), c("Label", "Type", "From", "To"))

})

test_that(".dsr_parse_procedures rejects anything but codes, blocks and ranges", {

  bad <- c("11012-001", "1826-11012", "12345678", "abc", "J45", "11012-",
           "-11012", "11012-00 OR 1=1", "11012'", "1820 to 1830 to 1840")
  for (b in bad) {
    expect_error(.dsr_parse_procedures(b), "is not an ACHI procedure code",
                 label = b)
  }
  expect_error(.dsr_parse_procedures("1830-1820"),
               "the first block comes after the second")
  expect_error(.dsr_parse_procedures("11027 to 11000"),
               "the first code comes after the second")

})

test_that("a procedure range takes in the codes or blocks between its ends", {

  procs <- .dsr_parse_procedures(c("11000-00 to 11027-01", "1820-1830",
                                   "13100"))
  inside <- data.frame(
    code  = c("11000-00", "11012-00", "1102700", " 11027-01 ", "11027-011",
              "13100-00", "13100-09", "99999-00", NA),
    block = c(NA, NA, NA, NA, NA, NA, NA, 1820, 1830)
  )
  outside <- data.frame(
    code  = c("10999-99", "11027-02", "11030-00", "13101-00", "", NA),
    block = c(NA, NA, NA, NA, 1819, 1831)
  )
  expect_true(all(dsr_procedure_matches(inside$code, inside$block, procs)))
  expect_false(any(dsr_procedure_matches(outside$code, outside$block, procs)))

})

test_that(".dsr_code_cmp orders codes as SQL Server orders letters and digits", {

  expect_equal(.dsr_code_cmp("C13", "C13"), 0)
  expect_equal(.dsr_code_cmp("C13", "C134"), -1)
  expect_equal(.dsr_code_cmp("C159", "C1545"), 1)
  expect_equal(.dsr_code_cmp("C19", "CA"), -1)

})

# ── Diagnosis titles ────────────────────────────────────────────────────────

test_that("the titles query takes each code from its latest edition", {

  sql <- .dsr_sql_icd()
  expect_match(sql, "FROM Analysis.dbo.ICD10AM", fixed = TRUE)
  expect_match(sql, "WHERE VarType IN ('Diagnosis', 'Procedure')",
               fixed = TRUE)
  expect_match(sql, "MIN(ICD10AMVersion) OVER (PARTITION BY VarType, Code)",
               fixed = TRUE)
  expect_match(sql, "ProcedureBlock, BlockDescription", fixed = TRUE)
  expect_match(sql, "ORDER BY ICD10AMVersion DESC) AS Latest", fixed = TRUE)
  expect_match(sql, "WHERE Latest = 1", fixed = TRUE)
  # The table's dates are not reliable
  expect_no_match(sql, "StartDate|EndDate")

})

test_that(".dsr_icd_titles orders the codes and names the editions that hold them", {

  icd <- .dsr_icd_titles(fake_icd)
  title <- function(code) icd$Title[icd$Code == code]

  # Morphology codes are left out, and each code comes before those under it
  expect_equal(icd$Code, c("A41.50", "A41.51", "H81.0", "I20", "I21", "I21.0",
                           "I21.4", "I22", "I25", "I25.2", "I27", "J45",
                           "J45.0", "J45.9", "J46", "U07.1", "X99.9"))
  expect_equal(icd$Key[icd$Code == "I21.4"], "I214")
  expect_equal(attr(icd, "editions"), c(3L, 11L))

  # Codes that some editions lack say which hold them
  expect_equal(title("I21"), "Acute myocardial test condition")
  expect_equal(title("X99.9"), "Retired test condition (editions 3 to 9)")
  expect_equal(title("U07.1"), "New test condition (editions 10 to 11)")

  # Windows-1252 text is read as UTF-8, and matched without its accents
  expect_equal(title("H81.0"), "Ménière test condition")
  expect_equal(icd$Words[icd$Code == "H81.0"], " h810 meniere test condition")
  expect_equal(icd$Words[icd$Code == "A41.51"],
               " a4151 test sepsis due to e coli")

  expect_null(.dsr_icd_titles(NULL))
  expect_null(.dsr_icd_titles(fake_icd[0, ]))

})

test_that(".dsr_icd_search matches words typed to the start of words in codes and titles", {

  icd <- .dsr_icd_titles(fake_icd)
  codes <- function(text, ...) .dsr_icd_search(icd, text, ...)$Code

  expect_equal(codes("myocard"), c("I21", "I21.0", "I21.4", "I22", "I25.2"))
  # Every word must match the start of a word, in any case
  expect_equal(codes("ACUTE Myoc"), c("I21", "I21.0", "I21.4"))
  expect_equal(codes("acute old"), character(0))
  expect_equal(codes("cardial"), character(0))
  # Codes match with or without their dot and bring the codes under them
  expect_equal(codes("I21"), c("I21", "I21.0", "I21.4"))
  expect_equal(codes("i21.4"), "I21.4")
  expect_equal(codes("I214"), "I21.4")
  # Accents and punctuation do not matter
  expect_equal(codes("meniere"), "H81.0")
  expect_equal(codes("ménière"), "H81.0")
  expect_equal(codes("e. coli"), "A41.51")
  # At most n, in code order, and none for nothing to match
  expect_equal(codes("test", n = 2), c("A41.50", "A41.51"))
  for (text in c("", "  ", "-")) {
    expect_length(codes(text), 0)
  }

})

test_that(".dsr_icd_titles lists procedures by block, each block before its codes", {

  icd <- .dsr_icd_titles(fake_icd, "Procedure")
  expect_equal(icd$Code, c("[1900]", "90002-00", "[1901]", "90001-00",
                           "90001-01", "[1905]", "90003-00", "90004-00",
                           "90010-00"))
  expect_equal(icd$Type, c("block", "code", "block", "code", "code", "block",
                           "code", "code", "code"))
  expect_equal(icd$Key[icd$Code %in% c("[1901]", "90001-01")],
               c("1901", "9000101"))
  expect_equal(attr(icd, "classification"), "ACHI")
  expect_equal(attr(.dsr_icd_titles(fake_icd), "classification"), "ICD-10-AM")

  # A block takes its description from the latest edition that holds it
  expect_equal(icd$Title[icd$Code == "[1905]"], "Test hearing checks")
  expect_equal(icd$Title[icd$Code == "90004-00"],
               "Retired test procedure (editions 3 to 9)")
  expect_equal(icd$Words[icd$Code == "[1901]"], " block 1901 test nerve studies")

  # Each type is read only from its own rows
  expect_false(any(grepl("^[A-Z]", icd$Code)))
  expect_false(any(grepl("^[0-9]", .dsr_icd_titles(fake_icd)$Code)))
  expect_null(.dsr_icd_titles(fake_icd[fake_icd$VarType == "Diagnosis", ],
                              "Procedure"))

})

test_that(".dsr_icd_search finds procedures and blocks by words, codes and numbers", {

  icd <- .dsr_icd_titles(fake_icd, "Procedure")
  codes <- function(text) .dsr_icd_search(icd, text)$Code

  expect_equal(codes("nerve"), c("[1901]", "90001-00", "90001-01"))
  expect_equal(codes("nerve muscle"), "90001-01")
  # Codes match with or without their hyphen, and blocks by number
  expect_equal(codes("90001-01"), "90001-01")
  expect_equal(codes("9000101"), "90001-01")
  expect_equal(codes("90001"), c("90001-00", "90001-01"))
  expect_equal(codes("1905"), "[1905]")
  expect_equal(codes("block"), c("[1900]", "[1901]", "[1905]"))

})

test_that("suggestions are answered as JSON with their place in code order", {

  icd <- .dsr_icd_titles(fake_icd)
  res <- .dsr_icd_response(icd, list(QUERY_STRING = "?q=old%20myocard"))
  expect_equal(res$status, 200L)
  expect_match(res$content_type, "application/json", fixed = TRUE)
  rows <- jsonlite::fromJSON(res$content)
  expect_equal(rows$value, "I25.2")
  expect_equal(rows$title, "Old myocardial test condition")
  expect_equal(rows$words, " i252 old myocardial test condition")
  expect_equal(rows$rank, match("I25.2", icd$Code))

  for (q in c("", "?q=")) {
    res <- .dsr_icd_response(icd, list(QUERY_STRING = q))
    expect_length(jsonlite::fromJSON(res$content), 0)
  }

})

test_that(".dsr_icd_describe spells out codes, partial codes and ranges", {

  icd <- .dsr_icd_titles(fake_icd)
  absent <- "not in ICD-10-AM editions 3 to 11"
  d <- .dsr_icd_describe(
    .dsr_parse_codes(c("I21", "I2", "I25-I27", "J45.0-J46.9", "A41.5",
                       "X99.9", "Y01", "C13-C15.45")),
    icd
  )
  expect_equal(d$Label, c("I21", "I2", "I25-I27", "J45.0-J46.9", "A41.5",
                          "X99.9", "Y01", "C13-C15.45"))
  expect_equal(d$Text, c(
    "Acute myocardial test condition",
    "I20 Test chest pain condition to I27 Other pulmonary heart test condition",
    "I25 Chronic heart test condition to I27 Other pulmonary heart test condition",
    paste0("J45.0 Allergic test wheeze condition to J46.9 (", absent, ")"),
    "A41.50 Test sepsis, first organism to A41.51 Test sepsis due to E. coli",
    "Retired test condition (editions 3 to 9)",
    absent,
    paste0("C13 (", absent, ") to C15.45 (", absent, ")")
  ))
  expect_equal(nrow(.dsr_icd_describe(.dsr_parse_codes(NULL), icd)), 0)

  # Procedures: codes, partial codes, blocks and ranges of either
  icd <- .dsr_icd_titles(fake_icd, "Procedure")
  absent <- "not in ACHI editions 3 to 11"
  d <- .dsr_icd_describe(
    .dsr_parse_procedures(c("90001-00", "90001", "1901", "1900-1905",
                            "1900-1902", "90002-00 to 90003-00", "9999")),
    icd
  )
  expect_equal(d$Label, c("90001-00", "90001", "[1901]", "[1900] to [1905]",
                          "[1900] to [1902]", "90002-00 to 90003-00", "[9999]"))
  expect_equal(d$Text, c(
    "Test nerve conduction study, one nerve",
    paste("90001-00 Test nerve conduction study, one nerve to 90001-01 Test",
          "nerve conduction study with muscle recording"),
    "Test nerve studies",
    "[1900] Test brain recordings to [1905] Test hearing checks",
    paste0("[1900] Test brain recordings to [1902] (", absent, ")"),
    "90002-00 Test brain recording to 90003-00 Test hearing check",
    absent
  ))

})

test_that(".dsr_pop_table names the ERP5 table for the settings", {

  expect_equal(.dsr_pop_table(.dsr_spec("ED", "ACT", "state", 2023)), "ERP5_STE_ACT")
  expect_equal(.dsr_pop_table(.dsr_spec("ED", "ACT", "sa3", 2023)), "ERP5_SA3_ACT")
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

  expect_match(.dsr_sql_events(.dsr_spec("ED", "ACT", "state", 2023)),
               "geo_STATE AS Area", fixed = TRUE)
  expect_match(.dsr_sql_events(.dsr_spec("ED", "ACT", "sa3", 2023)),
               "geo_SA3_2021 AS Area", fixed = TRUE)
  expect_match(.dsr_sql_events(.dsr_spec("ED", "ACT", "sa2", 2023)),
               "geo_SA2_2021 AS Area", fixed = TRUE)

})

test_that("events SQL keeps ACT residents and unrecorded residence for the ACT", {

  expect_match(.dsr_sql_events(.dsr_spec("ED", "ACT", "state", 2023)),
               "(geo_STATE = 'ACT' OR geo_STATE IS NULL)", fixed = TRUE)
  expect_match(.dsr_sql_events(.dsr_spec("ED", "ACT", "sa2", 2023)),
               "(geo_SA2_2021 LIKE '8%' OR geo_SA2_2021 IS NULL)", fixed = TRUE)

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

test_that("procedure SQL searches the principal or every procedure and block", {

  principal <- .dsr_sql_codes(.dsr_spec("APC", years = 2023,
                                        procedures = c("11012-00", "1826")))
  expect_match(principal, "AND ICDVersion = 10", fixed = TRUE)
  expect_match(principal,
               "FROM (VALUES (Procedure1, BlockProcedure1)) AS p (Code, Block)",
               fixed = TRUE)
  expect_match(principal,
               "REPLACE(REPLACE(LTRIM(RTRIM(p.Code)), '-', ''), '.', '')",
               fixed = TRUE)
  expect_match(principal,
               "WHERE c.Code LIKE '1101200%'\n             OR p.Block = 1826",
               fixed = TRUE)
  expect_no_match(principal, "Diagnosis", fixed = TRUE)

  all <- .dsr_sql_codes(.dsr_spec("APC", years = 2023, procedure = "all",
                                  procedures = c("11000 to 11027-00",
                                                 "1820-1830")))
  pairs <- regmatches(all, gregexpr("\\(Procedure[0-9]+, BlockProcedure[0-9]+\\)",
                                    all))[[1]]
  expect_equal(pairs, paste0("(Procedure", 1:50, ", BlockProcedure", 1:50, ")"))
  expect_match(all, paste0("(c.Code >= '11000' AND (c.Code <= '1102700' OR ",
                           "c.Code LIKE '1102700%'))"), fixed = TRUE)
  expect_match(all, "p.Block BETWEEN 1820 AND 1830", fixed = TRUE)

  # ED has no procedures, so they are left out
  ed <- .dsr_spec("ED", years = 2023, codes = "J45", procedures = "11012-00")
  expect_equal(nrow(ed$procedures), 0)
  expect_no_match(.dsr_sql_codes(ed), "Procedure", fixed = TRUE)

})

test_that("diagnoses and procedures combine with and or or", {

  spec <- function(combine) {
    .dsr_spec("APC", years = 2023, codes = "J45", procedures = "1826",
              combine = combine)
  }
  both <- .dsr_sql_codes(spec("and"))
  expect_match(both, "\n      AND EXISTS (\n          SELECT 1\n          FROM (VALUES (Diagnosis1))",
               fixed = TRUE)
  expect_match(both, "\n      )\n      AND EXISTS (\n          SELECT 1\n          FROM (VALUES (Procedure1, ",
               fixed = TRUE)
  either <- .dsr_sql_codes(spec("or"))
  expect_match(either, "\n      AND (EXISTS (", fixed = TRUE)
  expect_match(either, "\n      )\n        OR EXISTS (", fixed = TRUE)
  expect_match(either, "p.Block = 1826\n      ))$")

  # Diagnoses alone give the same condition as before procedures were added
  expect_identical(
    .dsr_sql_codes(.dsr_spec("APC", years = 2023, codes = "J45",
                             combine = "or")),
    .dsr_sql_codes(.dsr_spec("APC", years = 2023, codes = "J45"))
  )
  expect_error(.dsr_spec(years = 2023, combine = "xor"))
  expect_error(.dsr_spec(years = 2023, procedure = "secondary"))

})

test_that("population SQL reads the ERP5 table for the same years", {

  st <- .dsr_sql_population(.dsr_spec("ED", "ACT", "state", c(2019, 2023)))
  expect_match(st, "FROM Analysis.dbo.ERP5_STE_ACT", fixed = TRUE)
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
    .dsr_sql_standards(), .dsr_sql_dates("ED"), .dsr_sql_dates("APC"),
    .dsr_sql_icd(),
    .dsr_sql_events(.dsr_spec("APC", years = 2023, codes = "J45",
                              procedures = c("11012-00", "1820-1830"),
                              procedure = "all", combine = "or"))
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

test_that("the year slider starts on the default year and the two before it", {

  expect_equal(.dsr_default_years(list(min = 2004L, default = 2024L)),
               c(2022L, 2024L))
  expect_equal(.dsr_default_years(list(min = 2023L, default = 2024L)),
               c(2023L, 2024L))
  expect_equal(.dsr_default_years(list(min = 2024L, default = 2024L)),
               c(2024L, 2024L))

})

# ── Copied code ─────────────────────────────────────────────────────────────

test_that("the copied code parses and embeds the calculator's SQL", {

  specs <- c(all_specs(c(2019, 2023)),
             all_specs(c(2015, 2023), pool = 3, ages = c(8, 15)),
             all_specs(2023, codes = c("C13-C15.45", "J45"), diagnosis = "all",
                       ages = c(18, 18)))
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
    "by SA3 \\(ASGS 2021\\), 2015 to 2023 in 3-year periods"))
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

test_that("running copied code for an age range gives the rates for those ages", {

  fake <- fake_episerver()
  s <- .dsr_spec("ED", "ACT", "state", 2023, ages = c(8, 15))
  code <- .dsr_code(s, by_sex = TRUE, age_specific = TRUE)
  expect_match(code, paste0(
    "# Ages 35 to 74, age groups 8:15. Events with no recorded age\n",
    "# stay in, to be reported as left out of the rates.\n",
    "events     <- events[is.na(events$AgeGroup) | events$AgeGroup %in% 8:15, ]\n",
    "population <- population[population$AgeGroup %in% 8:15, ]\n"), fixed = TRUE)
  expect_no_match(.dsr_code(.dsr_spec(years = 2023)), "# Ages", fixed = TRUE)
  expect_match(.dsr_code(.dsr_spec(years = 2023, ages = c(18, 18))),
               "events$AgeGroup %in% 18, ]", fixed = TRUE)

  code <- gsub("actepir::episerver_connect", "fake_connect", code, fixed = TRUE)
  code <- gsub("DBI::dbGetQuery", "fake_get", code, fixed = TRUE)
  code <- gsub("DBI::dbDisconnect", "fake_disconnect", code, fixed = TRUE)

  env <- new.env()
  env$fake_connect    <- function() "conn"
  env$fake_get        <- function(conn, sql) fake$query(sql)
  env$fake_disconnect <- function(conn) invisible(TRUE)
  suppressMessages(eval(parse(text = code), envir = env))

  expected <- suppressMessages(dsr_calculate(
    .dsr_truncate(fake_events, s$ages), .dsr_truncate(fake_population, s$ages),
    fake_standard, by = c("Year", "Area"), by_sex = TRUE
  ))
  expect_equal(env$rates, expected)
  expect_equal(unique(env$age_rates$AgeGroup), 8:15)

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
  expect_equal(.dsr_describe(s),
               "ED presentations, ACT residents, by SA3 (ASGS 2021), 2019 to 2023")
  expect_equal(.dsr_describe(s, "Australia (2001)", years = FALSE),
               "ED presentations, ACT residents, by SA3 (ASGS 2021), standardised to Australia (2001)")

  expect_equal(
    .dsr_describe(.dsr_spec("ED", "SURROUNDS", "sa2", c(2015, 2023), pool = 3)),
    "ED presentations, residents of the ACT and surrounds, by SA2 (ASGS 2021), 2015 to 2023 in 3-year periods"
  )
  expect_equal(.dsr_describe(.dsr_spec(years = c(2019, 2023), pool = 5)),
               "ED presentations, ACT residents, by state or territory, 2019 to 2023 pooled")
  expect_equal(
    .dsr_describe(.dsr_spec("APC", "ACT", "state", 2023, ages = c(1, 15))),
    "APC separations, ACT residents aged under 75, by state or territory, 2023"
  )
  # The Explore tab's subtitles leave the ages to the title
  expect_equal(
    .dsr_describe(.dsr_spec("APC", "ACT", "state", 2023, ages = c(1, 15)),
                  years = FALSE, ages = FALSE),
    "APC separations, ACT residents, by state or territory"
  )

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
                      table = "rates")
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
  sent <- list()
  local_mocked_bindings(
    .epi_send_code = function(session, code) sent[[length(sent) + 1]] <<- code
  )

  # The code adds the age-specific rates while their table is shown
  shiny::testServer(app, {
    session$setInputs(dataset = "APC", scope = "SURROUNDS", level = "sa3",
                      standard = "300", years = c(2019, 2022),
                      multiplier = "10000", by_sex = TRUE,
                      table = "age", suppress = 5,
                      codes = c("C13-C15.45", "J45"), diagnosis = "all")
    session$setInputs(insert_code = 1)
    session$setInputs(table = "rates", insert_code = 2)
  })

  code <- function(age_specific) {
    .dsr_code(
      .dsr_spec("APC", "SURROUNDS", "sa3", c(2019, 2022), 300,
                codes = c("C13-C15.45", "J45"), diagnosis = "all"),
      by_sex = TRUE, age_specific = age_specific, multiplier = 10000,
      standard_name = "European (1976) (superseded)", suppress = 5
    )
  }
  expect_length(sent, 2)
  expect_equal(sent[[1]], code(TRUE))
  expect_equal(sent[[2]], code(FALSE))

})

test_that("the bug button opens a report that holds the settings but no data", {

  fake <- fake_episerver()
  app <- episerver_dsr_app(query = fake$query)
  copied <- NULL
  local_mocked_bindings(
    .epi_copy_text = function(session, text, message) copied <<- text
  )
  client <- list(agent = "Mozilla/5.0 (Windows NT 10.0) Edg/130.0",
                 width = 1920, height = 961, ratio = 1.25,
                 errors = list("ex_map: test error"), script_errors = list(),
                 at = 1)

  shiny::testServer(app, {
    session$setInputs(dataset = "ED", scope = "ACT", level = "state",
                      standard = "101", years = c(2023, 2023),
                      multiplier = "100000", suppress = 5, by_sex = FALSE,
                      codes = "J45", table = "rates")
    session$setInputs(calculate = 1)
    session$setInputs(bug_client = client)

    details <- output$bug_details
    expect_match(details, "Calculated: yes", fixed = TRUE)
    expect_match(details, "Settings error: none", fixed = TRUE)
    expect_match(details, "codes = \"J45\"", fixed = TRUE)
    expect_match(details, "years = c(2023, 2023)", fixed = TRUE)
    expect_match(details, "Code button: Copy Code", fixed = TRUE)
    # Copy Code's code for the settings, the session and the browser
    expect_match(details, "dsr_calculate(", fixed = TRUE)
    expect_match(details, paste0("actepir ", utils::packageVersion("actepir")),
                 fixed = TRUE)
    expect_match(details, "Edg/130.0", fixed = TRUE)
    expect_match(details, "Window: 1920 x 961", fixed = TRUE)
    expect_match(details, "Errors shown in the page: ex_map: test error",
                 fixed = TRUE)
    expect_no_match(details, "What happened", fixed = TRUE)
    # No counts from the results (years aside, which the settings hold)
    r <- results()$summary
    counts <- c(r$Events, r$Population)
    for (v in counts[counts >= 100 & !counts %in% 1990:2030]) {
      expect_no_match(details, format(v, big.mark = ","), fixed = TRUE)
      expect_no_match(details, paste0("\\b", v, "\\b"))
    }

    session$setInputs(bug_what = "The rates looked high.", bug_copy = 1)
    expect_match(copied, "What happened\n-------------\nThe rates looked high.",
                 fixed = TRUE)
    expect_match(copied, "codes = \"J45\"", fixed = TRUE)

    # Settings that cannot be used are reported, without code
    session$setInputs(codes = "C13-", bug_client = client)
    expect_match(output$bug_details, "Settings error: .*is not an ICD-10 code")
    expect_no_match(output$bug_details, "dsr_calculate(", fixed = TRUE)
  })

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
                      table = "rates")
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

test_that("the app copies the table shown for pasting into a spreadsheet", {

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
                      table = "rates")

    # Nothing to copy before the first calculation
    session$setInputs(copy_table = 1)
    expect_null(copied)

    session$setInputs(calculate = 1)

    session$setInputs(copy_table = 2)
    expect_equal(copied, .dsr_tsv(results()$summary, "State or territory",
                                  100000, suppress = 25))
    session$setInputs(table = "age", copy_table = 3)
    expect_equal(copied, .dsr_tsv(age_results(), "State or territory",
                                  100000, age = TRUE, suppress = 25))
    expect_match(copied, "\t<25\t", fixed = TRUE)
    session$setInputs(table = "counts", copy_table = 4)
    expect_equal(copied, .dsr_counts_tsv(counts(), "State or territory",
                                         suppress = 25))
  })

})

test_that("the app presents no DSR below the AIHW minimums, and says why", {

  fake <- fake_episerver()
  app <- episerver_dsr_app(query = fake$query)
  sent <- NULL
  copied <- NULL
  local_mocked_bindings(
    .epi_send_code = function(session, code) sent <<- code,
    .epi_copy_text = function(session, text, message) copied <<- text
  )
  n_events <- function() sum(grepl("AS Events", fake$calls(), fixed = TRUE))

  shiny::testServer(app, {
    session$setInputs(dataset = "ED", scope = "ACT", level = "state",
                      standard = "101", years = c(2023, 2023),
                      multiplier = "100000", by_sex = TRUE, table = "rates",
                      min_events = 20, min_population = 30)
    session$setInputs(calculate = 1)
    expect_true(all(is.na(results()$summary$DSRNote)))

    # Males and females have 531 events each, persons 1,080. The minimums
    # recalculate without querying the events again.
    session$setInputs(min_events = 600)
    s <- results()$summary
    expect_equal(s$DSRNote, c("fewer than 600 events", "fewer than 600 events",
                              NA))
    expect_true(all(is.na(s[1:2, c("DSR", "DSRLower", "DSRUpper")])))
    expect_false(anyNA(s$Crude))
    expect_match(strip_html(output$info_bar$html),
                 paste("Age-standardised rates are not presented (n.p.) for 2",
                       "of 3 rows: 2 with fewer than 600 events"),
                 fixed = TRUE)
    expect_equal(n_events(), 1)
    session$setInputs(copy_table = 1)
    male <- strsplit(strsplit(copied, "\n")[[1]][2], "\t")[[1]]
    expect_equal(tail(male, 3), rep("n.p.", 3))

    # Males number 10,150 at ages 0-4, females 10,200
    session$setInputs(min_events = 0, min_population = 10200)
    expect_equal(results()$summary$DSRNote,
                 c("a population under 10,200 in an age group", NA, NA))

    # Copy Code applies the same minimums, and a cleared one is off
    session$setInputs(insert_code = 1)
    expect_match(sent, "min_population = 10200", fixed = TRUE)
    expect_no_match(sent, "min_events", fixed = TRUE)
    session$setInputs(min_population = NA)
    expect_true(all(is.na(results()$summary$DSRNote)))
  })

})

test_that("counts by age set events and populations out with the ages across", {

  d <- .dsr_counts(fake_events, fake_population, c("Year", "Area"),
                   by_sex = TRUE, suppress = 22)
  ages <- .dsr_age_labels

  expect_equal(names(d), c("Year", "Area", "Sex", "Count", ages, "All ages"))
  expect_equal(d$Sex, rep(c("Male", "Female", "Persons"), each = 2))
  expect_equal(d$Count, rep(c("Events", "Population"), 3))

  # Males have 21 to 38 events, so 21 is suppressed, in each age group;
  # populations are never suppressed
  expect_equal(unname(unlist(d[1, ages])), c(NA, 22:38))
  expect_equal(unname(unlist(d[2, ages])), 10000 + 100 * (1:18) + 50)
  # Persons include sex not stated: 2 (20 + a) + 1
  expect_equal(unname(unlist(d[5, ages])), 2 * (21:38) + 1)

  # The totals are the counts of the rates
  r <- suppressMessages(dsr_calculate(fake_events, fake_population,
                                      fake_standard, by = c("Year", "Area"),
                                      by_sex = TRUE))
  expect_equal(d[["All ages"]][c(1, 3, 5)], r$Events)
  expect_equal(d[["All ages"]][c(2, 4, 6)], r$Population)

  # Limited to ages, the columns are those age groups
  ages_15_to_44 <- .dsr_counts(.dsr_truncate(fake_events, c(4L, 9L)),
                               .dsr_truncate(fake_population, c(4L, 9L)),
                               c("Year", "Area"))
  expect_equal(names(ages_15_to_44)[-(1:4)], c(ages[4:9], "All ages"))

  # Copied, ages and suppressed counts stay text in Excel
  lines <- strsplit(.dsr_counts_tsv(d, "State or territory", suppress = 22),
                    "\n", fixed = TRUE)[[1]]
  expect_length(lines, 7)
  expect_equal(strsplit(lines[1], "\t", fixed = TRUE)[[1]][c(1:5, 23)],
               c("Year", "State or territory", "Sex", "Count", "0–4",
                 "All ages"))
  expect_match(lines[2], "^2023\tACT\tMale\tEvents\t<22\t22\t23\t")

  # The table highlights populations and event totals below the minimums
  tbl <- .dsr_counts_datatable(d, "State or territory", suppress = 22,
                               min_events = 600, min_population = 10200)
  expect_equal(names(tbl$x$data), names(d))
  defs <- Filter(function(def) !is.null(def$render), tbl$x$options$columnDefs)
  renders <- vapply(defs, function(def) as.character(def$render), character(1))
  expect_match(renders[1], "!events && 10200 > 0 && data < 10200",
               fixed = TRUE)
  expect_match(renders[2], "events && 600 > 0", fixed = TRUE)
  # Each in its own colour: dsr-short-events or dsr-short-pop
  for (render in renders) {
    expect_match(render, "'<span class=\"dsr-short dsr-short-' +", fixed = TRUE)
    expect_match(render, "(events ? 'events' : 'pop')", fixed = TRUE)
  }

})

test_that("the app shows counts by age and explains the highlights", {

  fake <- fake_episerver()
  app <- episerver_dsr_app(query = fake$query)

  shiny::testServer(app, {
    session$setInputs(dataset = "ED", scope = "ACT", level = "state",
                      standard = "101", years = c(2023, 2023),
                      multiplier = "100000", by_sex = TRUE, suppress = 22,
                      min_events = 600, min_population = 10200,
                      table = "counts", ages = c(4, 9))
    session$setInputs(calculate = 1)
    r <- results()
    expect_equal(counts(), .dsr_counts(r$events, r$population, r$by,
                                       by_sex = TRUE, suppress = 22))
    expect_equal(names(counts())[-(1:4)], c(.dsr_age_labels[4:9], "All ages"))
    note <- gsub("\\s+", " ", strip_html(output$counts_note$html))
    expect_match(note,
                 paste("Highlighted: populations under 10,200 in an age group",
                       "and event totals under 600, below the minimums"),
                 fixed = TRUE)
    # Each in the colour it has in the table
    html <- output$counts_note$html
    expect_match(html, paste0("<span class=\"dsr-short dsr-short-pop\">",
                              "populations under 10,200"), fixed = TRUE)
    expect_match(html, paste0("<span class=\"dsr-short dsr-short-events\">",
                              "event totals under 600</span>"), fixed = TRUE)
    expect_match(output$counts_table, "dsr-short dsr-short-", fixed = TRUE)

    session$setInputs(min_population = 0)
    note <- gsub("\\s+", " ", strip_html(output$counts_note$html))
    expect_match(note, "Highlighted: event totals under 600, below the minimums",
                 fixed = TRUE)

    session$setInputs(min_events = 0, min_population = 0, suppress = 0)
    expect_no_match(strip_html(output$counts_note$html), "Highlighted")
  })

})

test_that("the Explore map shows the rates and saves them with the heading", {

  fake <- fake_episerver()
  app <- episerver_dsr_app(query = fake$query)
  requested <- NULL
  local_mocked_bindings(.dsr_boundaries = function(level, scope, ...) {
    requested <<- c(requested, paste(level, scope))
    square_polys(c("ACT", "NSW"))
  })

  shiny::testServer(app, {
    session$setInputs(dataset = "ED", scope = "SURROUNDS", level = "sa3",
                      standard = "101", years = c(2023, 2023),
                      multiplier = "100000", suppress = 5, by_sex = FALSE)
    session$setInputs(calculate = 1)
    session$setInputs(ex_value = "DSR", ex_year = "2023", ex_sex = "Persons")

    # On the page the heading is over both plots, so the map has no title
    p <- ex_map_plot()
    expect_s3_class(p, "ggplot")
    expect_null(p$labels$title)
    expect_match(p$labels$caption, "fewer than 5 events (suppressed)", fixed = TRUE)
    act <- results()$summary
    expect_equal(unique(p$data$Value[p$data$Area == "ACT"]), act$DSR)
    expect_true(all(is.na(p$data$Value[p$data$Area == "NSW"])))

    t <- ex_titles()
    expect_equal(t$title, "Age-standardised rate per 100,000, 2023, persons")
    expect_match(t$subtitle, paste("residents of the ACT and surrounds, by SA3",
                                   "(ASGS 2021), standardised to Australia",
                                   "(2001)"), fixed = TRUE)
    heading <- gsub("\\s+", " ", strip_html(output$ex_heading$html))
    expect_match(heading, t$title, fixed = TRUE)
    expect_match(heading, t$subtitle, fixed = TRUE)

    # Downloads are titled with the heading and named for what they show
    p <- ex_map_plot(t$title, t$subtitle)
    expect_equal(p$labels$title, t$title)
    expect_equal(gsub("\n", " ", p$labels$subtitle), t$subtitle)
    file <- output$ex_map_pdf
    expect_equal(basename(file), "dsr-map-dsr-2023-persons.pdf")
    expect_equal(readBin(file, "raw", 4), charToRaw("%PDF"))
    file <- output$ex_map_png
    expect_equal(basename(file), "dsr-map-dsr-2023-persons.png")
    expect_equal(readBin(file, "raw", 4), as.raw(c(0x89, 0x50, 0x4e, 0x47)))

    session$setInputs(ex_value = "Events")
    p <- ex_map_plot()
    expect_equal(unique(p$data$Value[p$data$Area == "ACT"]), act$Events)

    # Boundaries are fetched once for the level and population
    session$setInputs(by_sex = TRUE)
    invisible(ex_map_plot())
    expect_equal(requested, "sa3 SURROUNDS")
  })

})

test_that("the Explore tab compares areas at SA3 and SA2 but not by state", {

  # Two areas at every level: the ACT and the NSW surrounds
  query <- function(sql) {
    if (grepl("AS Events", sql, fixed = TRUE)) {
      nsw <- fake_events
      nsw$Area[!is.na(nsw$Area)] <- "NSW (surrounds)"
      return(rbind(fake_events, nsw))
    }
    if (grepl("AS Population", sql, fixed = TRUE)) {
      nsw <- fake_population
      nsw$Area <- "NSW (surrounds)"
      return(rbind(fake_population, nsw))
    }
    fake_episerver()$query(sql)
  }
  app <- episerver_dsr_app(query = query)
  requested <- NULL
  local_mocked_bindings(.dsr_boundaries = function(level, scope, ...) {
    requested <<- c(requested, paste(level, scope))
    square_polys(c("ACT", "NSW (surrounds)"))
  })

  shiny::testServer(app, {
    session$setInputs(dataset = "ED", scope = "SURROUNDS", level = "state",
                      standard = "101", years = c(2023, 2023),
                      multiplier = "100000", suppress = 5)
    expect_match(strip_html(output$ex_note$html), "Press Calculate")

    # State level: the two areas are in the tables, but neither the map nor
    # the funnel plot is drawn
    session$setInputs(calculate = 1)
    session$setInputs(ex_value = "DSR", ex_year = "2023", ex_sex = "Persons")
    expect_setequal(results()$summary$Area, c("ACT", "NSW (surrounds)"))
    expect_match(strip_html(output$ex_note$html),
                 "Choose SA3 or SA2 to compare areas within the ACT and surrounds.",
                 fixed = TRUE)
    for (id in c("ex_map", "ex_funnel", "ex_heading", "ex_method")) {
      expect_error(output[[id]], class = "shiny.silent.error", label = id)
    }
    expect_error(ex_funnel(), class = "shiny.silent.error")
    expect_null(requested)

    session$setInputs(scope = "ACT", calculate = 2)
    expect_match(strip_html(output$ex_note$html),
                 "Choose SA3 or SA2 to compare areas within the ACT.",
                 fixed = TRUE)
    expect_error(output$ex_funnel, class = "shiny.silent.error")

    # SA3: both plots compare the areas
    session$setInputs(scope = "SURROUNDS", level = "sa3", calculate = 3)
    expect_setequal(ex_funnel()$points$Area, c("ACT", "NSW (surrounds)"))
    expect_match(output$ex_funnel, "data-id='NSW (surrounds)'", fixed = TRUE)
    expect_match(output$ex_map, "data-id='ACT'", fixed = TRUE)
    expect_equal(requested, "sa3 SURROUNDS")
  })

})

test_that("the Explore tab explains a map it cannot draw", {

  fake <- fake_episerver()
  app <- episerver_dsr_app(query = fake$query)
  local_mocked_bindings(.dsr_boundaries = function(level, scope, ...) {
    stop("Could not resolve host: geo.abs.gov.au", call. = FALSE)
  })

  shiny::testServer(app, {
    session$setInputs(dataset = "ED", scope = "ACT", level = "sa3",
                      standard = "101", years = c(2023, 2023),
                      multiplier = "100000", suppress = 5)
    session$setInputs(calculate = 1)
    note <- strip_html(output$ex_note$html)
    expect_match(note, "could not be downloaded from the ABS")
    expect_match(note, "so only the funnel plot is shown", fixed = TRUE)
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
    session$setInputs(calculate = 1)
    sql <- grep("AS Events", fake$calls(), value = TRUE, fixed = TRUE)
    expect_match(sql, "(EDShortListCode), (Diagnosis1), (Diagnosis2), (Diagnosis3)",
                 fixed = TRUE)
    expect_match(strip_html(output$info_bar$html), "any diagnosis J45")
  })

})

test_that("the app spells out the diagnosis codes entered with their titles", {

  fake <- fake_episerver()
  app <- episerver_dsr_app(query = fake$query)
  note <- function(output) {
    gsub("\\s+", " ", strip_html(output$codes_note$html))
  }

  shiny::testServer(app, {
    session$setInputs(dataset = "APC", codes = c("I21", "I25-I27"))
    expect_match(note(output), "I21 Acute myocardial test condition",
                 fixed = TRUE)
    expect_match(note(output), paste(
      "I25-I27 I25 Chronic heart test condition to I27 Other pulmonary",
      "heart test condition"), fixed = TRUE)

    # An entry that is not a code is reported instead
    session$setInputs(codes = c("I21", "C13-"))
    expect_match(note(output), "is not an ICD-10 code")
    expect_no_match(note(output), "Acute myocardial")
  })

  # The titles are read once, when the app starts
  expect_equal(sum(grepl("Analysis.dbo.ICD10AM", fake$calls(), fixed = TRUE)),
               1)

})

test_that("the app searches procedures in APC and combines them with diagnoses", {

  fake <- fake_episerver()
  app <- episerver_dsr_app(query = fake$query)
  note <- function(output) {
    gsub("\\s+", " ", strip_html(output$procedures_note$html))
  }

  shiny::testServer(app, {
    session$setInputs(dataset = "APC", scope = "ACT", level = "state",
                      standard = "101", years = c(2023, 2023),
                      multiplier = "100000", codes = "I21",
                      diagnosis = "principal", procedures = c("90001-00", "1901"),
                      procedure = "all", combine = "or")
    expect_match(note(output),
                 "90001-00 Test nerve conduction study, one nerve", fixed = TRUE)
    expect_match(note(output), "[1901] Test nerve studies", fixed = TRUE)

    session$setInputs(calculate = 1)
    sql <- grep("AS Events", fake$calls(), value = TRUE, fixed = TRUE)
    expect_length(sql, 1)
    expect_match(sql, "(Procedure50, BlockProcedure50)", fixed = TRUE)
    expect_match(sql, "\n        OR EXISTS (", fixed = TRUE)
    info <- strip_html(output$info_bar$html)
    expect_match(info, paste("APC separations, principal diagnosis I21 or any",
                             "procedure 90001-00 or [1901],"), fixed = TRUE)
    expect_match(info, "cannot match the procedures", fixed = TRUE)

    session$setInputs(combine = "and")
    expect_match(strip_html(output$info_bar$html), "settings have changed")

    # A procedure that cannot be read stops the calculation and says why
    session$setInputs(procedures = "90001-001")
    expect_null(spec())
    expect_match(note(output), "is not an ACHI procedure code")

    # ED has no procedures: they are left out and the note says so
    session$setInputs(procedures = "90001-00", dataset = "ED")
    expect_equal(nrow(spec()$procedures), 0)
    expect_match(note(output), paste("ED presentations have no procedures, so",
                                     "procedure codes apply to APC separations",
                                     "only."), fixed = TRUE)
  })

})

test_that("the app takes typed codes when the titles cannot be read", {

  fake <- fake_episerver()
  failing <- function(sql) {
    if (grepl("ICD10AM", sql, fixed = TRUE)) {
      stop("Invalid object name 'Analysis.dbo.ICD10AM'.")
    }
    fake$query(sql)
  }
  app <- episerver_dsr_app(query = failing)

  # The notes behind the information buttons say that search is unavailable
  withr::local_options(sass.cache = FALSE)
  page <- app$httpHandler(list(PATH_INFO = "/", REQUEST_METHOD = "GET",
                               QUERY_STRING = ""))$content
  help <- function(id) {
    regmatches(page, regexpr(paste0("(?s)<details id=\"", id, "\".*?</details>"),
                             page, perl = TRUE))
  }
  expect_match(help("codes_help"), "Searching by condition is unavailable")
  expect_match(help("procedures_help"), "Searching by procedure is unavailable")

  shiny::testServer(app, {
    session$setInputs(dataset = "APC", scope = "ACT", level = "state",
                      standard = "101", years = c(2023, 2023),
                      multiplier = "100000", codes = "I21")
    expect_no_match(strip_html(output$codes_note$html), "Acute myocardial")
    session$setInputs(procedures = "90001-00")
    expect_no_match(strip_html(output$procedures_note$html), "Test nerve")
    session$setInputs(calculate = 1)
    expect_null(rv$error)
    expect_match(grep("AS Events", fake$calls(), value = TRUE),
                 "c.Code LIKE 'I21%'", fixed = TRUE)
  })

})

test_that("the diagnosis codes box adds typed codes and suggests codes for words", {

  withr::local_options(sass.cache = FALSE)
  app  <- episerver_dsr_app(query = fake_episerver()$query)
  page <- app$httpHandler(list(PATH_INFO = "/", REQUEST_METHOD = "GET",
                               QUERY_STRING = ""))$content
  config <- regmatches(page, regexpr(
    "<script type=\"application/json\" data-for=\"codes\"[^>]*>.*?</script>",
    page, perl = TRUE))
  expect_length(config, 1)

  # Typed codes and ranges are still created, and take precedence over the
  # suggestions; Tab adds as Enter does
  for (setting in c("\"create\":true", "\"createOnBlur\":true",
                    "\"addPrecedence\":true", "\"selectOnTab\":true",
                    "\"sortField\":\"rank\"", "\"openOnFocus\":false")) {
    expect_match(config, setting, fixed = TRUE)
  }
  # The functions and the code pattern are passed as JavaScript
  evaluated <- jsonlite::fromJSON(gsub(
    "&quot;", "\"", sub("(?s).*?data-eval=\"([^\"]*)\".*", "\\1", config,
                        perl = TRUE), fixed = TRUE))
  expect_true(all(c("createFilter", "load", "score", "render", "splitOn",
                    "onType", "onDropdownOpen") %in% evaluated))
  expect_match(page, "Shiny.addCustomMessageHandler('dsr_search_urls'",
               fixed = TRUE)
  expect_match(page, "data-for=\"procedures\"", fixed = TRUE)

})

test_that("the app starts on three years and all ages, with ASGS 2021 levels", {

  # Rendering compiles the theme; keep sass from caching it in the user's
  # cache folder
  withr::local_options(sass.cache = FALSE)
  app  <- episerver_dsr_app(query = fake_episerver()$query)
  page <- app$httpHandler(list(PATH_INFO = "/", REQUEST_METHOD = "GET",
                               QUERY_STRING = ""))$content
  slider <- function(id) {
    regmatches(page, regexpr(paste0("<input[^>]*id=\"", id, "\"[^>]*>"), page))
  }

  # The fake event data cover 2024 in full and the population ends in 2024
  expect_match(slider("years"), "data-from=\"2022\" data-to=\"2024\"",
               fixed = TRUE)
  expect_match(slider("ages"),
               "data-min=\"1\" data-max=\"18\" data-from=\"1\" data-to=\"18\"",
               fixed = TRUE)
  expect_match(page, "<option value=\"sa3\">SA3 (ASGS 2021)</option>",
               fixed = TRUE)

  # The Explore tab has the map settings, one Download menu of the four
  # files, and toolbars in the brand purples; the Population choice has no
  # Australian residents
  for (input in c("value", "year", "sex", "ages")) {
    expect_match(page, paste0("id=\"ex_", input, "\""), fixed = TRUE)
  }
  expect_match(page, "data-value=\"explore\"", fixed = TRUE)
  expect_match(page, "id=\"ex_funnel\"", fixed = TRUE)
  expect_match(page, "id=\"ex_overdispersion\"[^>]*checked")
  menu <- regmatches(page, regexpr("(?s)<div class=\"dropdown\">.*?</ul>",
                                   page, perl = TRUE))
  expect_length(menu, 1)
  expect_match(menu, "id=\"ex_download\"[^>]*data-bs-toggle=\"dropdown\"")
  links <- regmatches(menu, gregexpr(
    "<a [^>]*class=\"shiny-download-link[^\"]*dropdown-item\"[^>]*>[^<]*",
    menu))[[1]]
  expect_equal(sub(".*id=\"([a-z_]+)\".*", "\\1", links),
               c("ex_map_pdf", "ex_map_png", "ex_funnel_pdf", "ex_funnel_png"))
  expect_equal(sub(".*>", "", links),
               c("Map as PDF", "Map as PNG", "Funnel plot as PDF",
                 "Funnel plot as PNG"))
  expect_match(page, paste0("(?s)\\.ggiraph-toolbar-icon\\.neutral \\{\\s*",
                            "stroke: var\\(--epi-accent\\)"), perl = TRUE)
  expect_match(page, "data-value=\"help\"", fixed = TRUE)
  expect_match(page, "<option value=\"SURROUNDS\">ACT and surrounds</option>",
               fixed = TRUE)
  expect_no_match(page, "Australian residents", fixed = TRUE)

})

test_that("the settings are in a sidebar and the results in tabs on the top bar", {

  withr::local_options(sass.cache = FALSE)
  app  <- episerver_dsr_app(query = fake_episerver()$query)
  page <- app$httpHandler(list(PATH_INFO = "/", REQUEST_METHOD = "GET",
                               QUERY_STRING = ""))$content
  part <- function(pattern) {
    regmatches(page, regexpr(pattern, page, perl = TRUE))
  }
  has_id <- function(html, id) grepl(paste0("id=\"", id, "\""), html, fixed = TRUE)

  expect_match(page, "<title>DSR Calculator</title>", fixed = TRUE)

  # The top bar holds the name at the left, the tabs, and the logo at the
  # right. There is no Done button: closing the browser tab closes the app.
  navbar <- part("(?s)<nav .*?</nav>")
  expect_length(navbar, 1)
  expect_match(navbar, "(?s)class=\"navbar-brand\"[^>]*>\\s*DSR Calculator\\s*<",
               perl = TRUE)
  expect_match(navbar, "src=\"data:image/svg+xml;base64,", fixed = TRUE)
  in_bar <- function(x) regexpr(x, navbar, fixed = TRUE)
  for (tab in c("tables", "explore", "help")) {
    at <- in_bar(paste0("data-value=\"", tab, "\""))
    expect_gt(at, in_bar("DSR Calculator"))
    expect_lt(at, in_bar("class=\"epi-logo\""))
  }
  # The static map has gone; the Explore tab maps the areas
  expect_no_match(navbar, "data-value=\"map\"", fixed = TRUE)
  expect_false(has_id(page, "done"))

  # The sidebar holds every setting, in four sections that start open, and
  # Calculate, the code button and the bug button; the results are outside
  # it. It can be resized, with the handle moved off its scrollbar.
  sidebar <- part("(?s)<aside id=\"settings\" class=\"sidebar.*?</aside>")
  expect_length(sidebar, 1)
  expect_match(part("<aside id=\"settings\"[^>]*>"), "data-resizable")
  # Spacing, section headings and the resize handle come from the shared
  # style (test-epi_app.R)
  expect_match(page, "--epi-gap:", fixed = TRUE)
  for (id in c("dataset", "codes", "diagnosis", "procedures", "procedure",
               "combine", "scope", "level", "years",
               "pool", "ages", "standard", "multiplier", "suppress",
               "by_sex", "calculate", "insert_code", "bug_report")) {
    expect_true(has_id(sidebar, id), label = id)
  }
  sections <- c(events = "Events", population = "Population",
                time = "Time periods", rate = "Rate details")
  for (section in names(sections)) {
    expect_match(sidebar, paste0("(?s)class=\"accordion-item\" data-value=\"",
                                 section, "\".*?accordion-title\">",
                                 sections[[section]], "<"), perl = TRUE)
  }
  expect_equal(lengths(regmatches(sidebar, gregexpr("aria-expanded=\"true\"",
                                                    sidebar))), 4)
  # Ages and sex describe the population; the time periods are the years
  # alone
  at <- function(x) regexpr(x, sidebar, fixed = TRUE)
  for (id in c("scope", "level", "ages", "by_sex")) {
    expect_gt(at(paste0("id=\"", id, "\"")), at("data-value=\"population\""))
    expect_lt(at(paste0("id=\"", id, "\"")), at("data-value=\"time\""))
  }
  expect_lt(at("id=\"pool\""), at("data-value=\"rate\""))
  expect_match(sidebar, "for=\"scope\">Geographic scope</label>", fixed = TRUE)

  # Notes on a setting are behind an information button beside its label,
  # and start hidden
  help <- function(id) {
    part(paste0("(?s)<details id=\"", id, "_help\" class=\"epi-help\">.*?</details>"))
  }
  for (id in c("codes", "diagnosis", "procedures", "min_events")) {
    expect_length(help(id), 1)
    expect_true(grepl(help(id), sidebar, fixed = TRUE), label = id)
  }
  expect_match(help("codes"), "Type words from a condition", fixed = TRUE)
  expect_match(help("diagnosis"), paste0(
    "ED searches all its diagnosis fields (EDShortListCode, Diagnosis1, ",
    "Diagnosis2, Diagnosis3)"), fixed = TRUE)
  expect_match(help("diagnosis"), paste(
    "APC searches the principal diagnosis (Diagnosis1) or any diagnosis",
    "(Diagnosis1 to Diagnosis100)."), fixed = TRUE)
  expect_match(help("procedures"), "Type words from a procedure", fixed = TRUE)
  expect_match(help("min_events"), "AIHW (2011)", fixed = TRUE)
  for (id in c("info_bar", "summary_table", "ex_map", "ex_funnel", "done",
               "copy_table", "ex_value")) {
    expect_false(has_id(sidebar, id), label = id)
  }

  # The Explore tab's settings run down its own sidebar, beside the plots,
  # with the notes on selecting areas behind an information button
  explore <- part("(?s)<aside id=\"ex_settings\" class=\"sidebar.*?</aside>")
  expect_length(explore, 1)
  expect_match(page, "(?s)epi-tab-layout\"[^>]*>\\s*<div class=\"main\">.*?<aside id=\"ex_settings\"",
               perl = TRUE)
  for (id in c("ex_value", "ex_year", "ex_sex", "ex_ages", "ex_colour",
               "ex_overdispersion", "ex_zoom", "ex_reset", "ex_clear",
               "ex_download", "ex_map_pdf", "ex_funnel_png")) {
    expect_true(has_id(explore, id), label = id)
  }
  expect_false(has_id(explore, "ex_map"))
  expect_match(help("ex_zoom"), "lasso", fixed = TRUE)
  expect_true(grepl(help("ex_zoom"), explore, fixed = TRUE))

  # The Tables tab switches between the rates and the age-specific rates,
  # with one button that copies the table shown
  tables <- part("(?s)<div class=\"tab-pane[^\"]*\" data-value=\"tables\".*?id=\"footnote\"")
  expect_length(tables, 1)
  expect_true(has_id(tables, "table"))
  expect_match(tables, "data-value=\"rates\"", fixed = TRUE)
  expect_match(tables, "data-value=\"age\"", fixed = TRUE)
  expect_true(has_id(tables, "copy_table"))
  expect_match(tables, "data-value=\"counts\"", fixed = TRUE)
  expect_true(has_id(tables, "counts_table"))
  expect_false(has_id(page, "age_specific"))

  # The minimums for a DSR start at the AIHW (2011) principles' 20 and 30
  input_tag <- function(id) part(paste0("<input id=\"", id, "\"[^>]*>"))
  expect_match(input_tag("min_events"), "value=\"20\"", fixed = TRUE)
  expect_match(input_tag("min_population"), "value=\"30\"", fixed = TRUE)
  expect_true(has_id(sidebar, "min_events"))
  expect_true(has_id(sidebar, "min_population"))

  # The page explains itself once the app has stopped
  expect_match(page, "The DSR Calculator is no longer running.", fixed = TRUE)

  # The calculator opens in the web browser, with no other display offered
  expect_named(formals(episerver_dsr),
               c("background", "port", "driver", "max_attempts", "..."))

})

test_that("the app limits the rates to an age range and reuses the counts", {

  fake <- fake_episerver()
  app <- episerver_dsr_app(query = fake$query)
  n_events <- function() sum(grepl("AS Events", fake$calls(), fixed = TRUE))

  shiny::testServer(app, {
    session$setInputs(dataset = "ED", scope = "ACT", level = "state",
                      standard = "101", years = c(2023, 2023),
                      multiplier = "100000", by_sex = FALSE,
                      table = "age", ages = c(1, 18))
    session$setInputs(calculate = 1)
    all_ages <- results()$summary

    # Ages 35 to 74 leave the queries unchanged, so the counts are reused
    session$setInputs(ages = c(8, 15))
    expect_match(strip_html(output$info_bar$html), "settings have changed")
    session$setInputs(calculate = 2)
    expect_equal(n_events(), 1)

    r <- results()
    events <- .dsr_truncate(fake_events, c(8L, 15L))
    expected <- suppressMessages(dsr_calculate(
      events, .dsr_truncate(fake_population, c(8L, 15L)), fake_standard,
      by = c("Year", "Area")
    ))
    expect_equal(r$summary, expected)
    expect_false(isTRUE(all.equal(r$summary$DSR, all_ages$DSR)))
    expect_equal(unique(age_results()$AgeGroup), 8:15)

    info <- strip_html(output$info_bar$html)
    expect_match(info, "ACT residents aged 35 to 74, by state or territory",
                 fixed = TRUE)
    expect_match(info, "Rates per 100,000 population aged 35 to 74.",
                 fixed = TRUE)
    expect_no_match(info, "settings have changed")

    # Events left out are a share of the events of those ages
    ex <- attr(r$summary, "excluded")
    n  <- ex$Events[ex$Reason == "Missing AgeGroup"]
    expect_match(info, paste0("age not recorded ", .fmt_count(n),
                              sprintf(" (%.1f%%)", 100 * n / sum(events$Events))),
                 fixed = TRUE)
  })

})

test_that("the Explore tab maps age-specific rates for the ages chosen", {

  fake <- fake_episerver()
  app <- episerver_dsr_app(query = fake$query)
  local_mocked_bindings(.dsr_boundaries = function(level, scope, ...) {
    square_polys(c("ACT", "NSW"))
  })

  shiny::testServer(app, {
    session$setInputs(dataset = "ED", scope = "SURROUNDS", level = "sa3",
                      standard = "101", years = c(2023, 2023),
                      multiplier = "100000", suppress = 5, by_sex = FALSE,
                      ages = c(5, 15))
    session$setInputs(calculate = 1)
    session$setInputs(ex_value = "AgeRate", ex_year = "2023",
                      ex_sex = "Persons", ex_ages = c(5, 9))

    # Ages 20 to 44: the events of those ages over their population
    band <- suppressMessages(dsr_calculate(
      .dsr_truncate(fake_events, c(5L, 9L)),
      .dsr_truncate(fake_population, c(5L, 9L)),
      fake_standard, by = c("Year", "Area")
    ))
    p <- ex_map_plot()
    expect_equal(unique(p$data$Value[p$data$Area == "ACT"]), band$Crude)
    # The title states the ages mapped; the subtitle states no ages
    t <- ex_titles()
    expect_equal(t$title,
                 "Age-specific rate per 100,000, aged 20 to 44, 2023, persons")
    expect_match(t$subtitle, "residents of the ACT and surrounds, by SA3",
                 fixed = TRUE)
    expect_no_match(t$subtitle, "aged")
    expect_equal(ex_file("map", "png"),
                 "dsr-map-agerate-aged-20-to-44-2023-persons.png")

    tip <- strip_html(ex_tooltips()[["ACT"]])
    expect_match(tip, "SA3 ACT Area ACT, aged 20 to 44", fixed = TRUE)
    expect_match(tip, paste0(.fmt_count(band$Events), " events, population ",
                             .fmt_count(band$Population), ", rate "),
                 fixed = TRUE)

    # The ages mapped stay within the ages of the rates
    session$setInputs(ex_ages = c(1, 18))
    expect_equal(ex$ages(), c(5L, 15L))
    expect_equal(ex_titles()$title,
                 "Age-specific rate per 100,000, aged 20 to 74, 2023, persons")

    # Every measure states the ages of its values in the same places: the
    # title, the file name and the tooltip. Here they are the ages of the
    # rates, 20 to 74.
    session$setInputs(ex_value = "DSR")
    expect_equal(ex_titles()$title,
                 "Age-standardised rate per 100,000, aged 20 to 74, 2023, persons")
    expect_no_match(ex_titles()$subtitle, "aged")
    expect_equal(ex_file("funnel", "pdf"),
                 "dsr-funnel-dsr-aged-20-to-74-2023-persons.pdf")
    expect_match(strip_html(ex_tooltips()[["ACT"]]), "Area ACT, aged 20 to 74",
                 fixed = TRUE)
    session$setInputs(ex_value = "Events")
    expect_equal(ex_titles()$title, "Events, aged 20 to 74, 2023, persons")
  })

})

test_that("the app labels SA3 tables, descriptions and maps with ASGS 2021", {

  fake <- fake_episerver()
  app <- episerver_dsr_app(query = fake$query)
  copied <- NULL
  local_mocked_bindings(
    .epi_copy_text = function(session, text, message) copied <<- text,
    .dsr_boundaries = function(level, scope, ...) square_polys("ACT")
  )

  shiny::testServer(app, {
    session$setInputs(dataset = "ED", scope = "ACT", level = "sa3",
                      standard = "101", years = c(2023, 2023),
                      multiplier = "100000")
    session$setInputs(calculate = 1)
    session$setInputs(copy_table = 1)
    expect_match(copied, "^Year\tSA3 \\(ASGS 2021\\)\tSex\t")
    expect_match(strip_html(output$info_bar$html),
                 "ACT residents, by SA3 (ASGS 2021), 2023", fixed = TRUE)

    session$setInputs(ex_value = "DSR", ex_year = "2023", ex_sex = "Persons")
    expect_match(ex_titles()$subtitle, "by SA3 (ASGS 2021)", fixed = TRUE)
    expect_match(gsub("\n", " ", ex_map_plot()$labels$caption),
                 "Boundaries: ABS ASGS Edition 3 (2021), CC BY 4.0.", fixed = TRUE)
  })

})

# ── Explore tab ─────────────────────────────────────────────────────────────

# Six SA3s of different sizes; 80105 has twice the event rates of the others
explore_query <- local({
  codes <- c("80101", "80103", "80104", "80105", "80106", "80107")
  size  <- c(1, 0.05, 0.8, 0.6, 0.3, 0.9)
  rate  <- c(1, 1, 1, 2, 1, 1)
  pop <- do.call(rbind, lapply(seq_along(codes), function(i) {
    p <- fake_population
    p$Area <- codes[i]
    p$Population <- round(p$Population * size[i])
    p
  }))
  ev <- do.call(rbind, lapply(seq_along(codes), function(i) {
    e <- fake_events[!is.na(fake_events$Area), ]
    e$Area <- codes[i]
    e$Events <- round(e$Events * size[i] * rate[i] * 10)
    e
  }))
  function(sql) {
    if (grepl("AS Events", sql, fixed = TRUE)) return(ev)
    if (grepl("AS Population", sql, fixed = TRUE)) return(pop)
    fake_episerver()$query(sql)
  }
})
explore_polys <- function(level, scope, ...) {
  square_polys(c("80101", "80103", "80104", "80105", "80106", "80107"))
}

test_that("the Explore tab plots the areas in a funnel plot of the measure", {

  app <- episerver_dsr_app(query = explore_query)
  local_mocked_bindings(.dsr_boundaries = explore_polys)

  shiny::testServer(app, {
    session$setInputs(dataset = "ED", scope = "ACT", level = "sa3",
                      standard = "101", years = c(2023, 2023),
                      multiplier = "100000", suppress = 5)
    expect_match(strip_html(output$ex_note$html), "Press Calculate")
    session$setInputs(calculate = 1)
    session$setInputs(ex_value = "DSR", ex_year = "2023", ex_sex = "Persons")

    # DSRs against the population derived from their events, with the
    # average over all the areas
    r <- results()
    f <- ex_funnel()
    expect_equal(f$measure, "DSR")
    expect_setequal(f$points$Area, r$summary$Area)
    derived <- r$summary$Events * 1e5 / r$summary$DSR
    expect_equal(f$centre, sum(r$summary$Events) / sum(derived) * 1e5)
    expect_equal(as.character(f$points$Position[f$points$Area == "80105"]),
                 "Above 99.8% limit")
    method <- strip_html(output$ex_method$html)
    expect_match(method, "events / DSR")
    expect_match(method, "6 areas are plotted")
    expect_match(method, "overdispersion ratio")

    # The measure changes both plots
    session$setInputs(ex_value = "Crude")
    expect_equal(ex_funnel()$measure, "Crude")
    expect_equal(sort(ex_funnel()$points$X), sort(r$summary$Population))
    session$setInputs(ex_value = "Events")
    f <- ex_funnel()
    expect_equal(f$centre, 1)
    expect_equal(sum(f$points$Expected), sum(f$points$Events))
    session$setInputs(ex_value = "AgeRate", ex_ages = c(15, 18))
    f <- ex_funnel()
    band <- ex$band()
    expect_equal(sort(f$points$X), sort(band$Population))
    expect_match(ex$title(r), "aged 70 and over", fixed = TRUE)
    expect_equal(ex_axes(f),
                 list(x = "Population aged 70 and over",
                      y = "Age-specific rate per 100,000, aged 70 and over"))

    # The map shows each area with its tooltip, by value or by position
    session$setInputs(ex_value = "DSR", ex_colour = "position")
    html <- output$ex_map
    for (a in r$summary$Area) {
      expect_match(html, paste0("data-id='", a, "'"), fixed = TRUE)
    }
    expect_match(html, "Funnel plot: Above 99.8% limit", fixed = TRUE)
    expect_match(output$ex_funnel, "data-id='80105'", fixed = TRUE)

    # The toolbars have no PNG download, as Download saves both plots, titled
    # with the heading; the funnel plot has no zoom with rectangle either
    expect_match(html, "\"hidden\":\\[?\"saveaspng\"")
    expect_no_match(html, "zoom_rect", fixed = TRUE)
    expect_match(output$ex_funnel, "\"hidden\":[\"saveaspng\",\"zoom_rect\"]",
                 fixed = TRUE)
    file <- output$ex_funnel_pdf
    expect_equal(basename(file), "dsr-funnel-dsr-2023-persons.pdf")
    expect_equal(readBin(file, "raw", 4), charToRaw("%PDF"))
    file <- output$ex_funnel_png
    expect_equal(basename(file), "dsr-funnel-dsr-2023-persons.png")
    expect_equal(readBin(file, "raw", 4), as.raw(c(0x89, 0x50, 0x4e, 0x47)))
    t <- ex_titles()
    expect_equal(ex_funnel_plot(t$title, t$subtitle)$labels$title, t$title)
  })

})

test_that("the Explore tab selects areas in both plots and zooms to them", {

  app <- episerver_dsr_app(query = explore_query)
  local_mocked_bindings(.dsr_boundaries = explore_polys)

  shiny::testServer(app, {
    session$setInputs(dataset = "ED", scope = "ACT", level = "sa3",
                      standard = "101", years = c(2023, 2023),
                      multiplier = "100000", suppress = 5)
    session$setInputs(calculate = 1)
    session$setInputs(ex_value = "DSR", ex_year = "2023", ex_sex = "Persons")

    # A point selected in the funnel plot is the area selected in the map
    session$setInputs(ex_funnel_selected = c("80105", "80101"))
    expect_setequal(ex_selected(), c("80105", "80101"))
    table <- strip_html(output$ex_selected$html)
    expect_match(table, "80105")
    expect_match(table, "Above 99.8% limit")
    session$setInputs(ex_map_selected = "80105")
    expect_equal(ex_selected(), "80105")

    # Zoom to the selected area's square, then back
    session$setInputs(ex_zoom = 1)
    z <- ex_zoom()
    expect_true(z$xlim[1] < 152 && z$xlim[2] > 153)
    expect_true(z$ylim[1] < -35 && z$ylim[2] > -34)
    session$setInputs(ex_reset = 1)
    expect_null(ex_zoom())

    session$setInputs(ex_clear = 1)
    expect_equal(ex_selected(), character(0))
  })

})

test_that("the Explore tab allows for overdispersion unless it is unticked", {

  app <- episerver_dsr_app(query = explore_query)
  local_mocked_bindings(.dsr_boundaries = explore_polys)

  shiny::testServer(app, {
    session$setInputs(dataset = "ED", scope = "ACT", level = "sa3",
                      standard = "101", years = c(2023, 2023),
                      multiplier = "100000", suppress = 5)
    session$setInputs(calculate = 1)
    session$setInputs(ex_value = "Crude", ex_year = "2023", ex_sex = "Persons")

    # Ticked by default
    wide <- ex_funnel()
    expect_true(wide$adjusted)
    expect_gt(wide$od$phi, 1)
    expect_match(strip_html(output$ex_method$html), "allow for this")

    session$setInputs(ex_overdispersion = FALSE)
    plain <- ex_funnel()
    expect_false(plain$adjusted)
    expect_match(strip_html(output$ex_method$html),
                 "tick Allow for overdispersion")
    expect_lte(sum(wide$points$Position != "Within 95% limits"),
               sum(plain$points$Position != "Within 95% limits"))
  })

})
