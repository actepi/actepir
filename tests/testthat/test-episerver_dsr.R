# tests/testthat/test-episerver_dsr.R
#
# Offline tests for the DSR Calculator: settings, SQL, year ranges, the code it
# copies and the app's server logic. The app runs against a fake query function
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

all_specs <- function(years = 2023) {
  grid <- expand.grid(dataset = c("ED", "APC"), scope = c("ACT", "AUS"),
                      level = c("state", "sa3", "sa2"), stringsAsFactors = FALSE)
  lapply(seq_len(nrow(grid)), function(i) {
    .dsr_spec(grid$dataset[i], grid$scope[i], grid$level[i], years)
  })
}

# ── Settings ────────────────────────────────────────────────────────────────

test_that(".dsr_spec validates and normalises the settings", {

  s <- .dsr_spec("APC", "AUS", "sa2", 2023, "300")
  expect_equal(s, list(dataset = "APC", scope = "AUS", level = "sa2",
                       years = c(2023L, 2023L), standard = 300L))

  expect_equal(.dsr_spec(years = c(2019, 2023))$years, c(2019L, 2023L))

  expect_error(.dsr_spec("XX", years = 2023))
  expect_error(.dsr_spec(scope = "NSW", years = 2023))
  expect_error(.dsr_spec(level = "sa1", years = 2023))
  expect_error(.dsr_spec(years = c(2023, 2019)), "earliest first")
  expect_error(.dsr_spec(years = "2023; DROP TABLE x"), "calendar year")
  expect_error(.dsr_spec(years = 2023, standard = "abc"), "StdPopCode")

})

test_that(".dsr_pop_table names the ERP5 table for the settings", {

  expect_equal(.dsr_pop_table(.dsr_spec("ED", "ACT", "state", 2023)), "ERP5_STE_ACT")
  expect_equal(.dsr_pop_table(.dsr_spec("ED", "AUS", "sa3", 2023)), "ERP5_SA3_AUS")
  expect_equal(.dsr_pop_table(.dsr_spec("APC", "ACT", "sa2", 2023)), "ERP5_SA2_ACT")

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

test_that("standard SQL selects the 5-year age groups of one standard", {

  sql <- .dsr_sql_standard(.dsr_spec(years = 2023, standard = 300))
  expect_match(sql, "WHERE StdPopCode = 300", fixed = TRUE)
  expect_match(sql, "AgeGroupTypeName = 'AgeGroup05Code'", fixed = TRUE)

})

test_that("every SQL statement is complete and safe to embed in R code", {

  sqls <- c(
    unlist(lapply(all_specs(), function(s) {
      c(.dsr_sql_events(s), .dsr_sql_population(s), .dsr_sql_standard(s),
        .dsr_sql_erp_years(s))
    })),
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

  for (s in all_specs(c(2019, 2023))) {
    code <- .dsr_code(s, by_sex = TRUE, age_specific = TRUE)
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

})

test_that("running the copied code reproduces dsr_calculate()", {

  fake <- fake_episerver()
  s <- .dsr_spec("ED", "ACT", "state", 2023)
  code <- .dsr_code(s, by_sex = TRUE, age_specific = TRUE)

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
    by = c("Year", "Area"), by_sex = TRUE
  ))
  expect_equal(env$rates, expected)
  expect_equal(nrow(env$age_rates), 18 * 3)

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
                      age_specific = TRUE)
    session$setInputs(insert_code = 1)
  })

  expect_equal(sent, .dsr_code(
    .dsr_spec("APC", "AUS", "sa3", c(2019, 2022), 300),
    by_sex = TRUE, age_specific = TRUE, multiplier = 10000,
    standard_name = "European (1976) (superseded)"
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
