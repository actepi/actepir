# tests/testthat/test-episerver_dsr_live.R
#
# Live tests for the DSR Calculator. Each needs EpiServer and is skipped when it
# cannot be reached. They run every query the calculator builds and check the
# assumptions the SQL makes about the tables.

library(testthat)
library(actepir)

# One connection per test, closed when the test ends
live_db <- function(env = parent.frame()) {
  db <- .epi_connection()
  withr::defer(db$disconnect(), envir = env)
  db
}

test_that("standard populations have 5-year age groups and published totals", {

  skip_if_no_episerver()
  db <- live_db()

  standards <- db$query(.dsr_sql_standards())
  expect_true(101 %in% standards$StdPopCode)

  std <- db$query(.dsr_sql_standard(.dsr_spec(years = 2023, standard = 101)))
  expect_equal(std$AgeGroup, 1:18)
  expect_equal(std$AgeGroupName[c(1, 18)], c("00-04", "85+"))

  # Australian 2001 standard population
  expect_equal(sum(std$StdPopValue), 19413240)

})

test_that("every events and population query runs and matches on area", {

  skip_if_no_episerver()
  db <- live_db()

  grid <- expand.grid(dataset = c("ED", "APC"), scope = c("ACT", "AUS"),
                      level = c("state", "sa3", "sa2"), stringsAsFactors = FALSE)

  for (i in seq_len(nrow(grid))) {
    s <- .dsr_spec(grid$dataset[i], grid$scope[i], grid$level[i], 2023)
    label <- paste(unlist(s[c("dataset", "scope", "level")]), collapse = "/")

    events     <- db$query(.dsr_sql_events(s))
    population <- db$query(.dsr_sql_population(s))

    expect_named(events, c("Year", "Sex", "AgeGroup", "Area", "Events"),
                 label = label)
    expect_named(population, c("Year", "Sex", "AgeGroup", "AgeGroupName",
                               "Area", "Population"), label = label)
    expect_true(all(events$Year == 2023), label = label)
    expect_true(all(events$AgeGroup %in% c(1:18, NA)), label = label)
    expect_setequal(unique(population$Sex), 1:2)
    expect_setequal(unique(population$AgeGroup), 1:18)

    # Area codes from the geo_ columns must meet the population's codes
    recorded <- events[!is.na(events$Area), ]
    matched  <- sum(recorded$Events[recorded$Area %in% population$Area]) /
      sum(recorded$Events)
    expect_gt(matched, 0.9, label = label)
  }

})

test_that("geo_STATE uses the state abbreviations the population SQL maps to", {

  skip_if_no_episerver()
  db <- live_db()

  for (table in c("ED", "APC")) {
    states <- db$query(paste0("SELECT DISTINCT geo_STATE FROM Analysis.dbo.",
                              table, " WHERE geo_STATE IS NOT NULL"))
    expect_true(all(states$geo_STATE %in% .dsr_states), label = table)
  }

})

test_that("EntityCode 0 in ERP5_STE_AUS is the Australian total", {

  skip_if_no_episerver()
  db <- live_db()

  # The state-level population SQL keeps EntityCode 1 to 9 on this basis
  totals <- db$query("SELECT EntityCode, SUM(ERPCount) AS Population
                      FROM Analysis.dbo.ERP5_STE_AUS
                      WHERE ERPYear = 2023
                      GROUP BY EntityCode")
  australia <- totals$Population[totals$EntityCode == 0]
  states    <- sum(totals$Population[totals$EntityCode %in% 1:9])

  # Other Territories are in the total but have no row of their own
  expect_equal(states / australia, 1, tolerance = 0.005)

})

test_that("event date ranges and population years give a year range", {

  skip_if_no_episerver()
  db <- live_db()

  for (dataset in c("ED", "APC")) {
    dates <- db$query(.dsr_sql_dates(dataset))
    for (level in c("state", "sa3", "sa2")) {
      s   <- .dsr_spec(dataset, "ACT", level, 2023)
      erp <- db$query(.dsr_sql_erp_years(s))
      r   <- .dsr_year_range(dates$FirstDate, dates$LastDate,
                             c(erp$FirstYear, erp$LastYear))
      expect_false(is.null(r), label = paste(dataset, level))
      expect_true(r$min <= r$default && r$default <= r$max,
                  label = paste(dataset, level))
    }
  }

})

test_that("ACT rates for 2023 are complete and plausible", {

  skip_if_no_episerver()
  db <- live_db()

  for (dataset in c("ED", "APC")) {
    s <- .dsr_spec(dataset, "ACT", "state", 2023)
    r <- suppressMessages(dsr_calculate(
      db$query(.dsr_sql_events(s)),
      db$query(.dsr_sql_population(s)),
      db$query(.dsr_sql_standard(s)),
      by = c("Year", "Area"), by_sex = TRUE
    ))

    expect_equal(r$Sex, c("Male", "Female", "Persons"), label = dataset)
    expect_true(all(is.finite(r$DSR)), label = dataset)
    expect_equal(r$Population[3], sum(r$Population[1:2]), label = dataset)
    expect_gte(r$Events[3], sum(r$Events[1:2]))

    # The ACT had about 470,000 residents in 2023, and ED presentations and
    # APC separations each run at several tens of thousands per 100,000
    expect_gt(r$Population[3], 400000)
    expect_lt(r$Population[3], 600000)
    expect_gt(r$Crude[3], 5000)
    expect_lt(r$Crude[3], 100000)
  }

})

test_that("diagnosis codes select the events the R reading of the codes does", {

  skip_if_no_episerver()
  db <- live_db()

  # Distinct diagnoses of 2023 events with their counts, for checking the
  # calculator's condition against dsr_code_matches() (helper-dsr.R)
  diagnoses <- function(dataset, fields) {
    ds <- .dsr_datasets[[dataset]]
    db$query(.dsr_fill("SELECT {fields}, COUNT(*) AS n
FROM Analysis.dbo.{table}
WHERE {date} >= '20230101'
  AND {date} < '20240101'
  AND {icd10}
GROUP BY {fields}",
      fields = paste(fields, collapse = ", "), table = ds$table,
      date = ds$date, icd10 = ds$icd10))
  }
  counted <- function(dataset, codes, diagnosis) {
    s <- .dsr_spec(dataset, "AUS", "state", 2023, codes = codes,
                   diagnosis = diagnosis)
    sum(db$query(.dsr_sql_events(s))$Events)
  }

  checks <- list(
    ED  = c("J45", "S00-S09.9", "T78.3 to T78.4"),
    APC = c("C13-C15.45", "E10-E14", "I21")
  )
  for (dataset in names(checks)) {
    codes <- .dsr_parse_codes(checks[[dataset]])
    principal <- counted(dataset, checks[[dataset]], "principal")
    any_diag  <- counted(dataset, checks[[dataset]], "all")

    d <- diagnoses(dataset, "Diagnosis1")
    expect_equal(principal, sum(d$n[dsr_code_matches(d$Diagnosis1, codes)]),
                 label = paste(dataset, "principal"))
    expect_gt(principal, 0)
    expect_gte(any_diag, principal)
    expect_lt(any_diag, sum(db$query(.dsr_sql_events(
      .dsr_spec(dataset, "AUS", "state", 2023)))$Events))
  }

  # Every ED diagnosis field, checked the same way
  d <- diagnoses("ED", paste0("Diagnosis", 1:3))
  hit <- Reduce(`|`, lapply(d[paste0("Diagnosis", 1:3)], dsr_code_matches,
                            codes = .dsr_parse_codes(checks$ED)))
  expect_equal(counted("ED", checks$ED, "all"), sum(d$n[hit]))

})

test_that("ABS boundaries cover every area in the population tables", {

  skip_if_no_episerver()
  skip_if_no_abs()
  db <- live_db()
  cache <- withr::local_tempdir()

  for (level in c("sa3", "sa2")) {
    s <- .dsr_spec("ED", "ACT", level, 2023)
    areas <- unique(db$query(.dsr_sql_population(s))$Area)
    missing <- setdiff(areas, .dsr_boundaries(level, "ACT", cache = cache)$Area)
    expect_equal(missing, character(0), label = level)
  }

})

test_that("the app calculates against EpiServer", {

  skip_if_no_episerver()

  app <- episerver_dsr_app()

  shiny::testServer(app, {
    session$setInputs(dataset = "ED", scope = "ACT", level = "sa3",
                      standard = "101", years = c(2023, 2023),
                      multiplier = "100000", by_sex = TRUE,
                      age_specific = TRUE)
    session$setInputs(calculate = 1)

    expect_null(rv$error)
    r <- results()
    expect_null(r$error)
    expect_gt(nrow(r$summary), 0)
    expect_equal(nrow(age_results()), nrow(r$summary) * 18)

    # Asthma in any diagnosis, with the usual suppression
    total <- sum(r$summary$Events)
    session$setInputs(codes = "J45", diagnosis = "all", suppress = 5)
    session$setInputs(calculate = 2)
    expect_null(rv$error)
    r <- results()
    expect_null(r$error)
    expect_lt(sum(r$summary$Events, na.rm = TRUE), total)
    expect_true(all(is.na(r$summary$Events) == r$summary$Suppressed))
  })

})
