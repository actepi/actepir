# tests/testthat/test-dsr_validation.R
#
# Every rate the DSR Calculator reports, checked against implementations
# written by others:
# * PHEindicatormethods, the R package of the Office for Health Improvement
#   and Disparities (OHID, UK Government): DSRs with Dobson et al. (1991)
#   intervals (calculate_dsr()) and rates (phe_rate());
# * epitools (Aragon, UC Berkeley): crude rates and DSRs (ageadjust.direct())
#   and exact Poisson intervals (pois.exact());
# * R's stats::poisson.test(): exact Poisson intervals.
# OHID takes the limits of a total count from Byar's approximation where the
# calculator uses the exact limits, so OHID's Dobson intervals are compared
# with the exact limits swapped in. The reference figures are built from the
# raw counts with base R, apart from the calculator's own matching code.
# The tests skip where the packages are not installed.

library(testthat)
library(actepir)

# Australian standard population at 30 June 2001, 0-4 to 85+
std2001 <- data.frame(
  AgeGroup    = 1:18,
  StdPopValue = c(1282357, 1351664, 1353177, 1352745, 1302412, 1407081,
                  1466615, 1492204, 1479257, 1358594, 1300777, 1008799,
                  822024, 682513, 638380, 519356, 330050, 265235)
)

# Two years, three areas and both sexes: area A has tens of thousands of
# people, B a few thousand and C a few hundred, so totals run from under 10
# to thousands, and some age groups have no events. A few events have sex
# not stated (code 9), which count towards persons only.
validation_counts <- withr::with_seed(2011, {
  grid <- expand.grid(AgeGroup = 1:18, Sex = 1:2, Area = c("A", "B", "C"),
                      Year = 2022:2023, stringsAsFactors = FALSE)
  size  <- c(A = 60000, B = 6000, C = 600)[grid$Area] / 18
  shape <- 1.3 - 0.04 * grid$AgeGroup
  population <- pmax(round(size * shape * stats::runif(nrow(grid), 0.8, 1.2)),
                     1)
  rate   <- 0.0015 * exp(0.17 * (grid$AgeGroup - 1))
  events <- stats::rpois(nrow(grid), population * rate)
  not_stated <- data.frame(AgeGroup = c(5, 12, 12), Sex = 9,
                           Area = c("A", "A", "B"), Year = 2023,
                           Events = c(3, 2, 1))
  list(
    events = rbind(cbind(grid, Events = events), not_stated),
    population = cbind(grid, Population = population)
  )
})
ev <- validation_counts$events
po <- validation_counts$population

# Counts by year, area and age group for one sex (or persons), summed with
# base R
reference_strata <- function(ev, po, sexes = NULL, label = "Persons",
                             ages = 1:18) {
  if (!is.null(sexes)) {
    ev <- ev[ev$Sex %in% sexes, ]
    po <- po[po$Sex %in% sexes, ]
  }
  ev <- ev[ev$AgeGroup %in% ages, ]
  po <- po[po$AgeGroup %in% ages, ]
  x <- stats::aggregate(Events ~ Year + Area + AgeGroup, data = ev, FUN = sum)
  n <- stats::aggregate(Population ~ Year + Area + AgeGroup, data = po,
                        FUN = sum)
  d <- merge(n, x, all.x = TRUE)
  d$Events[is.na(d$Events)] <- 0
  d$StdPopValue <- std2001$StdPopValue[match(d$AgeGroup, std2001$AgeGroup)]
  d$Sex <- label
  d
}
reference_all <- function(ev, po, ages = 1:18) {
  rbind(reference_strata(ev, po, 1, "Male", ages),
        reference_strata(ev, po, 2, "Female", ages),
        reference_strata(ev, po, NULL, "Persons", ages))
}

# OHID's DSRs, with the exact limits of each total count swapped in for
# Byar's: OHID's interval is DSR + k sqrt(V / O) (B - O) for Byar's limit B,
# so (limit - DSR) / (B - O) recovers k sqrt(V / O)
ohid_dsr <- function(strata, conf = 0.95, multiplier = 1e5) {
  out <- as.data.frame(PHEindicatormethods::calculate_dsr(
    dplyr::group_by(strata, Year, Area, Sex),
    x = Events, n = Population, stdpop = StdPopValue,
    confidence = conf, multiplier = multiplier
  ))
  byars_lower <- utils::getFromNamespace("byars_lower", "PHEindicatormethods")
  byars_upper <- utils::getFromNamespace("byars_upper", "PHEindicatormethods")
  o <- out$total_count
  exact <- t(vapply(o, function(x) {
    stats::poisson.test(x, conf.level = conf)$conf.int
  }, numeric(2)))
  per_lower <- (out$value - out$lowercl) / (o - byars_lower(o, conf))
  per_upper <- (out$uppercl - out$value) / (byars_upper(o, conf) - o)
  out$exact_lower <- pmax(out$value - per_lower * (o - exact[, 1]), 0)
  out$exact_upper <- out$value + per_upper * (exact[, 2] - o)
  out
}

# The calculator's rows matched to the reference rows by year, area and sex
matched <- function(ours, ref, time = "Year") {
  key <- function(d) paste(d[[time]], d$Area, d$Sex)
  ref[match(key(ours), key(ref)), , drop = FALSE]
}

test_that("DSRs and their intervals match OHID's PHEindicatormethods", {

  skip_if_not_installed("PHEindicatormethods")

  ours <- suppressMessages(dsr_calculate(ev, po, std2001,
                                         by = c("Year", "Area"),
                                         by_sex = TRUE))
  ref <- matched(ours, ohid_dsr(reference_all(ev, po)))

  # OHID presents no DSR from fewer than 10 events; the data include some
  shown <- !is.na(ref$value)
  expect_gt(sum(shown), 10)
  expect_gt(sum(!shown), 0)
  expect_gt(sum(ref$total_count >= 10 & ref$total_count < 20), 0)

  expect_equal(ours$Events, ref$total_count)
  expect_equal(ours$Population, ref$total_pop)
  expect_equal(ours$DSR[shown], ref$value[shown], tolerance = 1e-12)
  expect_equal(ours$DSRLower[shown], ref$exact_lower[shown],
               tolerance = 1e-10)
  expect_equal(ours$DSRUpper[shown], ref$exact_upper[shown],
               tolerance = 1e-10)

  # Byar's approximation itself differs from the exact limits by under 1%
  # of the DSR from 10 events
  expect_lt(max(abs(ours$DSRUpper[shown] - ref$uppercl[shown]) /
                  ref$value[shown]), 0.01)

})

test_that("DSRs over an age range, pooled years, and other multipliers and levels match OHID", {

  skip_if_not_installed("PHEindicatormethods")

  # Ages 35 to 74: the weights are the standard's shares within the range
  ages <- 8:15
  ours <- suppressMessages(dsr_calculate(
    ev[ev$AgeGroup %in% ages, ], po[po$AgeGroup %in% ages, ], std2001,
    by = c("Year", "Area"), by_sex = TRUE
  ))
  ref <- matched(ours, ohid_dsr(reference_all(ev, po, ages)))
  shown <- !is.na(ref$value)
  expect_equal(ours$DSR[shown], ref$value[shown], tolerance = 1e-12)
  expect_equal(ours$DSRUpper[shown], ref$exact_upper[shown],
               tolerance = 1e-10)

  # 2022 and 2023 pooled: events and populations summed over the years
  pool <- function(d) transform(d, Period = "2022-2023")
  ours <- suppressMessages(dsr_calculate(pool(ev), pool(po), std2001,
                                         by = c("Period", "Area"),
                                         by_sex = TRUE))
  both <- reference_all(transform(ev, Year = "2022-2023"),
                        transform(po, Year = "2022-2023"))
  ref <- ohid_dsr(both)
  ref$Period <- ref$Year
  ref <- matched(ours, ref, time = "Period")
  shown <- !is.na(ref$value)
  expect_equal(ours$Events, ref$total_count)
  expect_equal(ours$DSR[shown], ref$value[shown], tolerance = 1e-12)
  expect_equal(ours$DSRLower[shown], ref$exact_lower[shown],
               tolerance = 1e-10)

  # Per 1,000 with 90% intervals
  ours <- suppressMessages(dsr_calculate(ev, po, std2001,
                                         by = c("Year", "Area"),
                                         by_sex = TRUE, multiplier = 1000,
                                         conf_level = 0.9))
  ref <- matched(ours, ohid_dsr(reference_all(ev, po), conf = 0.9,
                                multiplier = 1000))
  shown <- !is.na(ref$value)
  expect_equal(ours$DSR[shown], ref$value[shown], tolerance = 1e-12)
  expect_equal(ours$DSRLower[shown], ref$exact_lower[shown],
               tolerance = 1e-10)
  expect_equal(ours$DSRUpper[shown], ref$exact_upper[shown],
               tolerance = 1e-10)

})

test_that("crude rates and DSRs match epitools, small counts included", {

  skip_if_not_installed("epitools")

  ours <- suppressMessages(dsr_calculate(ev, po, std2001,
                                         by = c("Year", "Area"),
                                         by_sex = TRUE))
  strata <- reference_all(ev, po)
  groups <- split(strata, paste(strata$Year, strata$Area, strata$Sex))
  ref <- do.call(rbind, lapply(groups, function(g) {
    a <- epitools::ageadjust.direct(count = g$Events, pop = g$Population,
                                    stdpop = g$StdPopValue)
    data.frame(Year = g$Year[1], Area = g$Area[1], Sex = g$Sex[1],
               crude = a[["crude.rate"]] * 1e5, dsr = a[["adj.rate"]] * 1e5)
  }))
  ref <- matched(ours, ref)

  expect_equal(ours$Crude, ref$crude, tolerance = 1e-12)
  expect_equal(ours$DSR, ref$dsr, tolerance = 1e-12)
  expect_lt(min(ours$Events), 10)

})

test_that("crude and age-specific rates have the exact Poisson limits R gives", {

  exact <- function(x, n, conf = 0.95) {
    t(vapply(seq_along(x), function(i) {
      stats::poisson.test(x[i], T = n[i], conf.level = conf)$conf.int
    }, numeric(2))) * 1e5
  }

  ours <- suppressMessages(dsr_calculate(ev, po, std2001,
                                         by = c("Year", "Area"),
                                         by_sex = TRUE, conf_level = 0.9))
  ci <- exact(ours$Events, ours$Population, conf = 0.9)
  expect_equal(ours$Crude, ours$Events / ours$Population * 1e5,
               tolerance = 1e-12)
  expect_equal(ours$CrudeLower, ci[, 1], tolerance = 1e-10)
  expect_equal(ours$CrudeUpper, ci[, 2], tolerance = 1e-10)

  # Every age group, zero events included
  age <- suppressMessages(dsr_age_specific(ev, po, by = c("Year", "Area"),
                                           by_sex = TRUE))
  expect_gt(sum(age$Events == 0), 0)
  ref <- reference_all(ev, po)
  key <- function(d) paste(d$Year, d$Area, d$Sex, d$AgeGroup)
  ref <- ref[match(key(age), key(ref)), ]
  expect_equal(age$Events, ref$Events)
  expect_equal(age$Population, ref$Population)
  expect_equal(age$Rate, ref$Events / ref$Population * 1e5, tolerance = 1e-12)
  ci <- exact(age$Events, age$Population)
  expect_equal(age$RateLower, ci[, 1], tolerance = 1e-10)
  expect_equal(age$RateUpper, ci[, 2], tolerance = 1e-10)

})

test_that("crude and age-specific intervals match epitools' exact Poisson limits", {

  skip_if_not_installed("epitools")

  # epitools finds each limit by searching with uniroot(), which stops within
  # its default tolerance, about 1e-4 of an event
  expect_near_epitools <- function(rate, limit, n) {
    expect_lt(max(abs(rate / 1e5 - limit) * n), .Machine$double.eps^0.25)
  }

  ours <- suppressMessages(dsr_calculate(ev, po, std2001,
                                         by = c("Year", "Area"),
                                         by_sex = TRUE))
  epi <- epitools::pois.exact(ours$Events, pt = ours$Population)
  expect_near_epitools(ours$CrudeLower, epi$lower, ours$Population)
  expect_near_epitools(ours$CrudeUpper, epi$upper, ours$Population)

  age <- suppressMessages(dsr_age_specific(ev, po, by = c("Year", "Area"),
                                           by_sex = TRUE))
  epi <- epitools::pois.exact(age$Events, pt = age$Population)
  expect_near_epitools(age$RateLower, epi$lower, age$Population)
  expect_near_epitools(age$RateUpper, epi$upper, age$Population)

})

test_that("age-specific rates match OHID's phe_rate, and its exact intervals below 10 events", {

  skip_if_not_installed("PHEindicatormethods")

  age <- suppressMessages(dsr_age_specific(ev, po, by = c("Year", "Area"),
                                           by_sex = TRUE))
  ref <- as.data.frame(PHEindicatormethods::phe_rate(
    dplyr::group_by(age, Year, Area, Sex, AgeGroup),
    x = Events, n = Population
  ))
  key <- function(d) paste(d$Year, d$Area, d$Sex, d$AgeGroup)
  ref <- ref[match(key(age), key(ref)), ]

  expect_equal(age$Rate, ref$value, tolerance = 1e-12)
  small <- ref$method == "Exact"
  expect_gt(sum(small), 0)
  expect_equal(age$RateLower[small], ref$lowercl[small], tolerance = 1e-10)
  expect_equal(age$RateUpper[small], ref$uppercl[small], tolerance = 1e-10)

})

test_that("the calculator's rates, from query to table, match OHID's", {

  skip_if_not_installed("PHEindicatormethods")

  # The calculator reads these counts and the standard from a stand-in for
  # EpiServer
  names <- c(paste0(sprintf("%02d", seq(0, 80, 5)), "-",
                    sprintf("%02d", seq(4, 84, 5))), "85+")
  query <- function(sql) {
    if (grepl("GROUP BY StdPopCode", sql, fixed = TRUE)) {
      return(data.frame(StdPopCode = 101L, StdPopName = "Australia (2001)",
                        Note = "ABS Census"))
    }
    if (grepl("FROM Analysis.dbo.StandardPops", sql, fixed = TRUE)) {
      return(data.frame(AgeGroup = 1:18, AgeGroupName = names,
                        StdPopValue = std2001$StdPopValue))
    }
    if (grepl("MIN(PresentationDateTime)", sql, fixed = TRUE)) {
      return(data.frame(FirstDate = as.POSIXct("2004-07-01", tz = "UTC"),
                        LastDate  = as.POSIXct("2025-06-30", tz = "UTC")))
    }
    if (grepl("MIN(ERPYear)", sql, fixed = TRUE)) {
      return(data.frame(FirstYear = 2001L, LastYear = 2024L))
    }
    if (grepl("AS Events", sql, fixed = TRUE)) {
      return(ev[c("Year", "Sex", "AgeGroup", "Area", "Events")])
    }
    if (grepl("AS Population", sql, fixed = TRUE)) {
      return(transform(po, AgeGroupName = names[AgeGroup])[
        c("Year", "Sex", "AgeGroup", "AgeGroupName", "Area", "Population")])
    }
    stop("Unexpected SQL: ", sql)
  }
  app <- episerver_dsr_app(query = query)

  shiny::testServer(app, {
    session$setInputs(dataset = "ED", scope = "ACT", level = "sa3",
                      standard = "101", years = c(2022, 2023), pool = "1",
                      ages = c(1, 18), multiplier = "100000", suppress = 0,
                      min_events = 0, min_population = 0, by_sex = TRUE)
    session$setInputs(calculate = 1)
    ours <- results()$summary
    ref  <- matched(ours, ohid_dsr(reference_all(ev, po)))
    shown <- !is.na(ref$value)
    expect_equal(nrow(ours), 2 * 3 * 3)
    expect_equal(ours$DSR[shown], ref$value[shown], tolerance = 1e-12)
    expect_equal(ours$DSRLower[shown], ref$exact_lower[shown],
                 tolerance = 1e-10)
    expect_equal(ours$DSRUpper[shown], ref$exact_upper[shown],
                 tolerance = 1e-10)
  })

})
