# tests/testthat/test-dsr_calculate.R
#
# Offline tests for the rate calculations behind episerver_dsr(). Expected
# values are worked by hand or come from an independent implementation
# (stats::poisson.test() for exact Poisson limits).

library(testthat)
library(actepir)

# Two age groups weighted 3:1, small enough to work by hand
ev2 <- data.frame(AgeGroup = 1:2, Events = c(10, 20))
po2 <- data.frame(AgeGroup = 1:2, Population = c(1000, 4000))
st2 <- data.frame(AgeGroup = 1:2, StdPopValue = c(3, 1))

# Males, females and two events with sex not stated (METeOR code 9)
ev_sex <- data.frame(
  Sex      = c(1, 1, 2, 2, 9),
  AgeGroup = c(1, 2, 1, 2, 2),
  Events   = c(4, 6, 3, 5, 2)
)
po_sex <- data.frame(
  Sex        = c(1, 1, 2, 2),
  AgeGroup   = c(1, 2, 1, 2),
  Population = c(100, 200, 120, 180)
)
st_sex <- data.frame(AgeGroup = 1:2, StdPopValue = c(1, 1))

# ── dsr_calculate() ─────────────────────────────────────────────────────────

test_that("dsr_calculate matches a worked example", {

  r <- dsr_calculate(ev2, po2, st2)

  expect_equal(nrow(r), 1)
  expect_equal(r$Sex, "Persons")
  expect_equal(r$Events, 30)
  expect_equal(r$Population, 5000)
  expect_equal(r$Crude, 600)

  # 0.75 * 10 / 1000 + 0.25 * 20 / 4000 = 0.00875
  expect_equal(r$DSR, 875)

  # Dobson: 0.00875 + sqrt(v / 30) * (limit - 30) per 100,000, with
  # v = 0.75^2 * 10 / 1000^2 + 0.25^2 * 20 / 4000^2 and exact limits for 30
  expect_equal(r$DSRLower, 449.4929675779, tolerance = 1e-9)
  expect_equal(r$DSRUpper, 1434.2633440128, tolerance = 1e-9)

})

test_that("crude limits are exact Poisson limits", {

  r  <- dsr_calculate(ev2, po2, st2)
  pt <- stats::poisson.test(30, 5000)$conf.int * 1e5

  expect_equal(c(r$CrudeLower, r$CrudeUpper), as.numeric(pt))

})

test_that("a standard proportional to the population gives the crude rate", {

  # Each event then carries the same weight, so the Dobson interval reduces to
  # the exact interval of the crude rate.
  st <- data.frame(AgeGroup = 1:2, StdPopValue = c(1000, 4000))
  r  <- dsr_calculate(ev2, po2, st)

  expect_equal(r$DSR, r$Crude)
  expect_equal(r$DSRLower, r$CrudeLower)
  expect_equal(r$DSRUpper, r$CrudeUpper)

})

test_that("multiplier and conf_level are applied", {

  r <- dsr_calculate(ev2, po2, st2, multiplier = 1000)
  expect_equal(r$DSR, 8.75)
  expect_equal(r$Crude, 6)

  r90 <- dsr_calculate(ev2, po2, st2, conf_level = 0.9)
  r95 <- dsr_calculate(ev2, po2, st2)
  expect_gt(r90$DSRLower, r95$DSRLower)
  expect_lt(r90$DSRUpper, r95$DSRUpper)

})

test_that("by_sex returns males, females and persons", {

  r <- dsr_calculate(ev_sex, po_sex, st_sex, by_sex = TRUE)

  expect_equal(r$Sex, c("Male", "Female", "Persons"))
  expect_equal(r$Events, c(10, 8, 20))
  expect_equal(r$Population, c(300, 300, 600))
  expect_equal(r$DSR[1], (0.5 * 4 / 100 + 0.5 * 6 / 200) * 1e5)

  # Persons count every sex code, including sex not stated
  expect_equal(r$DSR[3], (0.5 * 7 / 220 + 0.5 * 13 / 380) * 1e5)

})

test_that("persons only by default, ignoring sex", {

  r <- dsr_calculate(ev_sex, po_sex, st_sex)

  expect_equal(r$Sex, "Persons")
  expect_equal(r$Events, 20)
  expect_equal(r$Population, 600)

})

test_that("population sex must use the METeOR codes when by_sex", {

  bad <- po_sex
  bad$Sex <- ifelse(bad$Sex == 1, "M", "F")

  expect_error(dsr_calculate(ev_sex, bad, st_sex, by_sex = TRUE),
               regexp = "codes 1")

})

test_that("groups are matched on the by columns and returned in order", {

  ev <- data.frame(Year = rep(c(2023, 2022), each = 2), AgeGroup = rep(1:2, 2),
                   Events = c(10, 20, 5, 5))
  po <- data.frame(Year = rep(c(2022, 2023), each = 2), AgeGroup = rep(1:2, 2),
                   Population = c(1000, 1000, 1000, 4000))

  r <- dsr_calculate(ev, po, st2, by = "Year")

  expect_equal(r$Year, c(2022, 2023))
  expect_equal(r$Events, c(10, 30))
  expect_equal(r$DSR, c((0.75 * 5 / 1000 + 0.25 * 5 / 1000) * 1e5, 875))

})

test_that("a matched column left out of by is pooled", {

  ev <- data.frame(Year = rep(c(2022, 2023), each = 2), AgeGroup = rep(1:2, 2),
                   Events = c(5, 5, 10, 20))
  po <- data.frame(Year = rep(c(2022, 2023), each = 2), AgeGroup = rep(1:2, 2),
                   Population = c(1000, 1000, 1000, 4000))

  r <- dsr_calculate(ev, po, st2)

  # Matched year by year, then summed into person-years
  expect_equal(r$Events, 40)
  expect_equal(r$Population, 7000)
  expect_equal(r$DSR, (0.75 * 15 / 2000 + 0.25 * 25 / 5000) * 1e5)

})

test_that("matching columns of different types still match", {

  # 100000 prints as 1e+05 as a double but not as an integer
  ev <- data.frame(Area = 100000, AgeGroup = 1:2, Events = c(10, 20))
  po <- data.frame(Area = 100000L, AgeGroup = 1:2, Population = c(1000L, 4000L))
  expect_equal(dsr_calculate(ev, po, st2, by = "Area")$DSR, 875)

  ev$Area <- factor("A")
  po$Area <- "A"
  expect_equal(dsr_calculate(ev, po, st2, by = "Area")$DSR, 875)

})

test_that("unusable events are excluded and reported", {

  ev <- data.frame(
    Area     = c("A", "A", NA, "Z", "A"),
    AgeGroup = c(1, 2, 1, 1, NA),
    Events   = c(10, 20, 7, 3, 2)
  )
  po <- data.frame(Area = "A", AgeGroup = 1:2, Population = c(1000, 4000))

  expect_message(r <- dsr_calculate(ev, po, st2, by = "Area"),
                 regexp = "excluded from the rates")

  expect_equal(r$Events, 30)
  expect_equal(r$DSR, 875)

  ex <- attr(r, "excluded")
  expect_equal(ex$Reason,
               c("Missing AgeGroup", "Missing Area", "No matching population"))
  expect_equal(ex$Events, c(2, 7, 3))

})

test_that("nothing is reported when every event is used", {

  expect_message(r <- dsr_calculate(ev2, po2, st2), regexp = NA)
  expect_equal(nrow(attr(r, "excluded")), 0)

})

test_that("a group with no events has a zero DSR and no DSR interval", {

  ev <- data.frame(Area = "A", AgeGroup = 1:2, Events = c(10, 20))
  po <- data.frame(Area = rep(c("A", "B"), each = 2), AgeGroup = rep(1:2, 2),
                   Population = c(1000, 4000, 500, 500))

  r <- dsr_calculate(ev, po, st2, by = "Area")
  b <- r[r$Area == "B", ]

  expect_equal(b$Events, 0)
  expect_equal(b$DSR, 0)
  expect_true(is.na(b$DSRLower))
  expect_true(is.na(b$DSRUpper))

  # The crude interval is still defined: 0 to the exact upper limit for 0
  expect_equal(b$CrudeLower, 0)
  expect_equal(b$CrudeUpper, stats::qchisq(0.975, 2) / 2 / 1000 * 1e5)

})

test_that("events in an age group with no population make the DSR NA", {

  po <- data.frame(AgeGroup = 1:2, Population = c(1000, 0))

  expect_message(r <- dsr_calculate(ev2, po, st2), regexp = "DSR is NA")
  expect_true(is.na(r$DSR))
  expect_equal(r$Crude, 3000)

  # With no events either, the empty age group contributes nothing
  ev <- data.frame(AgeGroup = 1:2, Events = c(10, 0))
  expect_equal(dsr_calculate(ev, po, st2)$DSR, 0.75 * 10 / 1000 * 1e5)

})

test_that("weights are taken over the age groups in the population", {

  # Standardising within age groups 2 and 3 only
  st <- data.frame(AgeGroup = 1:3, StdPopValue = c(2, 1, 3))
  ev <- data.frame(AgeGroup = 2:3, Events = c(10, 30))
  po <- data.frame(AgeGroup = 2:3, Population = c(1000, 1000))

  expect_equal(dsr_calculate(ev, po, st)$DSR, (0.25 * 0.01 + 0.75 * 0.03) * 1e5)

})

test_that("inputs are validated", {

  expect_error(dsr_calculate(list(), po2, st2), "must be a data frame")
  expect_error(dsr_calculate(ev2["AgeGroup"], po2, st2), "no column Events")
  expect_error(dsr_calculate(ev2, po2, st2, by = "Year"), "no column Year")
  expect_error(dsr_calculate(ev2, po2, st2, by_sex = TRUE), "no column Sex")
  expect_error(dsr_calculate(ev2, po2, st2, by = "Sex"), "cannot include Sex")
  expect_error(dsr_calculate(ev2, po2, st2, by_sex = NA), "TRUE or FALSE")
  expect_error(dsr_calculate(transform(ev2, Events = -1), po2, st2),
               "non-negative")
  expect_error(dsr_calculate(ev2, po2, st2, multiplier = 0), "positive")
  expect_error(dsr_calculate(ev2, po2, st2, conf_level = 95), "between 0 and 1")
  expect_error(dsr_calculate(ev2, po2[0, ], st2), "no rows")
  expect_error(dsr_calculate(ev2, po2, rbind(st2, st2)), "one row per AgeGroup")
  expect_error(dsr_calculate(ev2, po2, st2[1, ]), "no row for AgeGroup 2")

})

# ── dsr_age_specific() ──────────────────────────────────────────────────────

test_that("age-specific limits are exact Poisson limits", {

  a <- dsr_age_specific(ev2, po2)

  expect_equal(a$Rate, c(1000, 500))
  for (i in 1:2) {
    pt <- stats::poisson.test(a$Events[i], a$Population[i])$conf.int * 1e5
    expect_equal(c(a$RateLower[i], a$RateUpper[i]), as.numeric(pt))
  }

})

test_that("dsr_age_specific orders by group, sex and age group", {

  a <- dsr_age_specific(ev_sex, po_sex, by_sex = TRUE)

  expect_equal(a$Sex, rep(c("Male", "Female", "Persons"), each = 2))
  expect_equal(a$AgeGroup, rep(1:2, 3))
  expect_equal(a$Events, c(4, 6, 3, 5, 7, 13))

})

test_that("dsr_age_specific labels age groups and handles zero population", {

  po <- data.frame(AgeGroup = 1:2, AgeGroupName = c("00-04", "05-09"),
                   Population = c(1000, 0))
  ev <- data.frame(AgeGroup = 1:2, Events = c(10, 0))

  a <- dsr_age_specific(ev, po)

  expect_equal(a$AgeGroupName, c("00-04", "05-09"))
  expect_equal(a$Rate[1], 1000)
  expect_true(is.na(a$Rate[2]))

})

test_that("the DSR is the weighted sum of the age-specific rates", {

  a <- dsr_age_specific(ev_sex, po_sex, by_sex = TRUE)
  r <- dsr_calculate(ev_sex, po_sex, st_sex, by_sex = TRUE)

  by_sex <- split(a$Rate, factor(a$Sex, levels = c("Male", "Female", "Persons")))
  expect_equal(r$DSR, unname(vapply(by_sex, function(x) sum(0.5 * x), numeric(1))))

})
