# tests/testthat/test-dsr_funnel.R
#
# Offline tests for the funnel plots of the DSR Calculator: positions, limits,
# overdispersion and the data for each measure. The positions, limits and
# overdispersion estimates were also checked against PHEindicatormethods 2.1.2
# (OHID) and FunnelPlotR 0.6.0, which are not dependencies.

library(testthat)
library(actepir)

# Calculator rows for one year and sex: Area, Events, Population, Suppressed
# and the rates
funnel_rows <- function(events, population, dsr = NULL, suppressed = FALSE) {
  data.frame(Area = sprintf("A%02d", seq_along(events)), Events = events,
             Population = population, Crude = events / population * 1e5,
             DSR = if (is.null(dsr)) events / population * 1e5 else dsr,
             Suppressed = suppressed, stringsAsFactors = FALSE)
}

test_that("positions are exact Poisson tests of observed against expected events", {

  # An area is above a limit when its expected events fall below the exact
  # (Garwood) lower confidence limit of its count, and below a limit when they
  # exceed the upper confidence limit
  grid <- expand.grid(O = c(0:30, 50, 100, 400),
                      E = c(0.5, 2, 5, 10, 20, 50, 100, 400))
  lcl <- function(o, p) ifelse(o == 0, 0, stats::qchisq(p, 2 * o) / 2)
  ucl <- function(o, p) stats::qchisq(1 - p, 2 * o + 2) / 2
  expected <- with(grid, ifelse(
    E < lcl(O, 0.001), "Above 99.8% limit", ifelse(
    E < lcl(O, 0.025), "Above 95% limit", ifelse(
    E > ucl(O, 0.001), "Below 99.8% limit", ifelse(
    E > ucl(O, 0.025), "Below 95% limit", "Within 95% limits")))))

  pos <- .dsr_funnel_position(grid$O, grid$E)
  expect_equal(as.character(pos), expected)
  expect_equal(levels(pos), .dsr_funnel_positions)

})

test_that("limits trace the counts that are just significant", {

  lim <- .dsr_funnel_limits(1000)
  expect_setequal(unique(paste(lim$Limit, lim$Side)),
                  c("95% upper", "95% lower", "99.8% upper", "99.8% lower"))
  expect_lte(max(lim$Expected), 1000)

  # Each point is a count O = Ratio x Expected with a tail probability of p
  up <- lim[lim$Limit == "95%" & lim$Side == "upper", ]
  expect_equal(stats::pgamma(up$Expected, up$Ratio * up$Expected),
               rep(0.025, nrow(up)), tolerance = 1e-6)
  low <- lim[lim$Limit == "99.8%" & lim$Side == "lower" & lim$Expected > 0, ]
  expect_equal(1 - stats::pgamma(low$Expected, low$Ratio * low$Expected + 1),
               rep(0.001, nrow(low)), tolerance = 1e-6)

  # With many expected events the limits approach 1 +/- z / sqrt(E), and the
  # 99.8% limits lie outside the 95% limits
  at <- function(level, side) {
    l <- lim[lim$Limit == level & lim$Side == side, ]
    stats::approx(l$Expected, l$Ratio, xout = 900)$y
  }
  expect_equal(at("95%", "upper"), 1 + stats::qnorm(0.975) / 30, tolerance = 0.005)
  expect_equal(at("95%", "lower"), 1 - stats::qnorm(0.975) / 30, tolerance = 0.005)
  expect_gt(at("99.8%", "upper"), at("95%", "upper"))
  expect_lt(at("99.8%", "lower"), at("95%", "lower"))

  # Very steep upper limits near E = 0 can be cut off
  expect_lte(max(.dsr_funnel_limits(1000, ratio_max = 5)$Ratio), 5)

})

test_that("overdispersion follows Spiegelhalter's additive random effects method", {

  O <- c(12, 30, 45, 61, 80, 95, 130, 150, 210, 260, 300, 420, 515, 640, 700,
         880, 990, 1200, 1500, 2100)
  E <- c(15, 25, 50, 55, 90, 100, 110, 160, 190, 280, 330, 380, 540, 600, 760,
         850, 1050, 1150, 1600, 1900)
  od <- .dsr_funnel_od(O, E)

  # Values from FunnelPlotR 0.6.0 (data_type "SR", sr_method "CQC")
  expect_equal(od$phi, 1.9539172218, tolerance = 1e-9)
  expect_equal(od$tau2, 0.0005478381, tolerance = 1e-7)
  expect_equal(od$p, stats::pchisq(20 * od$phi, 19, lower.tail = FALSE))
  expect_equal(od$n, 20)

  # The widened limits bring areas outside the Poisson limits inside them
  expect_equal(sum(.dsr_funnel_position(O, E) != "Within 95% limits"), 4)
  expect_equal(sum(.dsr_funnel_position(O, E, od$tau2) != "Within 95% limits"), 1)

  # Counts at their expected values show no overdispersion
  none <- .dsr_funnel_od(round(E), E)
  expect_lt(none$phi, 1)
  expect_equal(none$tau2, 0)
  expect_null(.dsr_funnel_od(c(10, 20), c(12, 18)))

  # Widened limits on the square root scale, squared back to O / E
  lim <- .dsr_funnel_limits(1000, tau2 = od$tau2)
  up  <- lim[lim$Limit == "95%" & lim$Side == "upper", ]
  expect_equal(up$Ratio,
               (1 + stats::qnorm(0.975) * sqrt(1 / (4 * up$Expected) + od$tau2))^2)

})

test_that("crude rates are compared with the overall rate of the areas plotted", {

  rows <- funnel_rows(c(30, 80, 2, 150), c(20000, 40000, 900, 60000),
                      suppressed = c(FALSE, FALSE, TRUE, FALSE))
  f <- .dsr_funnel(rows, "Crude", multiplier = 1e5)

  # The suppressed area is neither plotted nor in the average
  expect_equal(f$points$Area, c("A01", "A02", "A04"))
  expect_equal(f$suppressed, 1)
  expect_equal(f$centre, 260 / 120000 * 1e5)
  expect_equal(f$points$Value, rows$Crude[-3])
  expect_equal(f$points$X, rows$Population[-3])
  expect_equal(f$points$Expected, rows$Population[-3] * 260 / 120000)

  # Limits on the plot's scales: rates against the population
  l <- f$limits[f$limits$Limit == "95%" & f$limits$Side == "upper", ]
  expect_equal(l$Value / f$centre * (l$X * 260 / 120000),
               stats::qgamma(0.025, l$Value / f$centre * (l$X * 260 / 120000)) *
                 l$Value / f$centre, tolerance = 1e-6)

  # Pooled years show the population a year
  pooled <- .dsr_funnel(rows, "Crude", multiplier = 1e5, years = 3)
  expect_equal(pooled$points$X, rows$Population[-3] / 3)
  expect_equal(pooled$centre, f$centre)

})

test_that("DSRs are plotted against the population derived from their events", {

  rows <- funnel_rows(c(40, 9, 120, 300, 0), c(30000, 8000, 50000, 90000, 500),
                      dsr = c(150, 95, 210, 330, 0))
  f <- .dsr_funnel(rows, "DSR", multiplier = 1e5)

  # OHID: population = events / DSR, and the average is total events over
  # total derived population, including DSRs from fewer than 10 events,
  # which are not plotted
  derived <- rows$Events[1:4] * 1e5 / rows$DSR[1:4]
  expect_equal(f$centre, sum(rows$Events[1:4]) / sum(derived) * 1e5)
  expect_equal(f$points$Area, c("A01", "A03", "A04"))
  expect_equal(f$few_events, 1)
  expect_equal(f$points$X, derived[-2])
  expect_equal(f$points$Value, rows$DSR[c(1, 3, 4)])
  expect_equal(f$points$Expected / f$points$Events,
               f$centre / f$points$Value)

})

test_that("events are compared with those expected from age-specific rates", {

  strata <- data.frame(
    Area = rep(c("A01", "A02", "A03"), each = 2), AgeGroup = rep(1:2, 3),
    Events = c(10, 40, 30, 20, 5, 5), Population = c(1000, 2000, 2500, 1500, 400, 600)
  )
  # Age-specific rates over A01 and A02: (10 + 30) / 3500 and (40 + 20) / 3500
  e <- .dsr_funnel_expected(strata, c("A01", "A02"))
  expect_equal(names(e), c("A01", "A02"))
  expect_equal(unname(e), c(1000 * 40 / 3500 + 2000 * 60 / 3500,
                            2500 * 40 / 3500 + 1500 * 60 / 3500))

  rows <- funnel_rows(c(50, 50, 10), c(3000, 4000, 1000),
                      suppressed = c(FALSE, FALSE, TRUE))
  f <- .dsr_funnel(rows, "Events", expected = e)
  expect_equal(f$centre, 1)
  expect_equal(f$points$X, unname(e))
  expect_equal(f$points$Value, 50 / unname(e))
  expect_equal(sum(f$points$Expected), sum(f$points$Events))

})

test_that("the funnel allows for overdispersion when asked", {

  set.seed(1)
  pop <- round(exp(stats::runif(40, log(2000), log(80000))))
  ev  <- stats::rpois(40, pop * 0.02 * exp(stats::rnorm(40, 0, 0.2)))
  rows <- funnel_rows(ev, pop)

  plain <- .dsr_funnel(rows, "Crude")
  wide  <- .dsr_funnel(rows, "Crude", overdispersion = TRUE)
  expect_false(plain$adjusted)
  expect_true(wide$adjusted)
  expect_gt(plain$od$phi, 1)
  expect_identical(wide$od, plain$od)
  outside <- function(f) sum(f$points$Position != "Within 95% limits")
  expect_lt(outside(wide), outside(plain))

  # No areas, or no events, give no points
  expect_null(.dsr_funnel(funnel_rows(c(0, 0), c(100, 200)), "Crude")$points)
  expect_null(.dsr_funnel(funnel_rows(5, 100, suppressed = TRUE), "Crude")$points)

})

test_that("the funnel plot and map identify areas for linking", {

  rows <- funnel_rows(c(30, 80, 45, 150), c(20000, 40000, 15000, 60000))
  f <- .dsr_funnel(rows, "Crude")
  f$points$Tooltip <- paste("Area", f$points$Area)
  p <- .dsr_funnel_plot(f, "Population", "Crude rate per 100,000")
  expect_s3_class(p, "ggplot")

  svg  <- ggiraph::girafe(ggobj = p)$x$html
  for (a in rows$Area) expect_match(svg, paste0("data-id='", a, "'"), fixed = TRUE)
  expect_match(svg, "Area A02", fixed = TRUE)

  # The interactive map: every area has a tooltip and its position colour
  polys <- do.call(rbind, lapply(seq_along(rows$Area), function(i) {
    data.frame(Area = rows$Area[i], Name = paste("Name", i),
               x = 149 + i + c(0, 1, 1, 0, 0), y = -35 + c(0, 0, 1, 1, 0),
               group = paste(rows$Area[i], 1), subgroup = 1L)
  }))
  values <- data.frame(Area = f$points$Area, Value = f$points$Position)
  tips <- stats::setNames(paste("Tip", rows$Area), rows$Area)
  m <- .dsr_map_plot(polys, values, legend = "Funnel plot", tooltips = tips)
  svg <- ggiraph::girafe(ggobj = m)$x$html
  for (a in rows$Area) expect_match(svg, paste0("data-id='", a, "'"), fixed = TRUE)
  expect_match(svg, "Tip A03", fixed = TRUE)
  built <- ggplot2::ggplot_build(m)$data[[1]]
  expect_true(all(built$fill %in% c(.dsr_funnel_colours, "#efefec")))

})
