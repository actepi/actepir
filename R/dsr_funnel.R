# Funnel plots for the DSR Calculator: each area's rate against the size of
# its population, with control limits around the overall rate of the areas
# plotted, so that areas can be compared allowing for chance.
#
# Every measure is tested as a Poisson count: an area's observed events O
# against the events E expected at the overall rate.
# * Crude and age-specific rates: E is the overall rate times the population.
# * DSRs, as in OHID's Fingertips funnel plots (PHEindicatormethods): the
#   population is derived as O / DSR, the population that would give the
#   DSR as a crude rate from the area's events. Areas whose DSR is not
#   presented (dsr_calculate()'s min_events and min_population) are left
#   out, in place of OHID's fewer than 10 events.
# * Events: the ratio O / E, with E from the age-specific rates of the areas
#   plotted (indirect standardisation).
# Limits are exact Poisson limits. Spiegelhalter's (2005) additive random
# effects adjustment widens them when areas vary more than chance allows.

# Two-sided control limits, as the probability outside each side
.dsr_funnel_p <- c("95%" = 0.025, "99.8%" = 0.001)

# Position of an area relative to the limits, and its colour, shape and size.
# Outside the limits the colours are four of the ACT Government's regional
# colours (dev/reference/act-brand.md) in spectral order: coral and yellow
# above, green and blue below, the outer colour beyond the 99.8% limits;
# within them, grey. Shape and size repeat the position for readers who
# cannot tell the colours apart.
.dsr_funnel_positions <- c("Above 99.8% limit", "Above 95% limit",
                           "Within 95% limits", "Below 95% limit",
                           "Below 99.8% limit")
.dsr_funnel_colours <- stats::setNames(
  c("#F15F50", "#C6B30B", "#CBCBC6", "#90A630", "#008FC5"),
  .dsr_funnel_positions
)
.dsr_funnel_shapes <- stats::setNames(c(24, 24, 21, 25, 25),
                                      .dsr_funnel_positions)
.dsr_funnel_sizes <- stats::setNames(c(3.8, 3, 2.6, 3, 3.8),
                                     .dsr_funnel_positions)


# Positions of areas with observed events O and expected events E: exact
# Poisson tests on each side, or, with between-area variance tau2 > 0, normal
# tests on the square root scale, where sqrt(O / E) has variance
# 1 / (4 E) + tau2
#' @noRd
.dsr_funnel_position <- function(observed, expected, tau2 = 0) {
  if (tau2 > 0) {
    z    <- (sqrt(observed / expected) - 1) / sqrt(1 / (4 * expected) + tau2)
    high <- stats::pnorm(z, lower.tail = FALSE)
    low  <- stats::pnorm(z)
  } else {
    high <- stats::ppois(observed - 1, expected, lower.tail = FALSE)
    low  <- stats::ppois(observed, expected)
  }
  p <- .dsr_funnel_p
  i <- ifelse(high < p[["99.8%"]], 1L,
       ifelse(high < p[["95%"]], 2L,
       ifelse(low < p[["99.8%"]], 5L,
       ifelse(low < p[["95%"]], 4L, 3L))))
  factor(.dsr_funnel_positions[i], levels = .dsr_funnel_positions)
}


# Spiegelhalter (2005): z-scores on the square root scale, winsorised at the
# 10th and 90th percentiles, give the overdispersion ratio phi (1 when areas
# vary only by chance) and its chi-squared test; the additive random effects
# model then estimates the between-area variance tau2 (0 without
# overdispersion). NULL for fewer than three areas.
#' @noRd
.dsr_funnel_od <- function(observed, expected, trim = 0.1) {
  n <- length(observed)
  if (n < 3) return(NULL)
  z <- 2 * (sqrt(observed) - sqrt(expected))
  q <- stats::quantile(z, c(trim, 1 - trim), names = FALSE)
  z <- pmin(pmax(z, q[1]), q[2])
  phi <- sum(z^2) / n
  w <- 4 * expected
  tau2 <- max(0, (n * phi - (n - 1)) / (sum(w) - sum(w^2) / sum(w)))
  list(n = n, phi = phi, tau2 = tau2,
       p = stats::pchisq(n * phi, n - 1, lower.tail = FALSE))
}


# Control limits as O / E for expected events up to e_max, one row per point
# of each curve: Limit ("95%", "99.8%"), Side ("upper", "lower"), Expected
# and Ratio. Exact limits are traced through the expected events at which each
# number of events is just significant: P(X >= O) = p when E is the p
# quantile of a gamma distribution with shape O, and P(X <= O) = p when E is
# the 1 - p quantile with shape O + 1. With tau2 > 0 the limits are normal
# limits on the square root scale. Ratios above ratio_max, where the upper
# limits rise steeply towards E = 0, are dropped.
#' @noRd
.dsr_funnel_limits <- function(e_max, tau2 = 0, ratio_max = Inf) {
  e_max <- max(e_max, 10)
  curves <- lapply(names(.dsr_funnel_p), function(level) {
    p <- .dsr_funnel_p[[level]]
    if (tau2 > 0) {
      e <- c(seq(0.05, 1, length.out = 20),
             exp(seq(0, log(e_max), length.out = 300)))
      s <- stats::qnorm(p, lower.tail = FALSE) * sqrt(1 / (4 * e) + tau2)
      upper <- data.frame(Expected = e, Ratio = (1 + s)^2)
      lower <- data.frame(Expected = e, Ratio = pmax(1 - s, 0)^2)
    } else {
      o_max <- e_max + 5 * sqrt(e_max) + 10
      o <- c(seq(0, 50, by = 0.1),
             exp(seq(log(50), log(max(o_max, 51)), length.out = 301))[-1])
      e_up  <- stats::qgamma(p, shape = o[o > 0])
      e_low <- stats::qgamma(1 - p, shape = o + 1)
      upper <- data.frame(Expected = e_up, Ratio = o[o > 0] / e_up)
      # No count is significantly low while E is below the first point
      lower <- rbind(data.frame(Expected = 0, Ratio = 0),
                     data.frame(Expected = e_low, Ratio = o / e_low))
    }
    rbind(data.frame(Limit = level, Side = "upper", upper),
          data.frame(Limit = level, Side = "lower", lower))
  })
  out <- do.call(rbind, curves)
  out <- out[out$Expected <= e_max & out$Ratio <= ratio_max, , drop = FALSE]
  rownames(out) <- NULL
  out
}


# Expected events of each area at the age-specific rates of the areas given,
# from age-group counts (.dsr_strata() data for one year or period and sex)
#' @noRd
.dsr_funnel_expected <- function(strata, areas) {
  d <- strata[strata$Area %in% areas, , drop = FALSE]
  events <- tapply(d$Events, d$AgeGroup, sum)
  pop    <- tapply(d$Population, d$AgeGroup, sum)
  rate   <- ifelse(pop > 0, events / pop, 0)
  e <- tapply(d$Population * rate[as.character(d$AgeGroup)], d$Area, sum)
  stats::setNames(as.vector(e[areas]), areas)
}


# Funnel plot data for one year (or period) and sex of the calculator's
# results. rows has one row per area: Area, Events, Population, Suppressed
# and the measure's column (DSR or Crude; Crude for age-specific rates).
# For "Events", expected holds each area's expected events, named by area.
# years is the number of years in each row, so that the x-axis shows the
# population a year. Suppressed areas, and for DSRs areas whose DSR is not
# presented (a DSRNote), are left out of the average and not plotted.
# Returns the areas plotted (points), the average (centre), the limits on
# the plot's scales, and the overdispersion estimate (od).
#' @noRd
.dsr_funnel <- function(rows, measure = c("DSR", "Crude", "AgeRate", "Events"),
                        multiplier = 100000, years = 1, expected = NULL,
                        overdispersion = FALSE) {

  measure <- match.arg(measure)
  d <- rows[!rows$Suppressed & !is.na(rows$Events), , drop = FALSE]
  n_suppressed <- sum(rows$Suppressed)
  n_not_presented <- 0

  if (measure == "DSR") {
    not_presented <- if (is.null(d$DSRNote)) FALSE else !is.na(d$DSRNote)
    n_not_presented <- sum(not_presented)
    d <- d[!not_presented, , drop = FALSE]
  }
  n_rows <- nrow(d)

  if (measure == "DSR") {
    d <- d[!is.na(d$DSR) & d$DSR > 0, , drop = FALSE]
    d$Size <- d$Events * multiplier / d$DSR
    d$Value <- d$DSR
  } else if (measure == "Events") {
    d$Expected <- unname(expected[d$Area])
    d <- d[!is.na(d$Expected) & d$Expected > 0, , drop = FALSE]
    d$Size <- d$Expected
    d$Value <- d$Events / d$Expected
  } else {
    d <- d[d$Population > 0, , drop = FALSE]
    d$Size <- d$Population
    d$Value <- d$Crude
  }

  n_none <- n_rows - nrow(d)

  # The average over the areas plotted, as events per unit of size
  rate <- if (nrow(d)) sum(d$Events) / sum(d$Size) else NA_real_
  pts <- d[, c("Area", "Events", "Population", "Value", "Size"), drop = FALSE]

  out <- list(measure = measure, multiplier = multiplier, years = years,
              points = NULL, centre = NA_real_, limits = NULL, od = NULL,
              adjusted = FALSE, suppressed = n_suppressed, no_value = n_none,
              not_presented = n_not_presented)
  if (!nrow(pts) || !is.finite(rate) || rate <= 0) return(out)

  pts$Expected <- rate * pts$Size
  od   <- .dsr_funnel_od(pts$Events, pts$Expected)
  tau2 <- if (overdispersion && !is.null(od)) od$tau2 else 0
  pts$Position <- .dsr_funnel_position(pts$Events, pts$Expected, tau2)

  # Limits and points on the plot's scales: rates against the population a
  # year, or O / E against expected events
  lim <- .dsr_funnel_limits(max(pts$Expected) * 1.05, tau2,
                            ratio_max = 3 * max(pts$Events / pts$Expected, 2))
  if (measure == "Events") {
    pts$X     <- pts$Expected
    lim$X     <- lim$Expected
    lim$Value <- lim$Ratio
    centre    <- 1
  } else {
    centre    <- rate * multiplier
    pts$X     <- pts$Size / years
    lim$X     <- lim$Expected / rate / years
    lim$Value <- lim$Ratio * centre
  }
  rownames(pts) <- NULL

  out$points   <- pts
  out$centre   <- centre
  out$limits   <- lim
  out$od       <- od
  out$adjusted <- tau2 > 0
  out

}


# Funnel plot of .dsr_funnel() output for ggiraph. The points carry a Tooltip
# column and are identified by Area, as the areas of the map are, so that the
# two can be linked. The view takes in every point, the average and the
# limits at the largest area.
#' @noRd
.dsr_funnel_plot <- function(f, xlab, ylab, title = NULL, subtitle = NULL,
                             caption = NULL) {

  pts <- f$points
  lim <- f$limits
  x_max <- max(pts$X) * 1.05
  near  <- lim$Value[lim$X >= 0.9 * max(pts$X) & lim$X <= x_max]
  y <- range(c(pts$Value, f$centre, near))
  y <- c(max(0, y[1] - 0.08 * diff(y)), y[2] + 0.08 * diff(y))
  # Areas outside the limits are drawn last, over those within
  pts <- pts[order(pts$Position == "Within 95% limits", decreasing = TRUE), ]
  comma <- function(v) format(v, big.mark = ",", scientific = FALSE, trim = TRUE)
  wrap  <- function(s, width) {
    if (!is.null(s)) paste(strwrap(s, width = width), collapse = "\n")
  }

  ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = f$centre, colour = "#333740",
                        linewidth = 0.5) +
    ggplot2::geom_line(
      data = lim,
      ggplot2::aes(x = .data$X, y = .data$Value,
                   group = paste(.data$Limit, .data$Side),
                   linetype = .data$Limit),
      colour = "#6b6b6b", linewidth = 0.4
    ) +
    ggiraph::geom_point_interactive(
      data = pts,
      ggplot2::aes(x = .data$X, y = .data$Value, fill = .data$Position,
                   shape = .data$Position, size = .data$Position,
                   tooltip = .data$Tooltip, data_id = .data$Area),
      colour = "white", stroke = 0.5, hover_nearest = TRUE,
      show.legend = c(fill = TRUE, shape = TRUE, size = TRUE,
                      linetype = FALSE)
    ) +
    ggplot2::scale_fill_manual(values = .dsr_funnel_colours, drop = FALSE,
                               name = NULL) +
    ggplot2::scale_shape_manual(values = .dsr_funnel_shapes, drop = FALSE,
                                name = NULL) +
    ggplot2::scale_size_manual(values = .dsr_funnel_sizes, drop = FALSE,
                               name = NULL) +
    ggplot2::scale_linetype_manual(values = c("95%" = "22", "99.8%" = "solid"),
                                   name = "Limits") +
    ggplot2::scale_x_continuous(labels = comma) +
    ggplot2::scale_y_continuous(labels = comma) +
    ggplot2::coord_cartesian(xlim = c(0, x_max), ylim = y, expand = FALSE) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(nrow = 2, order = 1),
      shape = ggplot2::guide_legend(nrow = 2, order = 1),
      size = ggplot2::guide_legend(nrow = 2, order = 1),
      linetype = ggplot2::guide_legend(order = 2)
    ) +
    ggplot2::labs(x = xlab, y = ylab, title = title,
                  subtitle = wrap(subtitle, 70), caption = wrap(caption, 90)) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.title       = ggplot2::element_text(colour = "#320557", face = "bold",
                                               size = 13),
      plot.subtitle    = ggplot2::element_text(colour = "#333740",
                                               margin = ggplot2::margin(4, 0, 8, 0)),
      plot.caption     = ggplot2::element_text(colour = "#777777", size = 8,
                                               hjust = 0),
      axis.title       = ggplot2::element_text(colour = "#333740", size = 9),
      axis.text        = ggplot2::element_text(colour = "#555555", size = 8),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "#ececec",
                                               linewidth = 0.3),
      legend.position  = "bottom",
      legend.box       = "vertical",
      legend.text      = ggplot2::element_text(colour = "#333740", size = 8),
      legend.title     = ggplot2::element_text(colour = "#333740", size = 8),
      plot.background  = ggplot2::element_rect(fill = "white", colour = NA),
      plot.margin      = ggplot2::margin(10, 14, 10, 10)
    )

}
