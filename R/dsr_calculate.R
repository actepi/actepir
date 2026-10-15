#' Directly age-standardised rates
#'
#' Calculates event counts, crude rates and directly age-standardised rates
#' (DSRs), with confidence intervals, from event counts, a population and a
#' standard population, each broken down by age group. [dsr_age_specific()]
#' returns the age-specific rates the DSR is built from.
#'
#' These functions do the calculation for [episerver_dsr()], and the code that
#' the calculator copies calls them. They need no database connection, so they
#' also work on counts from any other source arranged as described below.
#'
#' @param events Data frame of event counts with an `AgeGroup` column, an
#'   `Events` column, the columns named in `by`, and a `Sex` column when
#'   `by_sex = TRUE`.
#' @param population Data frame of population counts with an `AgeGroup`
#'   column, a `Population` column, the columns named in `by`, and a `Sex`
#'   column when `by_sex = TRUE`. An optional `AgeGroupName` column labels the
#'   age groups in [dsr_age_specific()].
#' @param standard Data frame of the standard population, one row per age
#'   group, with `AgeGroup` and `StdPopValue` columns.
#' @param by Character vector of grouping columns, for example
#'   `c("Year", "Area")`. One row is returned per group and sex. `NULL`
#'   (default) returns one row per sex.
#' @param by_sex Logical. `TRUE` returns rates for males, females and persons;
#'   `FALSE` (default) returns persons only.
#' @param multiplier Number. Rates are expressed per this many population.
#'   Default 100,000.
#' @param conf_level Number between 0 and 1. Confidence level of the
#'   intervals. Default 0.95.
#' @param suppress Number. Rows with fewer events than this are suppressed:
#'   their events and rates are not reported. Default 0, no suppression.
#' @param min_events,min_population Numbers. A row's DSR is not presented when
#'   it has fewer than `min_events` events, or a population under
#'   `min_population` in any age group. The AIHW (2011) principles, which the
#'   ABS applies, set these at 20 and 30. Default 0, no check.
#'
#' @return `dsr_calculate()` returns a data frame with the `by` columns, `Sex`
#'   (`"Male"`, `"Female"` or `"Persons"`), `Events`, `Population`, the crude
#'   rate (`Crude`, `CrudeLower`, `CrudeUpper`), the DSR (`DSR`, `DSRLower`,
#'   `DSRUpper`), `DSRNote` and `Suppressed`. `DSRNote` says why the DSR is
#'   `NA` in a row that is not suppressed, and is otherwise `NA`.
#'   `dsr_age_specific()` returns the `by`
#'   columns, `Sex`, `AgeGroup`, `AgeGroupName` (when `population` has it),
#'   `Events`, `Population`, the rate (`Rate`, `RateLower`, `RateUpper`) and
#'   `Suppressed`. Rates are per `multiplier`. Events left out of the rates
#'   are counted in the `"excluded"` attribute (columns `Reason` and `Events`)
#'   and reported in a message.
#'
#' @details
#' **Matching.** Events are matched to the population on every column the two
#' data frames share other than `Events`, `Population`, `Sex` and
#' `AgeGroupName`, so `AgeGroup` and the `by` columns are always matched. The
#' matched counts are then summed to the `by` groups. A shared column left out
#' of `by` is therefore pooled: leaving `Year` out gives one rate over all the
#' years, with the populations summed into person-years. Every population row
#' counts towards the denominator of its group.
#'
#' **Exclusions.** Events with a missing value in a matching column (for
#' example `AgeGroup` when age was not recorded, or `Area` when residence was
#' not geocoded) and events with no matching population row are left out of
#' every rate.
#'
#' **Sex.** Persons include events of every sex code. Male and female rates use
#' the METeOR codes 1 (male) and 2 (female), which `population$Sex` must use;
#' events with any other code count towards persons only.
#'
#' **Standardisation.** The weights are each age group's share of the standard
#' population, taken over the age groups present in `population`. Restricting
#' `events` and `population` to an age range therefore standardises within that
#' range. An age group with no population contributes nothing when it also has
#' no events; when it has events its rate is undefined, so the DSR for that
#' group is `NA`.
#'
#' **Confidence intervals.** Crude and age-specific rates use exact Poisson
#' limits for the count (Garwood, 1936), divided by the population. DSRs use
#' the method of Dobson et al. (1991), which scales the exact Poisson limits of
#' the total count by the variance of the DSR; a negative lower limit is set
#' to 0. The DSR interval is `NA` when there are no events.
#'
#' **Reliability.** The AIHW (2011) principles for direct age-standardisation,
#' which the ABS applies (ABS 2011), advise against age-standardising when a
#' population has fewer than 20 events in total (Principle 3) or a population
#' under 30 in any one age group (Principle 2). Pooling years or areas raises
#' the events, and wider age groups raise the populations; otherwise counts,
#' crude rates and age-specific rates are reported instead. With `min_events`
#' and `min_population` set, a row that falls short has its DSR and interval
#' set to `NA` and the reason in `DSRNote`, for example `"fewer than 20
#' events"` or `"a population under 30 in an age group"`, the events taking
#' precedence; its counts and crude rate are kept. An age group with no
#' population row in a group counts as a population of 0. `DSRNote` also
#' gives `"events in an age group with no population"` for a DSR that is
#' undefined (see Standardisation).
#'
#' **Suppression.** A row with fewer than `suppress` events, zero included,
#' has `Suppressed` set to `TRUE` and its events and rates set to `NA`; the
#' population is kept. Each row is judged on its own count, so a suppressed
#' count can sometimes be worked out from other rows (persons less females,
#' for example). Complementary suppression is not applied.
#'
#' @references
#' Australian Bureau of Statistics (2011). *Deaths, Australia, 2010*, Appendix
#' 1: Principles on the use of direct age-standardisation. Cat. no. 3302.0.
#'
#' Australian Institute of Health and Welfare (2011). *Principles on the use of
#' direct age-standardisation in administrative data collections: for
#' measuring the gap between Indigenous and non-Indigenous Australians*. Cat.
#' no. CSI 12. Canberra: AIHW.
#'
#' Dobson AJ, Kuulasmaa K, Eberle E, Scherer J (1991). Confidence intervals
#' for weighted sums of Poisson parameters. *Statistics in Medicine*, 10(3),
#' 457-462.
#'
#' Garwood F (1936). Fiducial limits for the Poisson distribution.
#' *Biometrika*, 28(3/4), 437-442.
#'
#' @keywords rates standardisation
#'
#' @seealso [episerver_dsr()] for the calculator that queries EpiServer.
#'
#' @export
#'
#' @examples
#' events <- data.frame(
#'   Year     = 2023,
#'   Sex      = rep(c(1, 2), each = 3),
#'   AgeGroup = rep(1:3, times = 2),
#'   Events   = c(12, 30, 85, 9, 26, 70)
#' )
#' population <- data.frame(
#'   Year       = 2023,
#'   Sex        = rep(c(1, 2), each = 3),
#'   AgeGroup   = rep(1:3, times = 2),
#'   Population = c(5000, 6000, 2500, 4800, 6100, 3000)
#' )
#' standard <- data.frame(AgeGroup = 1:3, StdPopValue = c(30000, 50000, 20000))
#'
#' dsr_calculate(events, population, standard, by = "Year")
#' dsr_calculate(events, population, standard, by = "Year", by_sex = TRUE)
#' dsr_age_specific(events, population, by = "Year")
#'
#' @author Warren Holroyd
#'
dsr_calculate <- function(events, population, standard,
                          by = NULL, by_sex = FALSE,
                          multiplier = 100000, conf_level = 0.95,
                          suppress = 0, min_events = 0, min_population = 0) {

  .dsr_check_args(multiplier, conf_level, suppress)
  .dsr_check_min(min_events, "min_events", "events")
  .dsr_check_min(min_population, "min_population", "people")
  .dsr_check_df(standard, "standard", c("AgeGroup", "StdPopValue"))
  if (anyDuplicated(standard$AgeGroup)) {
    stop("'standard' must have one row per AgeGroup.", call. = FALSE)
  }
  if (!is.numeric(standard$StdPopValue) || anyNA(standard$StdPopValue) ||
      any(standard$StdPopValue <= 0)) {
    stop("'standard$StdPopValue' must be positive numbers.", call. = FALSE)
  }

  st <- .dsr_strata(events, population, by, by_sex)
  d  <- st$data

  no_weight <- setdiff(unique(d$AgeGroup), standard$AgeGroup)
  if (length(no_weight)) {
    stop("'standard' has no row for AgeGroup ",
         paste(no_weight, collapse = ", "), ".", call. = FALSE)
  }

  # Weights are standard shares over the age groups present in the population
  std <- standard[standard$AgeGroup %in% d$AgeGroup, , drop = FALSE]
  d$w <- (std$StdPopValue / sum(std$StdPopValue))[match(d$AgeGroup, std$AgeGroup)]

  # An age group with no population has an undefined rate if it has events and
  # contributes nothing if it has none
  pos  <- d$Population > 0
  rate <- ifelse(pos, d$Events / d$Population, ifelse(d$Events > 0, NA, 0))
  d$wr <- d$w * rate
  d$wv <- ifelse(pos, d$w^2 * d$Events / d$Population^2, 0)
  # Age groups under min_population, and age groups present, in each group
  d$Small <- d$Population < min_population
  d$Cells <- 1

  out <- .dsr_sum(d, c(by, "Sex"),
                  c("Events", "Population", "wr", "wv", "Small", "Cells"))

  alpha <- (1 - conf_level) / 2
  lo <- .pois_lower(out$Events, alpha)
  hi <- .pois_upper(out$Events, alpha)

  N <- ifelse(out$Population > 0, out$Population, NA)
  out$Crude      <- out$Events / N * multiplier
  out$CrudeLower <- lo / N * multiplier
  out$CrudeUpper <- hi / N * multiplier

  # Dobson et al. (1991): exact limits for the total count, scaled by the
  # standard error of the DSR per event
  s <- ifelse(out$Events > 0, sqrt(out$wv / out$Events), NA)
  out$DSR      <- out$wr * multiplier
  out$DSRLower <- pmax(out$wr + s * (lo - out$Events), 0) * multiplier
  out$DSRUpper <- (out$wr + s * (hi - out$Events)) * multiplier

  # Why a DSR is not presented: it is undefined, or the group falls short of
  # min_events or min_population (see Reliability), the events first
  short <- min_population > 0 &
    (out$Small > 0 | out$Cells < length(unique(d$AgeGroup)))
  note <- rep(NA_character_, nrow(out))
  note[is.na(out$DSR)] <- "events in an age group with no population"
  note[short] <- paste("a population under", .fmt_count(min_population),
                       "in an age group")
  note[out$Events < min_events] <- paste("fewer than", .fmt_count(min_events),
                                         "events")
  out[!is.na(note), c("DSR", "DSRLower", "DSRUpper")] <- NA
  out$DSRNote <- note

  out <- out[.dsr_order(out, c(by, "Sex")),
             c(by, "Sex", "Events", "Population",
               "Crude", "CrudeLower", "CrudeUpper",
               "DSR", "DSRLower", "DSRUpper", "DSRNote")]
  rownames(out) <- NULL

  if (any(!is.na(note))) {
    n <- table(factor(note[!is.na(note)], unique(note[!is.na(note)])))
    message("The DSR is NA for ",
            paste0(n, " group(s) with ", names(n), collapse = ", "), ".")
  }

  .dsr_report(.dsr_suppress(out, suppress), st$excluded)

}


#' @rdname dsr_calculate
#' @export
dsr_age_specific <- function(events, population,
                             by = NULL, by_sex = FALSE,
                             multiplier = 100000, conf_level = 0.95,
                             suppress = 0) {

  .dsr_check_args(multiplier, conf_level, suppress)

  st <- .dsr_strata(events, population, by, by_sex)
  out <- st$data

  alpha <- (1 - conf_level) / 2
  N <- ifelse(out$Population > 0, out$Population, NA)
  out$Rate      <- out$Events / N * multiplier
  out$RateLower <- .pois_lower(out$Events, alpha) / N * multiplier
  out$RateUpper <- .pois_upper(out$Events, alpha) / N * multiplier

  cols <- c(by, "Sex", "AgeGroup")
  if ("AgeGroupName" %in% names(population)) {
    lab <- population[!duplicated(population$AgeGroup),
                      c("AgeGroup", "AgeGroupName")]
    out$AgeGroupName <- lab$AgeGroupName[match(out$AgeGroup, lab$AgeGroup)]
    cols <- c(cols, "AgeGroupName")
  }

  ord <- .dsr_order(out, c(by, "Sex", "AgeGroup"))
  out <- out[ord, c(cols, "Events", "Population",
                    "Rate", "RateLower", "RateUpper")]
  rownames(out) <- NULL

  .dsr_report(.dsr_suppress(out, suppress), st$excluded)

}


# Flags rows with fewer than `suppress` events and blanks their events and
# rates (every column after Population)
#' @noRd
.dsr_suppress <- function(out, suppress) {
  rates <- names(out)[-seq_len(match("Population", names(out)))]
  out$Suppressed <- out$Events < suppress
  out[out$Suppressed, c("Events", rates)] <- NA
  out
}


# Matches events to the population and sums both to the by groups, sex and age
# group. Returns list(data, excluded): data has the by columns, Sex, AgeGroup,
# Events and Population, with one row per population cell; excluded counts the
# events that could not be used, by reason.
#' @noRd
.dsr_strata <- function(events, population, by, by_sex) {

  if (!is.logical(by_sex) || length(by_sex) != 1 || is.na(by_sex)) {
    stop("'by_sex' must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.null(by) && (!is.character(by) || anyNA(by))) {
    stop("'by' must be a character vector of column names.", call. = FALSE)
  }
  reserved <- c("Sex", "AgeGroup", "AgeGroupName", "Events", "Population")
  if (any(by %in% reserved)) {
    stop("'by' cannot include ", paste(intersect(by, reserved), collapse = ", "),
         ". Use 'by_sex' for sex and dsr_age_specific() for age groups.",
         call. = FALSE)
  }

  sex <- if (by_sex) "Sex"
  .dsr_check_df(events, "events", c(by, sex, "AgeGroup", "Events"))
  .dsr_check_df(population, "population", c(by, sex, "AgeGroup", "Population"))
  .dsr_check_counts(events$Events, "events$Events")
  .dsr_check_counts(population$Population, "population$Population")
  if (nrow(population) == 0) {
    stop("'population' has no rows.", call. = FALSE)
  }
  if (by_sex && !all(population$Sex %in% c(1, 2))) {
    stop("'population$Sex' must use the codes 1 (male) and 2 (female).",
         call. = FALSE)
  }

  key <- setdiff(intersect(names(events), names(population)),
                 c("Events", "Population", "Sex", "AgeGroupName"))
  key <- c("AgeGroup", setdiff(key, "AgeGroup"))

  # The first matching column (in key order) that is missing gives the reason
  na_col <- rep(NA_character_, nrow(events))
  for (k in rev(key)) na_col[is.na(events[[k]])] <- k

  usable <- is.na(na_col)
  matched <- rep(FALSE, nrow(events))
  if (any(usable)) {
    k <- .dsr_keys(events[usable, , drop = FALSE], population, key)
    matched[usable] <- k$x %in% k$y
  }

  excluded <- data.frame(
    Reason = c(paste("Missing", key), "No matching population"),
    Events = c(vapply(key, function(k) sum(events$Events[na_col %in% k]),
                      numeric(1)),
               sum(events$Events[usable & !matched])),
    stringsAsFactors = FALSE
  )
  excluded <- excluded[excluded$Events > 0, , drop = FALSE]
  rownames(excluded) <- NULL

  kept <- events[matched, , drop = FALSE]
  grp  <- c(by, "AgeGroup")
  sexes <- if (by_sex) list(Male = 1, Female = 2, Persons = NULL) else list(Persons = NULL)

  data <- do.call(rbind, lapply(names(sexes), function(label) {
    code <- sexes[[label]]
    ev  <- if (is.null(code)) kept else kept[kept$Sex %in% code, , drop = FALSE]
    pop <- if (is.null(code)) population else population[population$Sex %in% code, , drop = FALSE]
    d <- .dsr_sum(pop, grp, "Population")
    e <- .dsr_sum(ev, grp, "Events")
    k <- .dsr_keys(d, e, grp)
    d$Events <- e$Events[match(k$x, k$y)]
    d$Events[is.na(d$Events)] <- 0
    d$Sex <- rep(label, nrow(d))
    d[, c(by, "Sex", "AgeGroup", "Events", "Population")]
  }))

  list(data = data, excluded = excluded)

}


# Row keys of x and y on cols, for matching with match() or %in%. Each key
# column of x is combined with its counterpart in y before conversion, so
# columns of different types (integer and double, factor and character)
# produce the same key for the same value.
#' @noRd
.dsr_keys <- function(x, y, cols) {
  parts <- lapply(cols, function(k) {
    a <- x[[k]]
    b <- y[[k]]
    if (is.factor(a)) a <- as.character(a)
    if (is.factor(b)) b <- as.character(b)
    as.character(c(a, b))
  })
  key <- do.call(paste, c(parts, sep = "\r"))
  list(x = key[seq_len(nrow(x))], y = key[nrow(x) + seq_len(nrow(y))])
}


# Sums the value columns of df within groups of cols, keeping groups in order
# of first appearance.
#' @noRd
.dsr_sum <- function(df, cols, values) {

  if (!length(cols)) {
    out <- as.data.frame(lapply(df[values], function(v) sum(as.numeric(v))))
    return(out)
  }

  if (nrow(df) == 0) {
    out <- df[0, cols, drop = FALSE]
    for (v in values) out[[v]] <- numeric(0)
    return(out)
  }

  key   <- do.call(paste, c(lapply(df[cols], as.character), sep = "\r"))
  group <- match(key, unique(key))
  out   <- df[!duplicated(group), cols, drop = FALSE]
  sums  <- rowsum(as.matrix(as.data.frame(lapply(df[values], as.numeric))),
                  group, reorder = FALSE)
  for (v in values) out[[v]] <- unname(sums[, v])
  rownames(out) <- NULL
  out

}


# Row order by cols, with Sex in the order Male, Female, Persons
#' @noRd
.dsr_order <- function(df, cols) {
  keys <- lapply(cols, function(k) {
    if (k == "Sex") match(df$Sex, c("Male", "Female", "Persons")) else df[[k]]
  })
  if (!length(keys)) return(seq_len(nrow(df)))
  do.call(order, unname(keys))
}


# Exact (Garwood) Poisson limits for a count
#' @noRd
.pois_lower <- function(x, alpha) {
  ifelse(x > 0, stats::qchisq(alpha, 2 * x) / 2, 0)
}

#' @noRd
.pois_upper <- function(x, alpha) {
  stats::qchisq(1 - alpha, 2 * (x + 1)) / 2
}


# Attaches the exclusion counts and reports them
#' @noRd
.dsr_report <- function(out, excluded) {
  if (nrow(excluded)) {
    message("Events excluded from the rates: ",
            paste0(tolower(excluded$Reason), " ", .fmt_count(excluded$Events),
                   collapse = "; "), ".")
  }
  attr(out, "excluded") <- excluded
  out
}


#' @noRd
.fmt_count <- function(x) {
  vapply(x, format, character(1), big.mark = ",", scientific = FALSE,
         trim = TRUE)
}


#' @noRd
.dsr_check_df <- function(x, name, cols) {
  if (!is.data.frame(x)) {
    stop("'", name, "' must be a data frame.", call. = FALSE)
  }
  missing <- setdiff(cols, names(x))
  if (length(missing)) {
    stop("'", name, "' has no column ", paste(missing, collapse = ", "), ".",
         call. = FALSE)
  }
}


#' @noRd
.dsr_check_counts <- function(x, name) {
  if (!is.numeric(x) || anyNA(x) || any(x < 0)) {
    stop("'", name, "' must be non-negative numbers with no missing values.",
         call. = FALSE)
  }
}


#' @noRd
.dsr_check_args <- function(multiplier, conf_level, suppress) {
  if (!is.numeric(multiplier) || length(multiplier) != 1 ||
      !is.finite(multiplier) || multiplier <= 0) {
    stop("'multiplier' must be a positive number.", call. = FALSE)
  }
  if (!is.numeric(conf_level) || length(conf_level) != 1 ||
      is.na(conf_level) || conf_level <= 0 || conf_level >= 1) {
    stop("'conf_level' must be a number between 0 and 1.", call. = FALSE)
  }
  .dsr_check_min(suppress, "suppress", "events")
}


#' @noRd
.dsr_check_min <- function(x, name, unit) {
  if (!is.numeric(x) || length(x) != 1 || !is.finite(x) || x < 0) {
    stop("'", name, "' must be a number of ", unit, ", 0 or more.",
         call. = FALSE)
  }
}
