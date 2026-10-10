#' Calculate Age-Standardised Rates from EpiServer
#'
#' @description
#' Opens the DSR Calculator, which calculates directly age-standardised rates
#' (DSRs) of emergency department presentations (ED) or admitted patient
#' separations (APC). It queries EpiServer for event counts by calendar year,
#' sex, 5-year age group and area of residence, matches them to the estimated
#' resident population of the same year, and standardises them to a chosen
#' standard population. Events can be limited to ICD-10 diagnosis codes. It
#' shows counts, crude rates and DSRs, optionally with age-specific rates,
#' maps them by area, and copies R code that reproduces the results.
#'
#' Like [episerver_browse()], the calculator runs in a background R process by
#' default so that the console stays free, and it can be opened from the ACT
#' Epidemiology menu in the RStudio Addins menu.
#'
#' @inheritParams episerver_browse
#' @inheritDotParams episerver_connect encrypt trust_certificate
#'
#' @return In background mode, invisibly returns the `callr` process handle.
#'   In foreground mode, returns nothing.
#'
#' @details
#' **Options**
#' * *Dataset*: ED presentations, counted by the calendar year of
#'   `PresentationDateTime`, or APC separations, counted by the calendar year
#'   of `SeparationDate`.
#' * *Population*: ACT residents, using the ACT population tables; ACT and
#'   surrounds; or Australian residents, using the Australian tables. ACT and
#'   surrounds covers the ACT and the four NSW SA3s that border it
#'   (Queanbeyan, Snowy Mountains, Young - Yass and Tumut - Tumbarumba) with
#'   their SA2s, and also uses the Australian tables. At state level it gives
#'   two areas, the ACT and NSW (surrounds), each summed from its SA3s.
#' * *Geographic level*: state or territory (`geo_STATE`), SA3
#'   (`geo_SA3_2021`) or SA2 (`geo_SA2_2021`) of residence, with rates for
#'   each area. Populations by 5-year age group are not available below SA2,
#'   so SA1 and mesh blocks are not offered.
#' * *Standard population*: any standard with 5-year age groups in
#'   `Analysis.dbo.StandardPops`. The default is the Australian 2001 standard
#'   population.
#' * *Calendar years*: one row of results per year, or per period when the
#'   years are pooled. The years offered are those covered by both the event
#'   data and the population table, and a year only partly covered by the
#'   event data is flagged.
#' * *Pool years*: pools the years into periods of 2 to 5 years, or into one
#'   period of all the selected years, for areas or conditions with few
#'   events. The number of years selected must be a multiple of the period
#'   length. Events and populations are summed over the years of a period, so
#'   the rates are per person-year.
#' * *Male and female*: adds male and female rates to the rates for persons.
#' * *Age-specific rates*: adds a table of rates by 5-year age group.
#' * *Rate per*: 1,000, 10,000 or 100,000 population.
#' * *Suppress counts below*: rows with fewer events than this have their
#'   events and rates withheld (see [dsr_calculate()]). The default is 5; 0
#'   turns suppression off.
#' * *Diagnosis codes*: counts only events with a diagnosis matching any of
#'   the ICD-10 codes or ranges entered, for example `J45`, `C13-C15.45` or
#'   `E18.3 to E18.78`. A partial code matches every code that starts with
#'   it. A range runs from its first code to its last and includes every
#'   code that starts with the last. Dots and case do not matter. Only
#'   ICD-10 coded records are searched: ED records with an `ICD10AMEdition`
#'   other than 99, and APC records with an `ICDVersion` of 10. With no codes
#'   every event is counted.
#' * *Search*: for APC, the principal diagnosis (`Diagnosis1`) or any
#'   diagnosis (`Diagnosis1` to `Diagnosis100`). ED always searches all four
#'   of its diagnosis fields (`EDShortListCode` and `Diagnosis1` to
#'   `Diagnosis3`), as `Diagnosis1` is recorded to 2020 and `EDShortListCode`
#'   from 2013, so neither holds the principal diagnosis in every year.
#'
#' **Tables and map**
#'
#' The *Tables* tab shows the rates and, when chosen, the age-specific
#' rates. *Copy table* copies a table as tab-separated text that pastes into
#' Excel.
#'
#' The *Map* tab maps the age-standardised rate, crude rate or events of one
#' year or period and sex by area. Australia-wide SA3 and SA2 maps open on
#' the ACT and surrounding region; the view of Australia leaves out
#' Christmas, Cocos (Keeling) and Norfolk Islands. Boundaries are the ASGS
#' Edition 3 (2021) boundaries, downloaded from the ABS boundary service
#' (`geo.abs.gov.au`) the first time a level is mapped and kept in the folder
#' given by `tools::R_user_dir("actepir", "cache")`.
#' *Copy map* copies the map as an image where the browser allows it; *PNG*
#' and *PDF* download it.
#'
#' **Data**
#'
#' Age groups (0-4, 5-9, ..., 80-84, 85+) are formed from age in years
#' (`AgeYrs` in ED, `AgeYears` in APC) and match `AgeGroup05Code` in the
#' population tables. Populations come from the 5-year estimated resident
#' population tables (`ERP5_STE`, `ERP5_SA3` and `ERP5_SA2`, ACT or
#' Australian) for the same year as the events. Area of residence comes from
#' the geocoded `geo_` columns, on the ASGS 2021 boundaries the population
#' tables use; for ACT and surrounds, the state comes from the SA3
#' (`geo_SA3_2021`).
#'
#' Events with no geocoded residence, events in an area missing from the
#' population table and events with no recorded age are left out of the
#' rates, and the calculator reports how many there are. See
#' [dsr_calculate()] for how the rates and their 95% confidence intervals are
#' calculated.
#'
#' **Code**
#'
#' The code button (*Copy Code* in background mode, *Insert Code* in
#' foreground mode) gives R code containing the three SQL queries the
#' calculator runs, including any diagnosis condition, the lines that assign
#' each year to its period when the years are pooled, and the
#' [dsr_calculate()] call, with the suppression threshold, that turns their
#' results into the table shown. The code reproduces the results outside the
#' calculator and can be adapted, for example by adding conditions to the
#' events query.
#'
#' @seealso
#' [dsr_calculate()] and [dsr_age_specific()] for the calculations,
#' [episerver_dsr_stop()] for stopping the background app
#'
#' @keywords episerver rates standardisation interactive
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Launch in a background process; console stays free
#' episerver_dsr()
#'
#' # Open in the system web browser
#' episerver_dsr(display = "browser")
#'
#' # Stop the background app
#' episerver_dsr_stop()
#' }
#'
#' @author Warren Holroyd
#'
episerver_dsr <- function(background = TRUE,
                          display = c("viewer", "window", "browser"),
                          port = NULL,
                          driver = NULL, max_attempts = NULL, ...) {

  display <- match.arg(display)

  .run_app(
    key          = "dsr",
    title        = "DSR Calculator",
    fn           = "episerver_dsr",
    factory      = "episerver_dsr_app",
    factory_args = c(list(driver = driver, max_attempts = max_attempts),
                     list(...)),
    background   = background,
    display      = display,
    port         = port
  )

}


#' Stop the Background DSR Calculator
#'
#' @description
#' Stops the background process started by [episerver_dsr()]. Has no effect if
#' no background calculator is running.
#'
#' @return Invisibly returns `TRUE` if a process was stopped and `FALSE`
#'   otherwise.
#'
#' @seealso [episerver_dsr()]
#'
#' @keywords episerver interactive
#'
#' @export
#'
#' @examples
#' \dontrun{
#' episerver_dsr_stop()
#' }
#'
#' @author Warren Holroyd
#'
episerver_dsr_stop <- function() {

  .stop_app("dsr", "DSR Calculator")

}


# ── Definitions ─────────────────────────────────────────────────────────────

# Event tables in Analysis.dbo: the date that sets the calendar year, the age
# in years, the diagnosis fields, whether the first of them is a principal
# diagnosis, and the condition that keeps ICD-10 coded records when diagnosis
# codes are searched. ED has no principal diagnosis field that spans the
# years: Diagnosis1 is recorded to 2020 and EDShortListCode from 2013, alone
# from 2021, and the two can differ, so ED always searches all four fields.
# Older APC records are coded in ICD-9, and ED records with ICD10AMEdition 99
# are not ICD-10, so codes such as V56.0 would otherwise match the wrong
# conditions.
.dsr_datasets <- list(
  ED  = list(table = "ED",  date = "PresentationDateTime", age = "AgeYrs",
             label = "ED presentations",
             diagnoses = c("EDShortListCode", paste0("Diagnosis", 1:3)),
             principal = FALSE,
             icd10 = "COALESCE(ICD10AMEdition, 0) <> 99"),
  APC = list(table = "APC", date = "SeparationDate",       age = "AgeYears",
             label = "APC separations",
             diagnoses = paste0("Diagnosis", 1:100),
             principal = TRUE,
             icd10 = "ICDVersion = 10")
)

# Populations: the choice offered, the wording in descriptions and the ERP5
# tables used. The ACT and surrounds take their populations from the
# Australian tables.
.dsr_scopes <- list(
  ACT       = list(label = "ACT residents", describe = "ACT residents",
                   erp = "ACT"),
  SURROUNDS = list(label = "ACT and surrounds",
                   describe = "residents of the ACT and surrounds",
                   erp = "AUS"),
  AUS       = list(label = "Australian residents",
                   describe = "Australian residents", erp = "AUS")
)

# The NSW SA3s (ASGS 2021) that share a border with the ACT. With the ACT they
# make up the ACT and surrounds, at SA2 level too, as SA2 codes start with the
# code of their SA3. At state level they form one area with this label.
.dsr_surrounds <- c("10102" = "Queanbeyan", "10103" = "Snowy Mountains",
                    "10106" = "Young - Yass", "11302" = "Tumut - Tumbarumba")
.dsr_surrounds_label <- "NSW (surrounds)"

# Geographic levels: the geo_ column holding the area of residence and the
# level of the ERP5 population table. The ERP5 tables use ASGS 2021.
.dsr_levels <- list(
  state = list(column = "geo_STATE",    erp = "STE", label = "State or territory"),
  sa3   = list(column = "geo_SA3_2021", erp = "SA3", label = "SA3"),
  sa2   = list(column = "geo_SA2_2021", erp = "SA2", label = "SA2")
)

# geo_STATE abbreviations by ABS state code (EntityCode in ERP5_STE tables,
# where 0 is the Australian total)
.dsr_states <- c("1" = "NSW", "2" = "VIC", "3" = "QLD", "4" = "SA",
                 "5" = "WA",  "6" = "TAS", "7" = "NT",  "8" = "ACT",
                 "9" = "OT")


# Validated calculator settings. Every value interpolated into SQL passes
# through here: names are matched against the definitions above, numbers are
# coerced to integer and diagnosis codes are parsed by .dsr_parse_codes().
# `pool` pools the years into periods of that many years, which must divide
# the range evenly.
#' @noRd
.dsr_spec <- function(dataset = "ED", scope = "ACT", level = "state",
                      years, standard = 101L, codes = NULL,
                      diagnosis = "principal", pool = 1L) {

  dataset   <- match.arg(dataset, names(.dsr_datasets))
  scope     <- match.arg(scope, names(.dsr_scopes))
  level     <- match.arg(level, names(.dsr_levels))
  diagnosis <- match.arg(diagnosis, c("principal", "all"))
  if (!.dsr_datasets[[dataset]]$principal) diagnosis <- "all"
  if (is.data.frame(codes)) codes <- codes$Label
  codes <- .dsr_parse_codes(codes)

  years <- suppressWarnings(as.integer(years))
  if (length(years) == 1) years <- c(years, years)
  if (length(years) != 2 || anyNA(years) || years[1] > years[2] ||
      years[1] < 1900 || years[2] > 2999) {
    stop("'years' must be one calendar year or a range of two, earliest first.",
         call. = FALSE)
  }

  standard <- suppressWarnings(as.integer(standard))
  if (length(standard) != 1 || is.na(standard)) {
    stop("'standard' must be a StdPopCode.", call. = FALSE)
  }

  pool <- suppressWarnings(as.integer(pool))
  if (length(pool) != 1 || is.na(pool) || pool < 1) {
    stop("'pool' must be a whole number of years, 1 or more.", call. = FALSE)
  }
  n <- years[2] - years[1] + 1L
  if (n %% pool != 0) {
    stop(pool, "-year periods need a number of years that is a multiple of ",
         pool, ": ", years[1], " to ", years[2], " is ", n,
         if (n == 1) " year." else " years.", call. = FALSE)
  }

  list(dataset = dataset, scope = scope, level = level, years = years,
       standard = standard, codes = codes, diagnosis = diagnosis, pool = pool)

}


# Diagnosis criteria as typed, e.g. "J45", "C13-C15.45" or "E18.3 to E18.78",
# several to an entry when separated by commas or semicolons. A code may be
# partial and stands for every code that starts with it; a range runs from
# the start of its first code to the end of its last. Returns one row per
# code or range: Label (tidied for display) and From and To (upper case,
# without dots) for the SQL. Anything else is an error naming the entry, so
# only letters and digits reach the SQL.
#' @noRd
.dsr_parse_codes <- function(x) {

  items <- trimws(unlist(strsplit(as.character(x), "[,;\n]+")))
  items <- items[nzchar(items)]

  rows <- lapply(items, function(item) {
    # En and em dashes become hyphens, matched as UTF-8 bytes so that the
    # session's encoding and locale do not matter
    s <- item
    for (dash in c("\u2013", "\u2014")) {
      s <- gsub(dash, "-", s, fixed = TRUE, useBytes = TRUE)
    }
    s <- toupper(s)
    s <- gsub("\\s+TO\\s+", "-", s)
    # The trailing space keeps an empty part after a final hyphen ("C13-")
    parts <- trimws(strsplit(paste0(s, " "), "-", fixed = TRUE)[[1]])
    parts <- sub("\\.?[*%]+$", "", parts)    # trailing wildcards
    parts <- sub("\\.$", "", parts)
    codes <- gsub(".", "", parts, fixed = TRUE)
    # A letter, then the two digits of an ICD-10 category, then up to three
    # more characters; any of it may be left off the end
    if (length(parts) < 1 || length(parts) > 2 ||
        !all(grepl("^[A-Z]([0-9]([0-9]([0-9A-Z]{0,3})?)?)?$", codes))) {
      stop("'", item, "' is not an ICD-10 code or range of codes, such as ",
           "J45 or C13-C15.45.", call. = FALSE)
    }
    if (length(codes) == 2 && .dsr_code_cmp(codes[1], codes[2]) > 0) {
      stop("In '", item, "' the first code comes after the second.",
           call. = FALSE)
    }
    data.frame(Label = paste(parts, collapse = "-"),
               From = codes[1], To = codes[length(codes)],
               stringsAsFactors = FALSE)
  })

  out <- do.call(rbind, c(list(data.frame(Label = character(0),
                                          From = character(0),
                                          To = character(0))), rows))
  out <- out[!duplicated(out[c("From", "To")]), , drop = FALSE]
  rownames(out) <- NULL
  out

}


# Compares two codes character by character as SQL Server orders letters and
# digits: -1, 0 or 1. Independent of the R session's collation.
#' @noRd
.dsr_code_cmp <- function(a, b) {
  x <- utf8ToInt(a)
  y <- utf8ToInt(b)
  n <- min(length(x), length(y))
  d <- which(x[seq_len(n)] != y[seq_len(n)])
  if (length(d)) return(sign(x[d[1]] - y[d[1]]))
  sign(length(x) - length(y))
}


# Population table for the settings, e.g. ERP5_SA3_ACT. The states of the ACT
# and surrounds are summed from SA3s, as the surrounds are only part of NSW.
#' @noRd
.dsr_pop_table <- function(spec) {
  erp <- if (spec$scope == "SURROUNDS" && spec$level == "state") {
    "SA3"
  } else {
    .dsr_levels[[spec$level]]$erp
  }
  paste0("ERP5_", erp, "_", .dsr_scopes[[spec$scope]]$erp)
}


# Pooled years: the column that holds the time in the results, and the label
# of the period each year falls in, e.g. "2016-2018" for 3-year periods from
# 2016
#' @noRd
.dsr_time <- function(spec) if (spec$pool > 1) "Period" else "Year"

#' @noRd
.dsr_period <- function(year, first, pool) {
  start <- year - (year - first) %% pool
  paste0(start, "-", start + pool - 1L)
}

# Adds the Period column to fetched events or population when years are pooled
#' @noRd
.dsr_pool <- function(df, spec) {
  if (spec$pool > 1) df$Period <- .dsr_period(df$Year, spec$years[1], spec$pool)
  df
}


# Replaces {name} placeholders in an SQL template
#' @noRd
.dsr_fill <- function(template, ...) {
  values <- list(...)
  for (nm in names(values)) {
    template <- gsub(paste0("{", nm, "}"), values[[nm]], template, fixed = TRUE)
  }
  template
}


# ── SQL ─────────────────────────────────────────────────────────────────────

# Event counts by calendar year, sex, 5-year age group and area of residence
#' @noRd
.dsr_sql_events <- function(spec) {

  ds   <- .dsr_datasets[[spec$dataset]]
  area <- .dsr_sql_area(spec)

  .dsr_fill("SELECT Year, Sex, AgeGroup, Area, COUNT(*) AS Events
FROM (
    SELECT YEAR({date}) AS Year,
           Sex,
           CASE WHEN {age} >= 85 THEN 18
                WHEN {age} >= 0 THEN {age} / 5 + 1
           END AS AgeGroup,
           {area} AS Area
    FROM Analysis.dbo.{table}
    WHERE {date} >= '{from}0101'
      AND {date} < '{to}0101'{scope}{codes}
) AS e
GROUP BY Year, Sex, AgeGroup, Area
ORDER BY Year, Sex, AgeGroup, Area",
    date = ds$date, age = ds$age, area = area$area, table = ds$table,
    from = spec$years[1], to = spec$years[2] + 1L, scope = area$where,
    codes = .dsr_sql_codes(spec))

}


# Area of residence in the events query: the expression giving Area, and the
# condition keeping the areas of the population. For the ACT and the ACT and
# surrounds, events with no recorded residence are kept so that they can be
# counted and reported; dsr_calculate() leaves them out of the rates. ASGS
# codes start with the state code, 8 for the ACT, and SA2 codes start with
# their SA3 code. The states of the ACT and surrounds come from the SA3.
#' @noRd
.dsr_sql_area <- function(spec) {

  col  <- .dsr_levels[[spec$level]]$column
  keep <- function(col, cond) {
    paste0("\n      AND (", cond, " OR ", col, " IS NULL)")
  }

  if (spec$scope == "AUS") return(list(area = col, where = ""))

  if (spec$scope == "ACT") {
    in_act <- if (spec$level == "state") " = 'ACT'" else " LIKE '8%'"
    return(list(area = col, where = keep(col, paste0(col, in_act))))
  }

  if (spec$level == "state") col <- .dsr_levels$sa3$column
  sa3 <- if (spec$level == "sa2") paste0("LEFT(", col, ", 5)") else col
  nsw <- paste0(sa3, " IN (",
                paste0("'", names(.dsr_surrounds), "'", collapse = ", "), ")")
  area <- if (spec$level == "state") {
    paste0("CASE WHEN ", col, " LIKE '8%' THEN 'ACT'",
           "\n                WHEN ", nsw, " THEN '", .dsr_surrounds_label, "'",
           "\n           END")
  } else {
    col
  }
  list(area = area, where = keep(col, paste0(col, " LIKE '8%' OR ", nsw)))

}


# Condition keeping events with a diagnosis in the codes and ranges of the
# settings, in the principal diagnosis or in any diagnosis field (always any
# for ED, which has no principal diagnosis field). Codes are
# compared without dots or spaces. A range keeps codes from its first code up
# to its last, plus every code that starts with the last; a single code keeps
# every code that starts with it. Morphology codes (M8140/3) are skipped.
# Empty when there are no codes.
#' @noRd
.dsr_sql_codes <- function(spec) {

  codes <- spec$codes
  if (is.null(codes) || nrow(codes) == 0) return("")

  ds <- .dsr_datasets[[spec$dataset]]
  fields <- if (spec$diagnosis == "all") ds$diagnoses else ds$diagnoses[1]
  values <- vapply(split(paste0("(", fields, ")"),
                         ceiling(seq_along(fields) / 5)),
                   paste, character(1), collapse = ", ")

  tests <- ifelse(
    codes$From == codes$To,
    sprintf("c.Code LIKE '%s%%'", codes$From),
    sprintf("(c.Code >= '%s' AND (c.Code <= '%s' OR c.Code LIKE '%s%%'))",
            codes$From, codes$To, codes$To)
  )

  paste0(
    "\n      AND ", ds$icd10,
    "\n      AND EXISTS (",
    "\n          SELECT 1",
    "\n          FROM (VALUES ",
    paste(values, collapse = ",\n                       "), ") AS d (Code)",
    "\n          CROSS APPLY (VALUES (REPLACE(LTRIM(RTRIM(d.Code)), '.', ''))) AS c (Code)",
    "\n          WHERE c.Code NOT LIKE '%/%'",
    "\n            AND (", paste(tests, collapse = "\n                 OR "), ")",
    "\n      )"
  )

}


# Estimated resident population by year, sex, 5-year age group and area. The
# ACT and surrounds select their SA3s and SA2s from the Australian tables, and
# at state level sum the SA3s into the ACT and the NSW surrounds.
#' @noRd
.dsr_sql_population <- function(spec) {

  population <- "ERPCount"
  group      <- ""

  if (spec$scope == "SURROUNDS") {
    code   <- "CAST(EntityCode AS varchar(9))"
    sa3    <- if (spec$level == "sa2") paste0("LEFT(", code, ", 5)") else code
    nsw    <- paste0(sa3, " IN (",
                     paste0("'", names(.dsr_surrounds), "'", collapse = ", "),
                     ")")
    entity <- paste0("\n  AND (", code, " LIKE '8%' OR ", nsw, ")")
    if (spec$level == "state") {
      area <- paste0("CASE WHEN ", code, " LIKE '8%' THEN 'ACT' ELSE '",
                     .dsr_surrounds_label, "' END")
      population <- "SUM(ERPCount)"
      group <- paste0("\nGROUP BY ERPYear, SexCode, AgeGroup05Code, AgeGroup05Name,",
                      "\n         ", area)
    } else {
      area <- code
    }
  } else if (spec$level == "state") {
    whens <- sprintf("WHEN %s THEN %-5s", names(.dsr_states),
                     paste0("'", .dsr_states, "'"))
    rows  <- split(whens, ceiling(seq_along(whens) / 3))
    area  <- paste0(
      "CASE EntityCode ",
      paste(vapply(rows, function(r) sub("\\s+$", "", paste(r, collapse = " ")),
                   character(1)),
            collapse = "\n                       "),
      "\n       END")
    # 0 is the Australian total
    entity <- "\n  AND EntityCode BETWEEN 1 AND 9"
  } else {
    area   <- "CAST(EntityCode AS varchar(9))"
    entity <- ""
  }

  .dsr_fill("SELECT ERPYear AS Year,
       SexCode AS Sex,
       AgeGroup05Code AS AgeGroup,
       AgeGroup05Name AS AgeGroupName,
       {area} AS Area,
       {population} AS Population
FROM Analysis.dbo.{table}
WHERE ERPYear BETWEEN {from} AND {to}{entity}{group}
ORDER BY Year, Sex, AgeGroup, Area",
    area = area, population = population, table = .dsr_pop_table(spec),
    from = spec$years[1], to = spec$years[2], entity = entity, group = group)

}


# Standard population by 5-year age group
#' @noRd
.dsr_sql_standard <- function(spec) {

  .dsr_fill("SELECT AgeGroupCode AS AgeGroup,
       AgeGroupName,
       StdPopValue
FROM Analysis.dbo.StandardPops
WHERE StdPopCode = {code}
  AND AgeGroupTypeName = 'AgeGroup05Code'
ORDER BY AgeGroupCode",
    code = spec$standard)

}


# Standard populations with 5-year age groups, for the selector
#' @noRd
.dsr_sql_standards <- function() {
  "SELECT StdPopCode, StdPopName, MAX(Note) AS Note
FROM Analysis.dbo.StandardPops
WHERE AgeGroupTypeName = 'AgeGroup05Code'
GROUP BY StdPopCode, StdPopName
ORDER BY StdPopCode"
}


# First and last event dates of a dataset
#' @noRd
.dsr_sql_dates <- function(dataset) {
  ds <- .dsr_datasets[[match.arg(dataset, names(.dsr_datasets))]]
  .dsr_fill("SELECT MIN({date}) AS FirstDate, MAX({date}) AS LastDate
FROM Analysis.dbo.{table}", date = ds$date, table = ds$table)
}


# First and last years of a population table
#' @noRd
.dsr_sql_erp_years <- function(spec) {
  .dsr_fill("SELECT MIN(ERPYear) AS FirstYear, MAX(ERPYear) AS LastYear
FROM Analysis.dbo.{table}", table = .dsr_pop_table(spec))
}


# ── Years ───────────────────────────────────────────────────────────────────

# Years the calculator offers: those covered by both the events (first_date
# to last_date) and the population table (erp_years). A year is complete when
# the events run from 1 January to 31 December; the default is the latest
# complete year. Unknown dates (NA) leave the population years alone.
# Returns NULL when there is no overlap.
#' @noRd
.dsr_year_range <- function(first_date, last_date, erp_years) {

  erp_years <- as.integer(erp_years)
  if (length(erp_years) != 2 || anyNA(erp_years)) return(NULL)

  # Dates come back as Date or POSIXct; formatting first keeps the calendar
  # date the server recorded, whatever the time zone attribute
  as_day <- function(x) {
    if (length(x) == 0 || is.na(x[1])) return(as.Date(NA))
    as.Date(format(x[1], "%Y-%m-%d"))
  }
  first_date <- as_day(first_date)
  last_date  <- as_day(last_date)
  year <- function(d) as.integer(format(d, "%Y"))

  if (is.na(first_date) || is.na(last_date)) {
    complete <- erp_years
    events   <- erp_years
  } else {
    events   <- c(year(first_date), year(last_date))
    complete <- c(
      if (format(first_date, "%m-%d") == "01-01") events[1] else events[1] + 1L,
      if (format(last_date, "%m-%d") == "12-31") events[2] else events[2] - 1L
    )
  }

  lo <- max(events[1], erp_years[1])
  hi <- min(events[2], erp_years[2])
  if (lo > hi) return(NULL)

  default <- min(max(complete[2], lo), hi)

  list(min = lo, max = hi, default = default, complete = complete,
       first_date = first_date, last_date = last_date)

}


# Years in the range that the event data only partly cover
#' @noRd
.dsr_partial_years <- function(years, range) {
  if (is.null(range)) return(integer(0))
  y <- seq(years[1], years[2])
  y[y < range$complete[1] | y > range$complete[2]]
}


# ── Descriptions and code ───────────────────────────────────────────────────

#' @noRd
.dsr_describe <- function(spec, standard_name = NULL, years = TRUE) {
  n <- spec$years[2] - spec$years[1] + 1L
  years <- if (!years) {
    NULL
  } else if (n == 1) {
    paste0(", ", spec$years[1])
  } else {
    paste0(", ", spec$years[1], " to ", spec$years[2],
           if (spec$pool == n) {
             " pooled"
           } else if (spec$pool > 1) {
             paste0(" in ", spec$pool, "-year periods")
           })
  }
  level <- .dsr_levels[[spec$level]]$label
  if (spec$level == "state") level <- tolower(level)
  codes <- spec$codes$Label
  diagnosis <- if (length(codes)) {
    paste0(", ", if (spec$diagnosis == "all") "any" else "principal",
           " diagnosis ",
           if (length(codes) > 1) {
             paste(paste(codes[-length(codes)], collapse = ", "), "or",
                   codes[length(codes)])
           } else {
             codes
           })
  }
  paste0(
    .dsr_datasets[[spec$dataset]]$label, diagnosis, ", ",
    .dsr_scopes[[spec$scope]]$describe,
    ", by ", level, years,
    if (!is.null(standard_name)) paste0(", standardised to ", standard_name)
  )
}


# R code that reproduces the calculator's results
#' @noRd
.dsr_code <- function(spec, by_sex = FALSE, age_specific = FALSE,
                      multiplier = 100000, standard_name = NULL,
                      suppress = 0) {

  args <- paste0(
    "  by = c(\"", .dsr_time(spec), "\", \"Area\"), by_sex = ",
    if (by_sex) "TRUE" else "FALSE",
    ", multiplier = ", format(multiplier, scientific = FALSE),
    if (suppress > 0) paste0(", suppress = ", format(suppress, scientific = FALSE)),
    "\n"
  )

  # Same periods as .dsr_period()
  pooling <- if (spec$pool > 1) {
    paste0(
      "# Pool the years into ", spec$pool, "-year periods from ",
      spec$years[1], "\n",
      "period <- function(year) {\n",
      "  start <- year - (year - ", spec$years[1], ") %% ", spec$pool, "\n",
      "  paste0(start, \"-\", start + ", spec$pool - 1L, ")\n",
      "}\n",
      "events$Period     <- period(events$Year)\n",
      "population$Period <- period(population$Year)\n\n"
    )
  }

  paste0(
    "# DSR Calculator: ", .dsr_describe(spec, standard_name), "\n",
    "conn <- actepir::episerver_connect()\n\n",
    "# Events by calendar year, sex, 5-year age group and area of residence\n",
    "events <- DBI::dbGetQuery(conn, \"\n", .dsr_sql_events(spec), "\n\")\n\n",
    "# Estimated resident population\n",
    "population <- DBI::dbGetQuery(conn, \"\n", .dsr_sql_population(spec), "\n\")\n\n",
    "# Standard population\n",
    "standard <- DBI::dbGetQuery(conn, \"\n", .dsr_sql_standard(spec), "\n\")\n\n",
    "DBI::dbDisconnect(conn)\n\n",
    pooling,
    "rates <- actepir::dsr_calculate(\n",
    "  events, population, standard,\n",
    args,
    ")\n",
    if (age_specific) {
      paste0(
        "\nage_rates <- actepir::dsr_age_specific(\n",
        "  events, population,\n",
        args,
        ")\n"
      )
    }
  )

}


# ── Display ─────────────────────────────────────────────────────────────────

# Wording for the reasons dsr_calculate() gives for leaving events out
#' @noRd
.dsr_reasons <- function(reason) {
  lookup <- c(
    "Missing AgeGroup"       = "age not recorded",
    "Missing Area"           = "residence not recorded",
    "No matching population" = "area not in the population table"
  )
  ifelse(reason %in% names(lookup), lookup[reason], tolower(reason))
}


# DataTable of calculator results with grouped rate headers. Suppressed
# counts show as "<suppress" and sort below zero.
#' @noRd
.dsr_datatable <- function(df, area_label, multiplier, age = FALSE,
                           suppress = 0) {

  time    <- if ("Period" %in% names(df)) "Period" else "Year"
  id_cols <- c(time, "Area", "Sex", if (age) "AgeGroupName")
  if (age) {
    rate_cols  <- c("Rate", "RateLower", "RateUpper")
    rate_heads <- list("Age-specific rate (95% CI)")
  } else {
    rate_cols  <- c("Crude", "CrudeLower", "CrudeUpper",
                    "DSR", "DSRLower", "DSRUpper")
    rate_heads <- list("Crude rate (95% CI)", "Age-standardised rate (95% CI)")
  }
  df <- df[, c(id_cols, "Events", "Population", rate_cols)]

  heads <- c(Year = "Year", Period = "Period", Area = area_label, Sex = "Sex",
             AgeGroupName = "Age group")[id_cols]
  sketch <- htmltools::tags$table(
    htmltools::tags$thead(
      htmltools::tags$tr(
        lapply(c(heads, "Events", "Population"),
               function(h) htmltools::tags$th(rowspan = 2, h)),
        lapply(rate_heads,
               function(h) htmltools::tags$th(colspan = 3, h,
                                               style = "text-align: center;"))
      ),
      htmltools::tags$tr(
        lapply(rep(c("Rate", "Lower", "Upper"), length(rate_heads)),
               htmltools::tags$th)
      )
    )
  )

  long <- nrow(df) > 100
  digits <- if (multiplier >= 100000) 1 else 2
  events <- DT::JS(sprintf(
    "function(data, type) {
       if (data === null) return type === 'display' ? '&lt;%s' : -1;
       return type === 'display' ?
         DTWidget.formatRound(data, 0, 3, ',', '.', null) : data;
     }", .fmt_count(suppress)))

  tbl <- DT::datatable(
    df,
    container = sketch,
    rownames  = FALSE,
    selection = "none",
    class     = "compact stripe hover",
    style     = "bootstrap",
    options   = list(
      dom        = if (long) "ftip" else "ft",
      paging     = long,
      pageLength = 100,
      scrollX    = TRUE,
      autoWidth  = FALSE,
      columnDefs = list(list(targets = match("Events", names(df)) - 1L,
                             render = events))
    )
  )
  tbl <- DT::formatRound(tbl, "Population", digits = 0, mark = ",")
  DT::formatRound(tbl, rate_cols, digits = digits, mark = ",")

}


# Calculator results as tab-separated text for pasting into a spreadsheet,
# with the column headings of the table and its rounding. Suppressed counts
# are written as "<suppress".
#' @noRd
.dsr_tsv <- function(df, area_label, multiplier, age = FALSE, suppress = 0) {

  time    <- if ("Period" %in% names(df)) "Period" else "Year"
  id_cols <- c(time, "Area", "Sex", if (age) "AgeGroupName")
  heads   <- c(Year = "Year", Period = "Period", Area = area_label, Sex = "Sex",
               AgeGroupName = "Age group")[id_cols]
  rates <- if (age) {
    c(Rate = "Age-specific rate", RateLower = "Lower 95% CI",
      RateUpper = "Upper 95% CI")
  } else {
    c(Crude = "Crude rate", CrudeLower = "Crude lower 95% CI",
      CrudeUpper = "Crude upper 95% CI", DSR = "Age-standardised rate",
      DSRLower = "DSR lower 95% CI", DSRUpper = "DSR upper 95% CI")
  }
  digits <- if (multiplier >= 100000) 1 else 2

  # Excel reads age groups such as 05-09 as dates; an en dash keeps them,
  # and periods such as 2016-2018, as text
  out <- lapply(id_cols, function(col) {
    x <- as.character(df[[col]])
    if (col %in% c("AgeGroupName", "Period")) {
      x <- gsub("-", "\u2013", x, fixed = TRUE)
    }
    x
  })
  out <- c(out, list(
    ifelse(is.na(df$Events), paste0("<", .fmt_count(suppress)),
           format(df$Events, scientific = FALSE, trim = TRUE)),
    format(df$Population, scientific = FALSE, trim = TRUE)
  ))
  out <- c(out, lapply(names(rates), function(col) {
    ifelse(is.na(df[[col]]), "",
           formatC(df[[col]], format = "f", digits = digits))
  }))

  lines <- c(paste(c(heads, "Events", "Population", rates), collapse = "\t"),
             do.call(paste, c(out, sep = "\t")))
  paste0(paste(lines, collapse = "\n"), "\n")

}


# ── App ─────────────────────────────────────────────────────────────────────

# Internal factory that builds the Shiny app. Runs in the current session for
# foreground mode, or inside the callr background process for background mode.
# The database connection is created here and closed when the app stops.
# `query` replaces the EpiServer connection with any function that takes SQL
# and returns a data frame; tests use it to run the app offline.
#' @noRd
episerver_dsr_app <- function(driver = NULL, max_attempts = NULL, ...,
                              query = NULL) {

  # ── Check dependencies ────────────────────────────────────────────────────
  if (!requireNamespace("shiny", quietly = TRUE) ||
      !requireNamespace("miniUI", quietly = TRUE) ||
      !requireNamespace("DT", quietly = TRUE)) {
    stop(
      "The 'shiny', 'miniUI', and 'DT' packages are required for episerver_dsr().\n",
      "Install them with: install.packages(c('shiny', 'miniUI', 'DT'))",
      call. = FALSE
    )
  }

  # Foreground (RStudio session) or background process?
  in_rstudio <- tryCatch(rstudioapi::isAvailable(), error = function(e) FALSE)

  # ── Establish connection ──────────────────────────────────────────────────
  db <- NULL
  if (is.null(query)) {
    connect_args <- list()
    if (!is.null(driver)) connect_args$driver <- driver
    if (!is.null(max_attempts)) connect_args$max_attempts <- max_attempts
    db <- .epi_connection(c(connect_args, list(...)))
    query <- db$query
  }

  # Lookups that do not change while the app runs (standards, event date
  # ranges, population years) are cached once they succeed
  cache <- new.env(parent = emptyenv())
  cached <- function(key, sql) {
    if (is.null(cache[[key]])) cache[[key]] <- query(sql)
    cache[[key]]
  }
  try_cached <- function(key, sql) {
    tryCatch(cached(key, sql), error = function(e) NULL)
  }

  year_range <- function(dataset, scope, level) {
    spec  <- .dsr_spec(dataset, scope, level, years = 2000L)
    dates <- try_cached(paste0("dates_", dataset), .dsr_sql_dates(dataset))
    erp   <- try_cached(paste0("erp_", .dsr_pop_table(spec)),
                        .dsr_sql_erp_years(spec))
    if (is.null(erp) || nrow(erp) == 0) return(NULL)
    have_dates <- !is.null(dates) && nrow(dates) > 0
    .dsr_year_range(
      if (have_dates) dates$FirstDate[1] else NA,
      if (have_dates) dates$LastDate[1] else NA,
      c(erp$FirstYear[1], erp$LastYear[1])
    )
  }

  standards <- try_cached("standards", .dsr_sql_standards())
  if (is.null(standards) || nrow(standards) == 0) {
    standards <- data.frame(StdPopCode = 101L, StdPopName = "Australia (2001)",
                            Note = NA_character_)
  }
  std_choices <- stats::setNames(
    as.character(standards$StdPopCode),
    ifelse(!is.na(standards$Note) & grepl("^Supers?ceded$", standards$Note),
           paste(standards$StdPopName, "(superseded)"), standards$StdPopName)
  )
  std_name <- function(code) {
    names(std_choices)[match(as.character(code), std_choices)]
  }

  # Initial year range for the default settings; the slider follows the
  # settings once the app is running
  this_year <- as.integer(format(Sys.Date(), "%Y"))
  init <- year_range("ED", "ACT", "state")
  if (is.null(init)) {
    init <- list(min = this_year - 10L, max = this_year, default = this_year - 1L)
  }

  code_button_label <- if (in_rstudio) "Insert Code" else "Copy Code"

  # ── UI ────────────────────────────────────────────────────────────────────
  ui <- miniUI::miniPage(

    miniUI::gadgetTitleBar(
      "DSR Calculator",
      left  = NULL,
      right = miniUI::miniTitleBarButton("done", "Done", primary = TRUE)
    ),

    miniUI::miniContentPanel(

      .epi_app_style(),

      # Calculator-only rules: wrapping selector rows for the Viewer pane,
      # slider spacing, notes, the error bar, table headings, tabs and the
      # diagnosis code tags
      shiny::tags$style(shiny::HTML("
        .dsr-row { flex-wrap: wrap; }
        .dsr-row > * { min-width: 200px; }
        .selector-row .irs { margin-top: -4px; }
        .info-bar p { margin: 0 0 4px 0; }
        .info-bar p:last-child { margin-bottom: 0; }
        .info-bar .epi-warn { color: #8a5300; }
        .info-bar.epi-error { border-left-color: #b00020; color: #b00020; }
        .epi-note { font-size: 11px; color: #777; margin: -4px 0 10px 0; }
        .epi-note.epi-error { color: #b00020; }
        .epi-table-head { display: flex; align-items: center;
                          justify-content: space-between; margin: 4px 0 6px 0; }
        .epi-table-head h5 { color: var(--epi-primary); font-weight: 600;
                             margin: 0; }
        .tab-pane .epi-table-head + div { margin-bottom: 14px; }
        .nav-tabs { margin: 6px 0 12px 0; }
        .nav-tabs > li > a { color: var(--epi-accent); }
        .nav-tabs > li > a:hover { background-color: var(--epi-info-bg); }
        .nav-tabs > li.active > a, .nav-tabs > li.active > a:hover,
        .nav-tabs > li.active > a:focus { color: var(--epi-primary);
                                          font-weight: 600; }
        .selectize-control.multi .selectize-input > div {
          background: var(--epi-info-bg); color: var(--epi-primary);
          border-radius: 3px;
        }
        .map-click { font-size: 12px; color: #333; min-height: 1.6em;
                     margin-top: 4px; }
        .radio.disabled label { color: #999; }
      ")),

      # Clipboard handler for the code and table copy buttons
      .epi_copy_script(),

      # A dataset without a principal diagnosis field (ED) searches every
      # field: the principal option is disabled for it, and the last choice
      # made for the other dataset comes back with that dataset
      shiny::tags$script(shiny::HTML(sprintf("
        (function() {
          var none = %s, chosen = 'principal';
          function sync() {
            var principal = $('input[name=diagnosis][value=principal]');
            var off = none.indexOf($('#dataset').val()) >= 0;
            if (!principal.length || off === principal.prop('disabled')) return;
            if (off) {
              chosen = $('input[name=diagnosis]:checked').val() || chosen;
              $('input[name=diagnosis][value=all]').prop('checked', true)
                .trigger('change');
            } else {
              $('input[name=diagnosis][value=' + chosen + ']')
                .prop('checked', true).trigger('change');
            }
            principal.prop('disabled', off).closest('.radio')
              .toggleClass('disabled', off);
          }
          $(document).on('change', '#dataset', sync);
          $(sync);
        })();
      ", jsonlite::toJSON(names(Filter(function(d) !d$principal, .dsr_datasets)))))),

      # Copy map: the plot is a PNG data URI, decoded and written to the
      # clipboard as an image within the click, where the browser allows it
      shiny::tags$script(shiny::HTML("
        $(document).on('click', '#copy_map', function() {
          var report = function(ok, msg) {
            Shiny.setInputValue('copy_map_result',
                                {ok: ok, msg: msg, t: Date.now()});
          };
          var img = document.querySelector('#map img');
          var m = img && /^data:([^;]+);base64,(.*)$/.exec(img.src);
          if (!m) { report(false, 'There is no map to copy yet.'); return; }
          if (!(navigator.clipboard && navigator.clipboard.write &&
                window.ClipboardItem)) {
            report(false, 'This window cannot copy images. ' +
                          'Use the PNG or PDF download instead.');
            return;
          }
          var bin = atob(m[2]);
          var bytes = new Uint8Array(bin.length);
          for (var i = 0; i < bin.length; i++) bytes[i] = bin.charCodeAt(i);
          var item = {};
          item[m[1]] = new Blob([bytes], {type: m[1]});
          navigator.clipboard.write([new ClipboardItem(item)])
            .then(function() { report(true, 'Map copied to the clipboard.'); })
            .catch(function(e) {
              report(false, 'The map could not be copied (' + e.message +
                            '). Use the PNG or PDF download instead.');
            });
        });
      ")),

      shiny::div(
        class = "selector-row dsr-row",
        shiny::selectInput(
          "dataset", "Dataset",
          choices = c("ED presentations" = "ED", "APC separations" = "APC"),
          width = "100%"
        ),
        shiny::selectInput(
          "scope", "Population",
          choices = stats::setNames(
            names(.dsr_scopes),
            vapply(.dsr_scopes, `[[`, character(1), "label")
          ),
          width = "100%"
        ),
        shiny::selectInput(
          "level", "Geographic level",
          choices = stats::setNames(
            names(.dsr_levels),
            vapply(.dsr_levels, `[[`, character(1), "label")
          ),
          width = "100%"
        ),
        shiny::selectInput(
          "standard", "Standard population",
          choices  = std_choices,
          selected = if ("101" %in% std_choices) "101" else std_choices[1],
          width = "100%"
        )
      ),

      shiny::div(
        class = "selector-row dsr-row",
        shiny::div(
          style = "flex: 3 1 0;",
          shiny::sliderInput(
            "years", "Calendar years",
            min = init$min, max = init$max,
            value = c(init$default, init$default),
            step = 1, sep = "", ticks = FALSE, width = "100%"
          )
        ),
        shiny::div(
          style = "flex: 1 1 0;",
          shiny::selectInput(
            "pool", "Pool years",
            choices = c("None" = "1", "2 years" = "2", "3 years" = "3",
                        "4 years" = "4", "5 years" = "5",
                        "All selected years" = "all"),
            width = "100%"
          )
        ),
        shiny::div(
          style = "flex: 1 1 0;",
          shiny::selectInput(
            "multiplier", "Rate per",
            choices  = c("1,000" = "1000", "10,000" = "10000",
                         "100,000" = "100000"),
            selected = "100000",
            width = "100%"
          )
        ),
        shiny::div(
          style = "flex: 1 1 0;",
          shiny::numericInput(
            "suppress", "Suppress counts below",
            value = 5, min = 0, step = 1, width = "100%"
          )
        )
      ),
      shiny::uiOutput("years_note"),

      shiny::div(
        class = "selector-row dsr-row",
        shiny::div(
          style = "flex: 3 1 0;",
          shiny::selectizeInput(
            "codes", "Diagnosis codes",
            choices = NULL, multiple = TRUE, width = "100%",
            options = list(
              create       = TRUE,
              createOnBlur = TRUE,
              persist      = FALSE,
              delimiter    = ",",
              splitOn      = I("/\\s*[,;]+\\s*/"),
              plugins      = list("remove_button"),
              placeholder  = paste("All events. Type a code or range,",
                                   "e.g. J45 or C13-C15.45, then Enter")
            )
          )
        ),
        shiny::div(
          style = "flex: 1 1 0;",
          shiny::radioButtons(
            "diagnosis", "Search",
            choices = c("Principal diagnosis" = "principal",
                        "Any diagnosis" = "all")
          )
        )
      ),
      shiny::uiOutput("codes_note"),

      # Info bar: what is shown, what was left out, and any warnings
      shiny::uiOutput("info_bar"),

      # Options row
      shiny::div(
        class = "options-row",
        shiny::actionButton(
          "calculate", "Calculate",
          icon  = shiny::icon("calculator"),
          class = "btn-sm btn-primary"
        ),
        shiny::actionButton(
          "insert_code", code_button_label,
          icon  = shiny::icon("terminal"),
          class = "btn-sm btn-primary"
        ),
        # Spacer pushes the display options to the right of the row
        shiny::div(style = "flex: 1 1 auto;"),
        shiny::checkboxInput("by_sex", "Male and female", value = FALSE,
                             width = "auto"),
        shiny::checkboxInput("age_specific", "Age-specific rates", value = FALSE,
                             width = "auto")
      ),

      shiny::tabsetPanel(
        id = "view",

        shiny::tabPanel(
          "Tables", value = "tables",
          shiny::div(
            class = "epi-table-head",
            shiny::h5("Rates"),
            shiny::actionButton("copy_summary", "Copy table",
                                icon = shiny::icon("copy"),
                                class = "btn-sm btn-primary")
          ),
          DT::dataTableOutput("summary_table"),
          shiny::conditionalPanel(
            "input.age_specific",
            shiny::div(
              class = "epi-table-head",
              shiny::h5("Age-specific rates"),
              shiny::actionButton("copy_age", "Copy table",
                                  icon = shiny::icon("copy"),
                                  class = "btn-sm btn-primary")
            ),
            DT::dataTableOutput("age_table")
          ),
          shiny::uiOutput("footnote")
        ),

        shiny::tabPanel(
          "Map", value = "map",
          shiny::div(
            class = "selector-row dsr-row",
            shiny::selectInput(
              "map_value", "Show",
              choices = c("Age-standardised rate" = "DSR",
                          "Crude rate" = "Crude", "Events" = "Events"),
              width = "100%"
            ),
            shiny::selectInput("map_year", "Year", choices = NULL,
                               width = "100%"),
            shiny::selectInput("map_sex", "Sex", choices = "Persons",
                               width = "100%"),
            shiny::uiOutput("map_extent_ui")
          ),
          shiny::div(
            class = "options-row",
            shiny::actionButton("copy_map", "Copy map",
                                icon = shiny::icon("copy"),
                                class = "btn-sm btn-primary"),
            shiny::downloadButton("map_png", "PNG",
                                  class = "btn-sm btn-primary"),
            shiny::downloadButton("map_pdf", "PDF",
                                  class = "btn-sm btn-primary")
          ),
          shiny::uiOutput("map_note"),
          shiny::plotOutput("map", height = "560px", click = "map_click"),
          shiny::uiOutput("map_click_info")
        )
      )

    )
  )

  # ── Server ────────────────────────────────────────────────────────────────
  server <- function(input, output, session) {

    rv <- shiny::reactiveValues(fetched = NULL, error = NULL)

    # Why the diagnosis codes cannot be read, or NULL
    codes_error <- shiny::reactive({
      tryCatch({
        .dsr_parse_codes(input$codes)
        NULL
      }, error = function(e) conditionMessage(e))
    })

    # Years per period: "All selected years" pools the whole range
    pool <- shiny::reactive({
      if (is.null(input$pool)) return(1L)
      if (input$pool == "all") return(as.integer(diff(input$years)) + 1L)
      as.integer(input$pool)
    })

    # Settings as entered, and why they cannot be used if they cannot
    settings <- shiny::reactive({
      shiny::req(input$dataset, input$scope, input$level, input$standard,
                 input$years)
      tryCatch(
        list(spec = .dsr_spec(input$dataset, input$scope, input$level,
                              input$years, input$standard, codes = input$codes,
                              diagnosis = if (is.null(input$diagnosis)) {
                                "principal"
                              } else {
                                input$diagnosis
                              },
                              pool = pool()),
             error = NULL),
        error = function(e) list(spec = NULL, error = conditionMessage(e))
      )
    })
    spec <- shiny::reactive(settings()$spec)

    # A cleared or negative threshold means no suppression, which the info
    # bar then states. Counts are whole numbers, so a fractional threshold is
    # rounded up to the equivalent whole one.
    suppress <- shiny::reactive({
      s <- input$suppress
      if (is.null(s) || is.na(s) || s < 0) 0 else ceiling(s)
    })

    available <- shiny::reactive({
      shiny::req(input$dataset, input$scope, input$level)
      year_range(input$dataset, input$scope, input$level)
    })

    # Keep the slider within the years available for the settings
    shiny::observeEvent(available(), {
      r <- available()
      shiny::req(r)
      sel <- input$years
      if (is.null(sel) || sel[2] < r$min || sel[1] > r$max) {
        sel <- c(r$default, r$default)
      }
      sel <- pmin(pmax(sel, r$min), r$max)
      shiny::updateSliderInput(session, "years",
                               min = r$min, max = r$max, value = sel)
    }, ignoreNULL = FALSE)

    output$codes_note <- shiny::renderUI({
      err <- codes_error()
      if (!is.null(err)) {
        return(shiny::div(class = "epi-note epi-error", err))
      }
      dataset <- if (is.null(input$dataset)) "ED" else input$dataset
      ds <- .dsr_datasets[[dataset]]
      shiny::div(
        class = "epi-note",
        "Leave empty for all events. A partial code such as J45 matches ",
        "every code that starts with it, and a range such as C13-C15.45 ",
        "includes both ends. Only ICD-10 coded records are searched.",
        if (!ds$principal) {
          paste0(" ", dataset, " searches all its diagnosis fields (",
                 paste(ds$diagnoses, collapse = ", "), "), as none of them ",
                 "holds the principal diagnosis in every year.")
        }
      )
    })

    # The periods the years will be pooled into, or why they cannot be
    output$years_note <- shiny::renderUI({
      shiny::req(input$years, pool() > 1)
      err <- tryCatch({
        .dsr_spec(years = input$years, pool = pool())
        NULL
      }, error = function(e) conditionMessage(e))
      if (!is.null(err)) {
        return(shiny::div(class = "epi-note epi-error", err))
      }
      years   <- as.integer(input$years)
      periods <- unique(.dsr_period(seq(years[1], years[2]), years[1], pool()))
      shiny::div(class = "epi-note", paste0(
        if (length(periods) == 1) "One period: " else
          paste0(length(periods), " periods: "),
        paste(periods, collapse = ", "), "."
      ))
    })

    # ── Calculate: run the events and population queries ──────────────────
    shiny::observeEvent(input$calculate, {
      st <- settings()
      s  <- st$spec
      if (is.null(s)) {
        shiny::showNotification(st$error, duration = 6, type = "warning")
        return(invisible(NULL))
      }
      rv$error <- NULL
      # Settings that leave both queries unchanged, such as pooling, reuse the
      # counts already fetched
      f <- rv$fetched
      if (!is.null(f) &&
          identical(.dsr_sql_events(s), .dsr_sql_events(f$spec)) &&
          identical(.dsr_sql_population(s), .dsr_sql_population(f$spec))) {
        f$spec <- s
        rv$fetched <- f
        return(invisible(NULL))
      }
      fetched <- tryCatch(
        shiny::withProgress(message = "Querying EpiServer", value = 0, {
          shiny::incProgress(0.1, detail = "events")
          events <- query(.dsr_sql_events(s))
          shiny::incProgress(0.6, detail = "population")
          population <- query(.dsr_sql_population(s))
          shiny::incProgress(0.3)
          list(spec = s, events = events, population = population)
        }),
        error = function(e) {
          rv$error <- conditionMessage(e)
          NULL
        }
      )
      rv$fetched <- fetched
    })

    # ── Rates: recalculated from the fetched counts when the standard, sex,
    # multiplier or suppression changes, without querying the events again ─
    results <- shiny::reactive({
      f <- rv$fetched
      shiny::req(f, input$standard, input$multiplier)
      s <- f$spec
      s$standard <- as.integer(input$standard)
      tryCatch({
        standard <- cached(paste0("standard_", s$standard), .dsr_sql_standard(s))
        if (is.null(standard) || nrow(standard) == 0) {
          stop("Standard population ", s$standard, " has no 5-year age groups.",
               call. = FALSE)
        }
        mult       <- as.numeric(input$multiplier)
        supp       <- suppress()
        events     <- .dsr_pool(f$events, s)
        population <- .dsr_pool(f$population, s)
        by         <- c(.dsr_time(s), "Area")
        summary <- suppressMessages(dsr_calculate(
          events, population, standard,
          by = by, by_sex = isTRUE(input$by_sex),
          multiplier = mult, suppress = supp
        ))
        list(spec = s, summary = summary, events = events,
             population = population, by = by, multiplier = mult,
             suppress = supp, total = sum(f$events$Events), error = NULL)
      }, error = function(e) list(error = conditionMessage(e)))
    })

    # Age-specific rates, calculated only while their table is shown
    age_results <- shiny::reactive({
      r <- results()
      shiny::req(r, is.null(r$error))
      suppressMessages(dsr_age_specific(
        r$events, r$population,
        by = r$by, by_sex = isTRUE(input$by_sex),
        multiplier = r$multiplier, suppress = r$suppress
      ))
    })

    # ── Info bar ───────────────────────────────────────────────────────────
    output$info_bar <- shiny::renderUI({
      if (!is.null(rv$error)) {
        return(shiny::div(class = "info-bar epi-error",
                          shiny::p("EpiServer query failed: ", rv$error)))
      }
      if (is.null(rv$fetched)) {
        return(shiny::div(class = "info-bar", shiny::p(
          "Choose the settings and press Calculate. The events are counted ",
          "on EpiServer, which can take a little while for large ranges."
        )))
      }
      r <- results()
      if (!is.null(r$error)) {
        return(shiny::div(class = "info-bar epi-error",
                          shiny::p("Could not calculate the rates: ", r$error)))
      }

      s  <- r$spec
      ex <- attr(r$summary, "excluded")
      notes <- list(shiny::p(
        shiny::strong(.dsr_describe(s, std_name(s$standard)), .noWS = "after"),
        paste0(". Rates per ", .fmt_count(r$multiplier), ".")
      ))

      if (nrow(ex)) {
        pct <- if (r$total > 0) sprintf(" (%.1f%%)", 100 * ex$Events / r$total) else ""
        notes <- c(notes, list(shiny::p(paste0(
          "Not in the rates: ",
          paste0(.dsr_reasons(ex$Reason), " ", .fmt_count(ex$Events), pct,
                 collapse = "; "), "."
        ))))
      }

      notes <- c(notes, list(shiny::p(
        if (r$suppress > 0) {
          paste0("Rows with fewer than ", .fmt_count(r$suppress),
                 " events are suppressed: ", sum(r$summary$Suppressed),
                 " of ", nrow(r$summary), " rows.")
        } else {
          "No rows are suppressed."
        }
      )))

      if (nrow(s$codes)) {
        notes <- c(notes, list(shiny::p(
          "Diagnosis search: ICD-10 coded records only; events with no ",
          if (s$diagnosis == "all") "diagnosis" else "principal diagnosis",
          " recorded cannot match."
        )))
      }

      rr <- year_range(s$dataset, s$scope, s$level)
      partial <- .dsr_partial_years(s$years, rr)
      if (length(partial)) {
        day <- function(d) sub("^0", "", format(d, "%d %B %Y"))
        notes <- c(notes, list(shiny::p(class = "epi-warn", paste0(
          "The ", s$dataset, " data run from ", day(rr$first_date), " to ",
          day(rr$last_date), ", so ", paste(partial, collapse = ", "),
          if (length(partial) == 1) " is" else " are",
          " only partly covered and the rates are understated."
        ))))
      }

      n_na <- sum(is.na(r$summary$DSR) & !r$summary$Suppressed)
      if (n_na) {
        notes <- c(notes, list(shiny::p(class = "epi-warn", paste0(
          "The DSR could not be calculated for ", n_na, " row(s) with events ",
          "in an age group that has no population."
        ))))
      }

      now <- spec()
      keys <- c("dataset", "scope", "level", "years", "codes", "diagnosis",
                "pool")
      if (!is.null(now) && !identical(now[keys], s[keys])) {
        notes <- c(notes, list(shiny::p(class = "epi-warn",
          "The settings have changed: press Calculate to update the results."
        )))
      }

      shiny::div(class = "info-bar", notes)
    })

    # ── Tables ─────────────────────────────────────────────────────────────
    area_label <- function(s) .dsr_levels[[s$level]]$label

    output$summary_table <- DT::renderDataTable({
      r <- results()
      shiny::req(r, is.null(r$error))
      .dsr_datatable(r$summary, area_label(r$spec), r$multiplier,
                     suppress = r$suppress)
    })

    output$age_table <- DT::renderDataTable({
      r <- results()
      shiny::req(r, is.null(r$error), isTRUE(input$age_specific))
      .dsr_datatable(age_results(), area_label(r$spec), r$multiplier,
                     age = TRUE, suppress = r$suppress)
    })

    output$footnote <- shiny::renderUI({
      r <- results()
      shiny::req(r, is.null(r$error))
      shiny::div(
        class = "epi-note",
        style = "margin-top: 6px;",
        "Crude and age-specific rates have exact Poisson confidence intervals; ",
        "DSRs have Dobson et al. (1991) intervals. A blank rate is suppressed ",
        "or could not be calculated. Persons include sex not stated and other sex."
      )
    })

    # Copy a table to the clipboard as tab-separated text, ready for Excel
    copy_table <- function(df, age) {
      if (is.null(rv$fetched)) {
        shiny::showNotification("Press Calculate first.", duration = 3,
                                type = "warning")
        return(invisible(NULL))
      }
      r <- results()
      shiny::req(is.null(r$error))
      .epi_copy_text(
        session,
        .dsr_tsv(df(), area_label(r$spec), r$multiplier, age = age,
                 suppress = r$suppress),
        "Table copied to the clipboard"
      )
    }
    shiny::observeEvent(input$copy_summary, {
      copy_table(function() results()$summary, age = FALSE)
    })
    shiny::observeEvent(input$copy_age, {
      copy_table(age_results, age = TRUE)
    })

    # ── Map ────────────────────────────────────────────────────────────────
    # Year or period, and sex, choices follow the results
    shiny::observeEvent(results(), {
      r <- results()
      shiny::req(r, is.null(r$error))
      times <- sort(unique(r$summary[[r$by[1]]]), decreasing = TRUE)
      sexes <- intersect(c("Persons", "Male", "Female"), unique(r$summary$Sex))
      shiny::updateSelectInput(
        session, "map_year", label = r$by[1], choices = times,
        selected = if (isTRUE(input$map_year %in% times)) input$map_year else times[1]
      )
      shiny::updateSelectInput(
        session, "map_sex", choices = sexes,
        selected = if (isTRUE(input$map_sex %in% sexes)) input$map_sex else "Persons"
      )
    })

    # A state map of the ACT alone shows one area
    mappable <- function(s) !(s$scope == "ACT" && s$level == "state")

    # Longitude and latitude limits of the map. The ACT, and the ACT and
    # surrounds, show all their areas. Australian SA3 and SA2 maps open on the
    # ACT and surrounding region. The view of Australia leaves out Christmas,
    # Cocos (Keeling) and Norfolk Islands, which would shrink the mainland.
    map_view <- function(s) {
      if (s$scope != "AUS") return(list())
      if (s$level != "state" && !identical(input$map_extent, "all")) {
        return(list(xlim = c(147.6, 150.6), ylim = c(-37.1, -34.0)))
      }
      list(xlim = c(112.5, 154), ylim = c(-44, -9.5))
    }

    output$map_extent_ui <- shiny::renderUI({
      r <- results()
      shiny::req(r, is.null(r$error), r$spec$scope == "AUS",
                 r$spec$level != "state")
      shiny::selectInput(
        "map_extent", "Extent",
        choices = c("ACT and surrounding region" = "region",
                    "Australia" = "all"),
        width = "100%"
      )
    })

    # Boundaries are downloaded when the map is first shown for a level and
    # population, then kept for the session (and on disk by .dsr_boundaries)
    boundaries <- shiny::reactive({
      r <- results()
      shiny::req(r, is.null(r$error), mappable(r$spec))
      key <- paste0("bounds_", r$spec$level, "_", r$spec$scope)
      if (is.null(cache[[key]])) {
        b <- tryCatch(
          shiny::withProgress(
            message = "Downloading boundaries from the ABS",
            list(polys = .dsr_boundaries(r$spec$level, r$spec$scope),
                 error = NULL)
          ),
          error = function(e) list(polys = NULL, error = conditionMessage(e))
        )
        if (!is.null(b$error)) return(b)
        cache[[key]] <- b
      }
      cache[[key]]
    })

    map_values <- shiny::reactive({
      r <- results()
      shiny::req(r, is.null(r$error), input$map_year, input$map_sex,
                 input$map_value)
      d <- r$summary[as.character(r$summary[[r$by[1]]]) == input$map_year &
                       r$summary$Sex == input$map_sex, ]
      list(r = r, rows = d,
           values = data.frame(Area = d$Area, Value = d[[input$map_value]],
                               stringsAsFactors = FALSE))
    })

    map_plot <- shiny::reactive({
      b <- boundaries()
      shiny::req(is.null(b$error))
      m <- map_values()
      r <- m$r
      s <- r$spec
      value <- input$map_value
      per <- paste0(" per ", .fmt_count(r$multiplier))
      label <- switch(value,
                      DSR    = paste0("Age-standardised rate", per),
                      Crude  = paste0("Crude rate", per),
                      Events = "Events")
      view <- map_view(s)
      .dsr_map_plot(
        b$polys, m$values,
        legend   = switch(value, DSR = "DSR", Crude = "Crude rate",
                          Events = "Events"),
        title    = paste0(label, ", ", input$map_year, ", ",
                          tolower(input$map_sex)),
        subtitle = .dsr_describe(s, if (value == "DSR") std_name(s$standard),
                                 years = FALSE),
        caption  = paste0(
          "Grey: ",
          if (r$suppress > 0) {
            paste0("fewer than ", .fmt_count(r$suppress),
                   " events (suppressed) or ")
          },
          "no value. Boundaries: ABS ASGS Edition 3 (2021), CC BY 4.0."
        ),
        xlim = view$xlim,
        ylim = view$ylim
      )
    })

    output$map <- shiny::renderPlot(map_plot(), res = 96)

    output$map_note <- shiny::renderUI({
      if (is.null(rv$fetched)) {
        return(shiny::div(class = "epi-note",
                          "Press Calculate, then the map shows the results."))
      }
      s <- rv$fetched$spec
      if (!mappable(s)) {
        return(shiny::div(class = "epi-note",
                          "Choose SA3 or SA2 to map areas within the ACT."))
      }
      b <- boundaries()
      if (!is.null(b$error)) {
        return(shiny::div(
          class = "epi-note epi-error",
          "The boundaries could not be downloaded from the ABS ",
          "(geo.abs.gov.au): ", b$error, " Press Calculate to try again."
        ))
      }
      shiny::div(class = "epi-note", "Click an area for its figures.")
    })

    output$map_click_info <- shiny::renderUI({
      click <- input$map_click
      shiny::req(click)
      b <- boundaries()
      shiny::req(is.null(b$error))
      area <- .dsr_point_area(click$x, click$y, b$polys)
      shiny::req(!is.na(area))
      m <- map_values()
      row <- m$rows[m$rows$Area == area, ]
      name <- b$polys$Name[match(area, b$polys$Area)]
      head <- paste0(area_label(m$r$spec), " ", area,
                     if (!is.na(name) && name != area) paste0(" ", name), ": ")
      digits <- if (m$r$multiplier >= 100000) 1 else 2
      num <- function(v) formatC(v, format = "f", digits = digits, big.mark = ",")
      text <- if (!nrow(row)) {
        "no results."
      } else if (isTRUE(row$Suppressed[1])) {
        paste0("fewer than ", .fmt_count(m$r$suppress),
               " events (suppressed); population ",
               .fmt_count(row$Population[1]), ".")
      } else {
        paste0(.fmt_count(row$Events[1]), " events, population ",
               .fmt_count(row$Population[1]), ", crude rate ",
               num(row$Crude[1]), ", DSR ",
               if (is.na(row$DSR[1])) "not calculated" else paste0(
                 num(row$DSR[1]), " (95% CI ", num(row$DSRLower[1]), " to ",
                 num(row$DSRUpper[1]), ")"),
               ".")
      }
      shiny::div(class = "map-click", shiny::strong(head), text)
    })

    shiny::observeEvent(input$copy_map_result, {
      res <- input$copy_map_result
      shiny::showNotification(res$msg, duration = 4,
                              type = if (isTRUE(res$ok)) "message" else "warning")
    })

    map_file <- function(ext) {
      function() {
        paste0("dsr-map-", tolower(input$map_value), "-", input$map_year, "-",
               tolower(input$map_sex), ".", ext)
      }
    }
    # A saved map takes the shape of the area in view
    save_map <- function(file, device) {
      p    <- map_plot()
      view <- map_view(results()$spec)
      size <- .dsr_map_size(p$data, view$xlim, view$ylim)
      ggplot2::ggsave(file, p, width = size[["width"]],
                      height = size[["height"]], dpi = 200, bg = "white",
                      device = device)
    }
    output$map_png <- shiny::downloadHandler(
      filename = map_file("png"),
      content  = function(file) save_map(file, "png")
    )
    output$map_pdf <- shiny::downloadHandler(
      filename = map_file("pdf"),
      content  = function(file) save_map(file, "pdf")
    )

    # ── Insert / copy code button ──────────────────────────────────────────
    shiny::observeEvent(input$insert_code, {
      st <- settings()
      s  <- st$spec
      if (is.null(s)) {
        shiny::showNotification(st$error, duration = 6, type = "warning")
        return(invisible(NULL))
      }
      code <- .dsr_code(
        s,
        by_sex        = isTRUE(input$by_sex),
        age_specific  = isTRUE(input$age_specific),
        multiplier    = as.numeric(input$multiplier),
        standard_name = std_name(s$standard),
        suppress      = suppress()
      )
      .epi_send_code(session, code)
    })

    # ── Done button ────────────────────────────────────────────────────────
    shiny::observeEvent(input$done, {
      shiny::stopApp()
    })

  }

  # App-level cleanup. A graceful disconnect only happens in foreground mode:
  # in a background worker, dbDisconnect() on a wedged ODBC connection can
  # hang R shutdown, so cleanup there is left to process exit instead.
  shiny::shinyApp(
    ui      = ui,
    server  = server,
    onStart = function() {
      if (in_rstudio && !is.null(db)) {
        shiny::onStop(db$disconnect)
      }
    }
  )

}
