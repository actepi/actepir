#' Calculate Age-Standardised Rates from EpiServer
#'
#' @description
#' Opens the DSR Calculator, which calculates directly age-standardised rates
#' (DSRs) of emergency department presentations (ED) or admitted patient
#' separations (APC). It queries EpiServer for event counts by calendar year,
#' sex, 5-year age group and area of residence, matches them to the estimated
#' resident population of the same year, and standardises them to a chosen
#' standard population. It shows counts, crude rates and DSRs, optionally with
#' age-specific rates, and copies R code that reproduces the results.
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
#' * *Population*: ACT residents, using the ACT population tables, or
#'   Australian residents, using the Australian tables.
#' * *Geographic level*: state or territory (`geo_STATE`), SA3
#'   (`geo_SA3_2021`) or SA2 (`geo_SA2_2021`) of residence, with rates for
#'   each area. Populations by 5-year age group are not available below SA2,
#'   so SA1 and mesh blocks are not offered.
#' * *Standard population*: any standard with 5-year age groups in
#'   `Analysis.dbo.StandardPops`. The default is the Australian 2001 standard
#'   population.
#' * *Calendar years*: one row of results per year. The years offered are
#'   those covered by both the event data and the population table, and a
#'   year only partly covered by the event data is flagged.
#' * *Male and female*: adds male and female rates to the rates for persons.
#' * *Age-specific rates*: adds a table of rates by 5-year age group.
#' * *Rate per*: 1,000, 10,000 or 100,000 population.
#'
#' **Data**
#'
#' Age groups (0-4, 5-9, ..., 80-84, 85+) are formed from age in years
#' (`AgeYrs` in ED, `AgeYears` in APC) and match `AgeGroup05Code` in the
#' population tables. Populations come from the 5-year estimated resident
#' population tables (`ERP5_STE`, `ERP5_SA3` and `ERP5_SA2`, ACT or
#' Australian) for the same year as the events. Area of residence comes from
#' the geocoded `geo_` columns, on the ASGS 2021 boundaries the population
#' tables use.
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
#' calculator runs and the [dsr_calculate()] call that turns their results
#' into the table shown. The code reproduces the results outside the
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

# Event tables in Analysis.dbo: the date that sets the calendar year and the
# age in years
.dsr_datasets <- list(
  ED  = list(table = "ED",  date = "PresentationDateTime", age = "AgeYrs",
             label = "ED presentations"),
  APC = list(table = "APC", date = "SeparationDate",       age = "AgeYears",
             label = "APC separations")
)

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
# through here: names are matched against the definitions above and numbers
# are coerced to integer.
#' @noRd
.dsr_spec <- function(dataset = "ED", scope = "ACT", level = "state",
                      years, standard = 101L) {

  dataset <- match.arg(dataset, names(.dsr_datasets))
  scope   <- match.arg(scope, c("ACT", "AUS"))
  level   <- match.arg(level, names(.dsr_levels))

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

  list(dataset = dataset, scope = scope, level = level, years = years,
       standard = standard)

}


# Population table for the settings, e.g. ERP5_SA3_ACT
#' @noRd
.dsr_pop_table <- function(spec) {
  paste0("ERP5_", .dsr_levels[[spec$level]]$erp, "_", spec$scope)
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

# Event counts by calendar year, sex, 5-year age group and area of residence.
# For ACT residents, events with no recorded residence are kept so that they
# can be counted and reported; dsr_calculate() leaves them out of the rates.
#' @noRd
.dsr_sql_events <- function(spec) {

  ds  <- .dsr_datasets[[spec$dataset]]
  geo <- .dsr_levels[[spec$level]]$column

  scope <- if (spec$scope == "ACT") {
    # ASGS codes start with the state code, 8 for the ACT
    in_act <- if (spec$level == "state") " = 'ACT'" else " LIKE '8%'"
    paste0("\n      AND (", geo, in_act, " OR ", geo, " IS NULL)")
  } else {
    ""
  }

  .dsr_fill("SELECT Year, Sex, AgeGroup, Area, COUNT(*) AS Events
FROM (
    SELECT YEAR({date}) AS Year,
           Sex,
           CASE WHEN {age} >= 85 THEN 18
                WHEN {age} >= 0 THEN {age} / 5 + 1
           END AS AgeGroup,
           {geo} AS Area
    FROM Analysis.dbo.{table}
    WHERE {date} >= '{from}0101'
      AND {date} < '{to}0101'{scope}
) AS e
GROUP BY Year, Sex, AgeGroup, Area
ORDER BY Year, Sex, AgeGroup, Area",
    date = ds$date, age = ds$age, geo = geo, table = ds$table,
    from = spec$years[1], to = spec$years[2] + 1L, scope = scope)

}


# Estimated resident population by year, sex, 5-year age group and area
#' @noRd
.dsr_sql_population <- function(spec) {

  if (spec$level == "state") {
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
       ERPCount AS Population
FROM Analysis.dbo.{table}
WHERE ERPYear BETWEEN {from} AND {to}{entity}
ORDER BY Year, Sex, AgeGroup, Area",
    area = area, table = .dsr_pop_table(spec),
    from = spec$years[1], to = spec$years[2], entity = entity)

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
.dsr_describe <- function(spec, standard_name = NULL) {
  years <- if (spec$years[1] == spec$years[2]) {
    spec$years[1]
  } else {
    paste(spec$years[1], "to", spec$years[2])
  }
  level <- .dsr_levels[[spec$level]]$label
  if (spec$level == "state") level <- tolower(level)
  paste0(
    .dsr_datasets[[spec$dataset]]$label, ", ",
    if (spec$scope == "ACT") "ACT residents" else "Australian residents",
    ", by ", level, ", ", years,
    if (!is.null(standard_name)) paste0(", standardised to ", standard_name)
  )
}


# R code that reproduces the calculator's results
#' @noRd
.dsr_code <- function(spec, by_sex = FALSE, age_specific = FALSE,
                      multiplier = 100000, standard_name = NULL) {

  args <- paste0(
    "  by = c(\"Year\", \"Area\"), by_sex = ", if (by_sex) "TRUE" else "FALSE",
    ", multiplier = ", format(multiplier, scientific = FALSE), "\n"
  )

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


# DataTable of calculator results with grouped rate headers
#' @noRd
.dsr_datatable <- function(df, area_label, multiplier, age = FALSE) {

  id_cols <- c("Year", "Area", "Sex", if (age) "AgeGroupName")
  if (age) {
    rate_cols  <- c("Rate", "RateLower", "RateUpper")
    rate_heads <- list("Age-specific rate (95% CI)")
  } else {
    rate_cols  <- c("Crude", "CrudeLower", "CrudeUpper",
                    "DSR", "DSRLower", "DSRUpper")
    rate_heads <- list("Crude rate (95% CI)", "Age-standardised rate (95% CI)")
  }
  df <- df[, c(id_cols, "Events", "Population", rate_cols)]

  heads <- c(Year = "Year", Area = area_label, Sex = "Sex",
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
      autoWidth  = FALSE
    )
  )
  tbl <- DT::formatRound(tbl, c("Events", "Population"), digits = 0, mark = ",")
  DT::formatRound(tbl, rate_cols, digits = digits, mark = ",")

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
      # slider spacing, notes and the error bar
      shiny::tags$style(shiny::HTML("
        .dsr-row { flex-wrap: wrap; }
        .dsr-row > * { min-width: 200px; }
        .selector-row .irs { margin-top: -4px; }
        .info-bar p { margin: 0 0 4px 0; }
        .info-bar p:last-child { margin-bottom: 0; }
        .info-bar .epi-warn { color: #8a5300; }
        .info-bar.epi-error { border-left-color: #b00020; color: #b00020; }
        .epi-note { font-size: 11px; color: #777; margin-top: 6px; }
        h5.epi-table-title { color: var(--epi-primary); font-weight: 600;
                             margin: 16px 0 6px 0; }
      ")),

      # Clipboard handler used when running outside RStudio (background mode)
      .epi_copy_script(),

      shiny::div(
        class = "selector-row dsr-row",
        shiny::selectInput(
          "dataset", "Dataset",
          choices = c("ED presentations" = "ED", "APC separations" = "APC"),
          width = "100%"
        ),
        shiny::selectInput(
          "scope", "Population",
          choices = c("ACT residents" = "ACT", "Australian residents" = "AUS"),
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
            "multiplier", "Rate per",
            choices  = c("1,000" = "1000", "10,000" = "10000",
                         "100,000" = "100000"),
            selected = "100000",
            width = "100%"
          )
        )
      ),

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

      DT::dataTableOutput("summary_table"),

      shiny::conditionalPanel(
        "input.age_specific",
        shiny::tags$h5("Age-specific rates", class = "epi-table-title"),
        DT::dataTableOutput("age_table")
      ),

      shiny::uiOutput("footnote")

    )
  )

  # ── Server ────────────────────────────────────────────────────────────────
  server <- function(input, output, session) {

    rv <- shiny::reactiveValues(fetched = NULL, error = NULL)

    # Settings as entered; NULL while the years are out of range
    spec <- shiny::reactive({
      shiny::req(input$dataset, input$scope, input$level, input$standard,
                 input$years)
      tryCatch(
        .dsr_spec(input$dataset, input$scope, input$level, input$years,
                  input$standard),
        error = function(e) NULL
      )
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

    # ── Calculate: run the events and population queries ──────────────────
    shiny::observeEvent(input$calculate, {
      s <- spec()
      shiny::req(s)
      rv$error <- NULL
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

    # ── Rates: recalculated from the fetched counts when the standard, sex
    # or multiplier changes, without querying the events again ─────────────
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
        mult    <- as.numeric(input$multiplier)
        summary <- suppressMessages(dsr_calculate(
          f$events, f$population, standard,
          by = c("Year", "Area"), by_sex = isTRUE(input$by_sex),
          multiplier = mult
        ))
        list(spec = s, summary = summary, multiplier = mult,
             total = sum(f$events$Events), error = NULL)
      }, error = function(e) list(error = conditionMessage(e)))
    })

    # Age-specific rates, calculated only while their table is shown
    age_results <- shiny::reactive({
      r <- results()
      shiny::req(r, is.null(r$error))
      f <- rv$fetched
      suppressMessages(dsr_age_specific(
        f$events, f$population,
        by = c("Year", "Area"), by_sex = isTRUE(input$by_sex),
        multiplier = r$multiplier
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

      n_na <- sum(is.na(r$summary$DSR))
      if (n_na) {
        notes <- c(notes, list(shiny::p(class = "epi-warn", paste0(
          "The DSR could not be calculated for ", n_na, " row(s) with events ",
          "in an age group that has no population."
        ))))
      }

      now <- spec()
      if (!is.null(now) &&
          !identical(now[c("dataset", "scope", "level", "years")],
                     s[c("dataset", "scope", "level", "years")])) {
        notes <- c(notes, list(shiny::p(class = "epi-warn",
          "The settings have changed: press Calculate to update the table."
        )))
      }

      shiny::div(class = "info-bar", notes)
    })

    # ── Tables ─────────────────────────────────────────────────────────────
    area_label <- function(s) .dsr_levels[[s$level]]$label

    output$summary_table <- DT::renderDataTable({
      r <- results()
      shiny::req(r, is.null(r$error))
      .dsr_datatable(r$summary, area_label(r$spec), r$multiplier)
    })

    output$age_table <- DT::renderDataTable({
      r <- results()
      shiny::req(r, is.null(r$error), isTRUE(input$age_specific))
      .dsr_datatable(age_results(), area_label(r$spec), r$multiplier, age = TRUE)
    })

    output$footnote <- shiny::renderUI({
      r <- results()
      shiny::req(r, is.null(r$error))
      shiny::div(
        class = "epi-note",
        "Crude and age-specific rates have exact Poisson confidence intervals; ",
        "DSRs have Dobson et al. (1991) intervals. A blank rate could not be ",
        "calculated. Persons include sex not stated and other sex."
      )
    })

    # ── Insert / copy code button ──────────────────────────────────────────
    shiny::observeEvent(input$insert_code, {
      s <- spec()
      if (is.null(s)) {
        shiny::showNotification("Choose a year range first.",
                                duration = 3, type = "warning")
        return(invisible(NULL))
      }
      code <- .dsr_code(
        s,
        by_sex        = isTRUE(input$by_sex),
        age_specific  = isTRUE(input$age_specific),
        multiplier    = as.numeric(input$multiplier),
        standard_name = std_name(s$standard)
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
