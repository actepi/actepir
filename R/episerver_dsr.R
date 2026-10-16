#' Calculate Age-Standardised Rates from EpiServer
#'
#' @description
#' Opens the DSR Calculator, which calculates directly age-standardised rates
#' (DSRs) of emergency department presentations (ED) or admitted patient
#' separations (APC). It queries EpiServer for event counts by calendar year,
#' sex, 5-year age group and area of residence, matches them to the estimated
#' resident population of the same year, and standardises them to a chosen
#' standard population. Events can be limited to ICD-10 diagnosis codes and
#' rates to an age range. It shows counts, crude rates and DSRs, optionally
#' with age-specific rates, compares areas in an interactive map and funnel
#' plot, and copies R code that reproduces the results. Its *Help* tab sets
#' out the data, formulas and sources.
#'
#' Like [episerver_browse()], the calculator runs in a background R process by
#' default so that the console stays free, and it can be opened from the
#' RStudio Addins menu (*Open DSR Calculator*). It opens in the system web
#' browser, so that it has the whole window: the settings are in a sidebar on
#' the left, in sections that open and close, and the results in tabs on the
#' top bar. The arrow at the top of the sidebar hides it.
#'
#' @inheritParams episerver_browse
#' @inheritDotParams episerver_connect encrypt trust_certificate
#'
#' @return In background mode, invisibly returns the `callr` process handle.
#'   In foreground mode, returns nothing.
#'
#' @details
#' **Options**
#'
#' The sidebar's sections hold the options: *Events* (dataset, diagnosis and
#' procedure codes, and how they are searched), *Population* (population and
#' geographic level), *Time
#' periods* (calendar years and pooling) and *Rate details* (ages, standard
#' population, rate per, suppression, and male and female).
#' * *Dataset*: ED presentations, counted by the calendar year of
#'   `PresentationDateTime`, or APC separations, counted by the calendar year
#'   of `SeparationDate`.
#' * *Population*: ACT residents, using the ACT population tables, or ACT and
#'   surrounds: the ACT and the four NSW SA3s that border it (Queanbeyan,
#'   Snowy Mountains, Young - Yass and Tumut - Tumbarumba) with their SA2s,
#'   using the Australian tables. At state level ACT and surrounds gives two
#'   areas, the ACT and NSW (surrounds), each summed from its SA3s. Both are
#'   populations that mostly use ACT hospitals.
#' * *Geographic level*: state or territory (`geo_STATE`), SA3
#'   (`geo_SA3_2021`) or SA2 (`geo_SA2_2021`) of residence, with rates for
#'   each area. SA3s and SA2s are ASGS 2021 areas (see **Data**). Populations
#'   by 5-year age group are not available below SA2, so SA1 and mesh blocks
#'   are not offered.
#' * *Standard population*: any standard with 5-year age groups in
#'   `Analysis.dbo.StandardPops`. The default is the Australian 2001 standard
#'   population.
#' * *Calendar years*: one row of results per year, or per period when the
#'   years are pooled. The years offered are those covered by both the event
#'   data and the population table, and a year only partly covered by the
#'   event data is flagged. The slider starts on three years: the latest year
#'   offered that the event data cover in full, and the two before it.
#' * *Pool years*: pools the years into periods of 2 to 5 years, or into one
#'   period of all the selected years, for areas or conditions with few
#'   events. The number of years selected must be a multiple of the period
#'   length. Events and populations are summed over the years of a period, so
#'   the rates are per person-year.
#' * *Ages*: limits the rates to a range of 5-year age groups, for example
#'   under 65 (0-4 to 60-64), under 75 or 35 to 74. The default is all ages.
#'   Events and populations outside the range are left out, and the DSR is
#'   standardised within the range: each age group in it is weighted by its
#'   share of the standard population of the range, so the weights sum to 1.
#'   The crude rate is the events in the range divided by the population in
#'   the range. Rates are per population of those ages, and can be compared
#'   only with rates for the same ages and standard population.
#' * *Male and female*: adds male and female rates to the rates for persons.
#' * *Rate per*: 1,000, 10,000 or 100,000 population.
#' * *Suppress counts below*: rows with fewer events than this have their
#'   events and rates withheld (see [dsr_calculate()]). The default is 5; 0
#'   turns suppression off.
#' * *Minimum events for a DSR* and *Minimum population per age group*: a
#'   row's age-standardised rate is not presented (n.p.) when it has fewer
#'   events in total, or a population under the minimum in any of its age
#'   groups, as the AIHW (2011) principles for direct age-standardisation
#'   advise (Principles 3 and 2), which the ABS applies (ABS 2011). The
#'   defaults are the principles' 20 and 30; 0 turns a check off. The row's
#'   counts and crude rate are still shown.
#' * *Diagnosis codes*: counts only events with a diagnosis matching any of
#'   the ICD-10 codes or ranges entered, for example `J45`, `C13-C15.45` or
#'   `E18.3 to E18.78`. A partial code matches every code that starts with
#'   it. A range runs from its first code to its last and includes every
#'   code that starts with the last. Dots and case do not matter. Only
#'   ICD-10 coded records are searched: ED records with an `ICD10AMEdition`
#'   other than 99, and APC records with an `ICDVersion` of 10. With no codes
#'   every event is counted. A code or range is added as typed, with Enter,
#'   Tab or a comma. Words from a condition, such as `myocardial`, list the
#'   ICD-10-AM codes whose titles have a word starting with each word typed,
#'   and Enter, Tab or a click adds the code highlighted. Each code or range
#'   entered is spelt out below the box with its titles. The titles come from
#'   `Analysis.dbo.ICD10AM`, each from the latest edition that holds the code;
#'   a code that some editions lack is labelled with the editions that hold
#'   it.
#' * *Diagnosis search*: for APC, the principal diagnosis (`Diagnosis1`) or
#'   any diagnosis (`Diagnosis1` to `Diagnosis100`). ED always searches all
#'   four of its diagnosis fields (`EDShortListCode` and `Diagnosis1` to
#'   `Diagnosis3`), as `Diagnosis1` is recorded to 2020 and `EDShortListCode`
#'   from 2013, so neither holds the principal diagnosis in every year.
#' * *Procedure codes*: for APC only, counts only events with a procedure
#'   matching any of the ACHI codes, blocks or ranges entered, for example
#'   `11012-00`, `11012`, `1826` or `11000-00 to 11027-00`. Codes match as
#'   diagnosis codes do, without hyphens. A number of up to four digits is an
#'   ACHI block, shown as `[1826]`, matched against the block recorded with
#'   each procedure (`BlockProcedure1` to `BlockProcedure50`), and a range of
#'   blocks such as `1820-1830` includes both ends. Words from a procedure
#'   list the codes and blocks whose titles match, as for diagnoses. ED
#'   records hold no procedures, so the procedure options are disabled for
#'   ED and its rates ignore any procedure codes entered.
#' * *Procedure search*: the principal procedure (`Procedure1`) or any
#'   procedure (`Procedure1` to `Procedure50`).
#' * *With diagnoses and procedures, count events with*: when both diagnosis
#'   and procedure codes are entered, events with a diagnosis and a procedure
#'   (the default), or with either.
#'
#' **Tables, map and funnel plot**
#'
#' The *Tables* tab shows the rates, the *Age-specific rates* by 5-year age
#' group for the ages chosen, or the *Counts by age*: the events and the
#' population of each age group, one row of each for every year or period,
#' area and sex, with populations under the minimum per age group highlighted
#' in yellow and event totals under the minimum for a DSR in blue. *Copy
#' table* copies the table shown as tab-separated text that pastes into
#' Excel.
#'
#' The *Explore* tab compares the SA3s or SA2s in an interactive map and a
#' funnel plot, side by side, of the age-standardised rate, crude rate,
#' age-specific rate or events of one year or period and sex. State level
#' gives too few areas to compare (one for ACT residents, two for the ACT
#' and surrounds), so the tab shows neither plot for it. The age-specific
#' rate is for the age range chosen with *Ages mapped*, within the ages of
#' the rates, for example 0 to 14 or 20 to 44: the events in the range
#' divided by the population in the range, suppressed as in the tables. It
#' is not age-standardised, so over a wide range it reflects differences in
#' age structure within the range; for a standardised rate, set *Ages* and
#' show the age-standardised rate.
#'
#' Pointing at an area or a point shows its figures. Clicking areas or
#' points, or drawing round them with the lasso in a plot's toolbar, selects
#' them in both plots and lists them below; *Zoom to selected* zooms the map
#' to them, and the magnifier in the toolbar zooms and pans. The map can be
#' coloured by value or by each area's position in the funnel plot.
#' *Download* saves the map or the funnel plot as PDF or PNG, titled with the
#' measure and settings; the map follows *Zoom to selected* but not the
#' toolbar's zoom. Boundaries are the ASGS Edition 3 (2021) boundaries,
#' downloaded from the ABS boundary service (`geo.abs.gov.au`) the first time
#' a level is mapped and kept in the folder given by
#' `tools::R_user_dir("actepir", "cache")`.
#'
#' The funnel plot compares each area with the average of the areas plotted,
#' allowing for the size of its population, so that areas can be compared
#' without being misled by the chance variation of small numbers:
#' * Crude and age-specific rates are plotted against the population.
#' * DSRs are plotted against the population that would give the DSR as a
#'   crude rate from the area's events (events / DSR), as in the funnel plots
#'   of the Office for Health Improvement and Disparities (OHID). The average
#'   is the events of the areas plotted over their derived populations. Areas
#'   whose DSR is not presented (n.p.) are left out.
#' * Events are divided by the events expected from the age-specific rates of
#'   all the areas plotted (indirect standardisation) and plotted against the
#'   expected events, as counts alone cannot be compared between areas of
#'   different sizes.
#'
#' The curves are 95% and 99.8% control limits from exact Poisson tests of
#' each area's events against the events expected at the average: an area
#' outside them differs from the average by more than chance. When areas
#' vary more than chance alone would explain (overdispersion), as large
#' counts such as all ED presentations often do, most areas fall outside
#' these limits. *Allow for overdispersion*, ticked by default, then widens
#' the limits by Spiegelhalter's additive random effects method, estimated
#' from z-scores on the square root scale winsorised at the 10th and 90th
#' percentiles, so that areas outside them stand out from the usual variation
#' between areas. The tab reports the overdispersion ratio and its
#' chi-squared test; with fewer than 10 areas the estimate is unreliable.
#' Suppressed areas are neither plotted nor included in the average. Areas
#' above the limits are coral (99.8%) and yellow (95%), and areas below them
#' blue (99.8%) and green (95%), from the ACT Government's regional colours.
#'
#' The *Help* tab gives the formulas for each rate, confidence interval and
#' funnel plot limit, the tables and fields used, the boundary services and
#' references.
#'
#' **Data**
#'
#' Age groups (0-4, 5-9, ..., 80-84, 85+) are formed from age in years
#' (`AgeYrs` in ED, `AgeYears` in APC) and match `AgeGroup05Code` in the
#' population tables. Populations come from the 5-year estimated resident
#' population tables (`ERP5_STE`, `ERP5_SA3` and `ERP5_SA2`, ACT or
#' Australian) for the same year as the events. Area of residence comes from
#' the geocoded `geo_` columns; for ACT and surrounds, the state comes from
#' the SA3 (`geo_SA3_2021`).
#'
#' SA3s and SA2s are areas of the Australian Statistical Geography Standard
#' (ASGS) Edition 3 (2021). The population tables hold areas of this
#' edition, as do the `geo_SA3_2021` and `geo_SA2_2021` columns and the map
#' boundaries. The tables do not record their edition, so the calculator
#' assumes ASGS 2021 and labels SA3 and SA2 results and maps with it. Area
#' codes and boundaries differ between editions, so populations on ASGS 2016
#' or ASGS 2026 areas would not match these.
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
#' calculator runs, including any diagnosis condition, the lines that keep
#' the age groups in range when the ages are limited and that assign each
#' year to its period when the years are pooled, and the
#' [dsr_calculate()] call, with the suppression threshold, that turns their
#' results into the table shown, and the [dsr_age_specific()] call when the
#' age-specific rates are shown. The code reproduces the results outside the
#' calculator and can be adapted, for example by adding conditions to the
#' events query.
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
#' Spiegelhalter DJ (2005). Funnel plots for comparing institutional
#' performance. *Statistics in Medicine*, 24(8), 1185-1202.
#'
#' Spiegelhalter DJ (2005). Handling over-dispersion of performance
#' indicators. *Quality and Safety in Health Care*, 14(5), 347-351.
#'
#' Public Health England (2017). *Technical guide: funnel plots*. Public
#' Health Data Science. Implemented in the R package PHEindicatormethods.
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
#' # Launch in a background process and open it in the web browser; the
#' # console stays free
#' episerver_dsr()
#'
#' # Stop the background app
#' episerver_dsr_stop()
#' }
#'
#' @author Warren Holroyd
#'
episerver_dsr <- function(background = TRUE, port = NULL,
                          driver = NULL, max_attempts = NULL, ...) {

  .run_app(
    key          = "dsr",
    title        = "DSR Calculator",
    fn           = "episerver_dsr",
    factory      = "episerver_dsr_app",
    factory_args = c(list(driver = driver, max_attempts = max_attempts),
                     list(...)),
    background   = background,
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

# ASGS edition of the areas in the ERP5 tables, which the tables do not
# record. They hold ASGS Edition 3 (2021) areas, as do the geo_..._2021
# columns of the event tables. The geo_ column names, the map boundaries and
# the area labels follow it; the SA3 codes in .dsr_surrounds are ASGS 2021
# codes.
.dsr_asgs <- list(year = 2021L, name = "ASGS Edition 3 (2021)",
                  short = "ASGS 2021")

# Event tables in Analysis.dbo: the date that sets the calendar year, the age
# in years, the diagnosis fields, whether the first of them is a principal
# diagnosis, and the condition that keeps ICD-10 coded records when diagnosis
# codes are searched. ED has no principal diagnosis field that spans the
# years: Diagnosis1 is recorded to 2020 and EDShortListCode from 2013, alone
# from 2021, and the two can differ, so ED always searches all four fields.
# Older APC records are coded in ICD-9, and ED records with ICD10AMEdition 99
# are not ICD-10, so codes such as V56.0 would otherwise match the wrong
# conditions. APC records also hold ACHI procedures, the first of them the
# principal procedure, each with its ACHI block in the matching BlockProcedure
# field; ED records hold none.
.dsr_datasets <- list(
  ED  = list(table = "ED",  date = "PresentationDateTime", age = "AgeYrs",
             label = "ED presentations",
             diagnoses = c("EDShortListCode", paste0("Diagnosis", 1:3)),
             principal = FALSE,
             procedures = NULL, blocks = NULL,
             icd10 = "COALESCE(ICD10AMEdition, 0) <> 99"),
  APC = list(table = "APC", date = "SeparationDate",       age = "AgeYears",
             label = "APC separations",
             diagnoses = paste0("Diagnosis", 1:100),
             principal = TRUE,
             procedures = paste0("Procedure", 1:50),
             blocks = paste0("BlockProcedure", 1:50),
             icd10 = "ICDVersion = 10")
)

# Populations: the choice offered, the wording in descriptions, the region
# whose areas are compared and the ERP5 tables used. The ACT and surrounds
# take their populations from the Australian tables.
.dsr_scopes <- list(
  ACT       = list(label = "ACT residents", describe = "ACT residents",
                   region = "the ACT", erp = "ACT"),
  SURROUNDS = list(label = "ACT and surrounds",
                   describe = "residents of the ACT and surrounds",
                   region = "the ACT and surrounds", erp = "AUS")
)

# The NSW SA3s (ASGS 2021) that share a border with the ACT. With the ACT they
# make up the ACT and surrounds, at SA2 level too, as SA2 codes start with the
# code of their SA3. At state level they form one area with this label.
.dsr_surrounds <- c("10102" = "Queanbeyan", "10103" = "Snowy Mountains",
                    "10106" = "Young - Yass", "11302" = "Tumut - Tumbarumba")
.dsr_surrounds_label <- "NSW (surrounds)"

# Geographic levels: the geo_ column holding the area of residence and the
# level of the ERP5 population table
.dsr_levels <- list(
  state = list(column = "geo_STATE", erp = "STE", label = "State or territory"),
  sa3   = list(column = paste0("geo_SA3_", .dsr_asgs$year), erp = "SA3",
               label = "SA3"),
  sa2   = list(column = paste0("geo_SA2_", .dsr_asgs$year), erp = "SA2",
               label = "SA2")
)

# 5-year age groups by AgeGroup05Code: 1 is 0-4, ..., 17 is 80-84, 18 is 85+
.dsr_age_labels <- c(paste0(seq(0, 80, 5), "-", seq(4, 84, 5)), "85+")

# geo_STATE abbreviations by ABS state code (EntityCode in ERP5_STE tables,
# where 0 is the Australian total)
.dsr_states <- c("1" = "NSW", "2" = "VIC", "3" = "QLD", "4" = "SA",
                 "5" = "WA",  "6" = "TAS", "7" = "NT",  "8" = "ACT",
                 "9" = "OT")


# Validated calculator settings. Every value interpolated into SQL passes
# through here: names are matched against the definitions above, numbers are
# coerced to integer, diagnosis codes are parsed by .dsr_parse_codes() and
# procedures by .dsr_parse_procedures(). `pool` pools the years into periods
# of that many years, which must divide the range evenly. `ages` is the first
# and last age group (codes 1 to 18) included in the rates. `procedure` is
# the principal procedure or all, and `combine` whether events need a
# diagnosis and a procedure, or either, when both are given. A dataset
# without procedures (ED) takes none.
#' @noRd
.dsr_spec <- function(dataset = "ED", scope = "ACT", level = "state",
                      years, standard = 101L, codes = NULL,
                      diagnosis = "principal", pool = 1L, ages = c(1L, 18L),
                      procedures = NULL, procedure = "principal",
                      combine = "and") {

  dataset   <- match.arg(dataset, names(.dsr_datasets))
  scope     <- match.arg(scope, names(.dsr_scopes))
  level     <- match.arg(level, names(.dsr_levels))
  diagnosis <- match.arg(diagnosis, c("principal", "all"))
  procedure <- match.arg(procedure, c("principal", "all"))
  combine   <- match.arg(combine, c("and", "or"))
  if (!.dsr_datasets[[dataset]]$principal) diagnosis <- "all"
  if (is.data.frame(codes)) codes <- codes$Label
  codes <- .dsr_parse_codes(codes)
  if (is.data.frame(procedures)) procedures <- procedures$Label
  if (is.null(.dsr_datasets[[dataset]]$procedures)) procedures <- NULL
  procedures <- .dsr_parse_procedures(procedures)

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

  ages <- suppressWarnings(as.integer(ages))
  if (length(ages) != 2 || anyNA(ages) || ages[1] > ages[2] || ages[1] < 1 ||
      ages[2] > length(.dsr_age_labels)) {
    stop("'ages' must be two age group codes from 1 (0-4) to 18 (85+), ",
         "lowest first.", call. = FALSE)
  }

  list(dataset = dataset, scope = scope, level = level, years = years,
       standard = standard, codes = codes, diagnosis = diagnosis, pool = pool,
       ages = ages, procedures = procedures, procedure = procedure,
       combine = combine)

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


# Procedure criteria as typed, several to an entry when separated by commas
# or semicolons: ACHI codes such as 11012-00, partial codes of five digits or
# more such as 11012, ACHI blocks (numbers of up to four digits) such as 1826,
# [1826] or block 1826, and ranges of codes or of blocks, such as 11000-00 to
# 11027-00 or 1820-1830. A partial code stands for every code that starts
# with it, as for diagnoses. Returns one row per entry: Label (tidied), Type
# ("code" or "block"), and From and To (digits only) for the SQL. Anything
# else is an error naming the entry, so only digits reach the SQL.
#' @noRd
.dsr_parse_procedures <- function(x) {

  items <- trimws(unlist(strsplit(as.character(x), "[,;\n]+")))
  items <- items[nzchar(items)]
  code  <- "[0-9]{5}(?:-?[0-9]{1,2})?"
  block <- "[0-9]{1,4}"
  one   <- function(p) grepl(paste0("^(?:", code, "|", block, ")$"), p,
                             perl = TRUE)

  rows <- lapply(items, function(item) {
    # En and em dashes join the ends of a range; they are made from their code
    # points and matched as UTF-8 bytes, so the session's locale does not
    # matter
    s <- item
    for (dash in intToUtf8(c(0x2013, 0x2014), multiple = TRUE)) {
      s <- gsub(dash, " TO ", s, fixed = TRUE, useBytes = TRUE)
    }
    s <- gsub("BLOCK *|\\[|\\]", "", toupper(gsub("\\s+", " ", trimws(s))))
    parts <- trimws(sub("[*%]+$", "", trimws(strsplit(s, " TO ",
                                                      fixed = TRUE)[[1]])))
    # A hyphen joins the parts of a code (11012-00), or else the ends of a
    # range (1820-1830, 11000-11027)
    if (length(parts) == 1 && !one(parts)) {
      m <- regmatches(parts, regexec(paste0("^(", code, "|", block, ") ?- ?(",
                                            code, "|", block, ")$"),
                                     parts, perl = TRUE))[[1]]
      if (length(m)) parts <- m[2:3]
    }
    blocks <- grepl(paste0("^", block, "$"), parts)
    if (length(parts) < 1 || length(parts) > 2 || !all(one(parts)) ||
        length(unique(blocks)) != 1) {
      stop("'", item, "' is not an ACHI procedure code, block or range, ",
           "such as 11012-00, 1826 or 11000-00 to 11027-00.", call. = FALSE)
    }
    type <- if (blocks[1]) "block" else "code"
    keys <- if (blocks[1]) {
      as.character(as.integer(parts))
    } else {
      gsub("-", "", parts, fixed = TRUE)
    }
    if (length(keys) == 2 &&
        (if (blocks[1]) as.integer(keys[1]) > as.integer(keys[2]) else
          .dsr_code_cmp(keys[1], keys[2]) > 0)) {
      stop("In '", item, "' the first ", type, " comes after the second.",
           call. = FALSE)
    }
    label <- if (blocks[1]) {
      paste0("[", keys, "]")
    } else {
      sub("^([0-9]{5})([0-9])", "\\1-\\2", keys)
    }
    data.frame(Label = paste(label, collapse = " to "), Type = type,
               From = keys[1], To = keys[length(keys)],
               stringsAsFactors = FALSE)
  })

  out <- do.call(rbind, c(list(data.frame(Label = character(0),
                                          Type = character(0),
                                          From = character(0),
                                          To = character(0))), rows))
  out <- out[!duplicated(out[c("Type", "From", "To")]), , drop = FALSE]
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


# ── Diagnosis titles ────────────────────────────────────────────────────────

# Codes and titles for the diagnosis or procedure search, from the rows of
# .dsr_sql_icd() of that type: Code as ICD-10-AM or ACHI writes it, Key
# without the dot or hyphen as the SQL compares codes, Type ("code"), Block
# (the ACHI block of a procedure), Title with the editions that hold the code
# unless every edition in the table does, and Words, the key and title as
# .dsr_icd_words() matches them. Procedures add a row for each ACHI block
# (Type "block", Code [1826], Key 1826), with its description from the
# latest edition. Diagnoses are in code order; procedures by block, each
# block before its codes. Morphology codes (M8140/3) are left out, as the
# diagnosis condition skips them. Attributes give the first and last editions
# and the classification. NULL without titles.
#' @noRd
.dsr_icd_titles <- function(df, type = c("Diagnosis", "Procedure")) {

  type <- match.arg(type)
  if (is.null(df) || !nrow(df)) return(NULL)
  df   <- df[tolower(df$VarType) %in% tolower(type), , drop = FALSE]
  code <- toupper(trimws(df$Code))
  form <- if (type == "Diagnosis") {
    "^[A-Z][0-9]{2}(\\.[0-9A-Z]{1,3})?$"
  } else {
    "^[0-9]{5}-[0-9]{2}$"
  }
  keep <- grepl(form, code) & !is.na(df$LastEdition) & !duplicated(code)
  if (!any(keep)) return(NULL)
  df   <- df[keep, , drop = FALSE]
  code <- code[keep]

  # varchar text in a Latin1_General collation is Windows-1252; text the
  # driver has not converted to UTF-8 is converted here, so it can be matched
  text <- function(x) {
    x[is.na(x)] <- ""
    bad <- !validUTF8(x)
    x[bad] <- iconv(x[bad], "CP1252", "UTF-8", sub = "?")
    Encoding(x) <- "UTF-8"
    trimws(x)
  }
  title <- text(df$Description)

  first <- as.integer(df$FirstEdition)
  last  <- as.integer(df$LastEdition)
  editions <- c(min(first), max(last))
  span <- ifelse(first == last, paste("edition", first),
                 paste("editions", first, "to", last))
  key   <- gsub("[.-]", "", code)
  block <- if (type == "Procedure") {
    suppressWarnings(as.integer(df$ProcedureBlock))
  } else {
    rep(NA_integer_, length(code))
  }

  out <- data.frame(
    Code  = code,
    Key   = key,
    Type  = "code",
    Block = block,
    Title = ifelse(first > editions[1] | last < editions[2],
                   paste0(title, " (", span, ")"), title),
    Words = .dsr_icd_words(paste(key, title)),
    stringsAsFactors = FALSE
  )

  if (type == "Procedure") {
    latest <- order(-last)
    b <- !is.na(block[latest]) & !duplicated(block[latest])
    n <- block[latest][b]
    describe <- text(df$BlockDescription)[latest][b]
    out <- rbind(out, data.frame(
      Code = paste0("[", n, "]"), Key = as.character(n), Type = "block",
      Block = n, Title = describe,
      Words = .dsr_icd_words(paste("block", n, describe)),
      stringsAsFactors = FALSE
    ))
    out <- out[order(is.na(out$Block), out$Block, out$Type != "block",
                     out$Key, method = "radix"), , drop = FALSE]
  } else {
    out <- out[order(out$Key, method = "radix"), , drop = FALSE]
  }
  rownames(out) <- NULL
  attr(out, "editions") <- editions
  attr(out, "classification") <- if (type == "Diagnosis") "ICD-10-AM" else "ACHI"
  out

}


# Text as the code search matches it: lower case, accents dropped, dots and
# the hyphen after the five-digit stem of an ACHI code removed so that codes
# match with or without them, and anything other than letters and digits a
# space, with a space before every word
#' @noRd
.dsr_icd_words <- function(x) {
  # Accented letters, by code point, are matched as UTF-8 bytes, so that the
  # session's encoding and locale do not matter
  accents <- list(a = 0xe0:0xe5, c = 0xe7, e = 0xe8:0xeb, i = 0xec:0xef,
                  n = 0xf1, o = c(0xf2:0xf6, 0xf8), u = 0xf9:0xfc, y = 0xfd)
  x <- tolower(x)
  for (letter in names(accents)) {
    pattern <- paste(intToUtf8(accents[[letter]], multiple = TRUE),
                     collapse = "|")
    x <- gsub(pattern, letter, x, useBytes = TRUE)
  }
  x <- gsub("([0-9]{5})-(?=[0-9])", "\\1", gsub(".", "", x, fixed = TRUE),
            perl = TRUE)
  x <- gsub("[^a-z0-9]+", " ", x, useBytes = TRUE)
  paste0(" ", trimws(x))
}


# Suggestions for text typed in a codes box: the first n codes and blocks, in
# the order of the titles, whose code or title has a word starting with each
# word typed
#' @noRd
.dsr_icd_search <- function(icd, text, n = 100L) {
  words <- strsplit(trimws(.dsr_icd_words(text)), " ", fixed = TRUE)[[1]]
  if (!length(words)) return(icd[0, , drop = FALSE])
  hit <- Reduce(`&`, lapply(words, function(w) {
    grepl(paste0(" ", w), icd$Words, fixed = TRUE)
  }))
  utils::head(icd[hit, , drop = FALSE], n)
}


# Answers a codes box's requests for suggestions (?q=text) with JSON rows:
# the code or block (value), its title, its words for matching in the
# browser and its place in the titles (rank)
#' @noRd
.dsr_icd_response <- function(icd, req) {
  text <- shiny::parseQueryString(req$QUERY_STRING)$q
  rows <- .dsr_icd_search(icd, if (is.null(text)) "" else text)
  json <- jsonlite::toJSON(
    data.frame(value = rows$Code, title = rows$Title, words = rows$Words,
               rank = match(rows$Code, icd$Code)),
    dataframe = "rows"
  )
  shiny::httpResponse(200L, "application/json; charset=utf-8",
                      enc2utf8(as.character(json)))
}


# The codes, blocks and ranges of the settings in words, from the titles:
# Label, and Text, which gives a code's or block's title, the first and last
# of the codes a partial code heads, or the codes or blocks at the ends of a
# range. Codes and blocks with no title are named as missing from the
# editions in the table.
#' @noRd
.dsr_icd_describe <- function(codes, icd) {

  editions <- attr(icd, "editions")
  achi <- identical(attr(icd, "classification"), "ACHI")
  absent <- paste0("not in ", attr(icd, "classification"), " editions ",
                   editions[1], " to ", editions[2])
  types <- if (is.null(codes$Type)) rep("code", nrow(codes)) else codes$Type

  # The row of a code or block, or else of the first or last of the highest
  # codes that start with it
  find <- function(key, type, last) {
    rows <- which(icd$Type == type)
    i <- rows[match(key, icd$Key[rows])]
    if (!is.na(i) || type == "block") return(i)
    under <- rows[startsWith(icd$Key[rows], key)]
    if (!length(under)) return(NA_integer_)
    under <- under[nchar(icd$Key[under]) == min(nchar(icd$Key[under]))]
    if (last) max(under) else min(under)
  }
  end <- function(i, key, type) {
    if (!is.na(i)) return(paste(icd$Code[i], icd$Title[i]))
    written <- if (type == "block") {
      paste0("[", key, "]")
    } else if (achi) {
      sub("^([0-9]{5})([0-9])", "\\1-\\2", key)
    } else {
      sub("^(.{3})(.)", "\\1.\\2", key)
    }
    paste0(written, " (", absent, ")")
  }

  text <- vapply(seq_len(nrow(codes)), function(k) {
    from <- codes$From[k]
    to   <- codes$To[k]
    type <- types[k]
    f <- find(from, type, FALSE)
    t <- find(to, type, TRUE)
    if (from == to) {
      if (is.na(f)) return(absent)
      if (icd$Key[f] == from) return(icd$Title[f])
    }
    if (!is.na(f) && identical(f, t)) return(end(f, from, type))
    paste(end(f, from, type), "to", end(t, to, type))
  }, character(1))

  data.frame(Label = codes$Label, Text = text, stringsAsFactors = FALSE)

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


# Keeps the age groups from ages[1] to ages[2] of fetched events or
# population. Events with no recorded age stay in, so that they are still
# reported as left out of the rates. With fewer age groups in the population,
# dsr_calculate() takes the standard's weights over those groups only.
#' @noRd
.dsr_truncate <- function(df, ages) {
  if (ages[1] == 1L && ages[2] == length(.dsr_age_labels)) return(df)
  keep <- is.na(df$AgeGroup) | df$AgeGroup %in% seq(ages[1], ages[2])
  df[keep, , drop = FALSE]
}

# The ages of a range of age groups in words, e.g. "under 65", "35 to 74" or
# "85 and over"; NULL for all ages
#' @noRd
.dsr_ages_text <- function(ages) {
  last <- length(.dsr_age_labels)
  from <- 5L * (ages[1] - 1L)
  to   <- 5L * ages[2] - 1L
  if (ages[1] == 1L && ages[2] == last) return(NULL)
  if (ages[2] == last) return(paste(from, "and over"))
  if (ages[1] == 1L) return(paste("under", to + 1L))
  paste(from, "to", to)
}

# Label of the areas of a level, with the ASGS edition for SA3 and SA2
#' @noRd
.dsr_area_label <- function(level) {
  label <- .dsr_levels[[level]]$label
  if (level == "state") label else paste0(label, " (", .dsr_asgs$short, ")")
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
# condition keeping the areas of the population. Events with no recorded
# residence are kept so that they can be counted and reported;
# dsr_calculate() leaves them out of the rates. ASGS codes start with the
# state code, 8 for the ACT, and SA2 codes start with their SA3 code. The
# states of the ACT and surrounds come from the SA3.
#' @noRd
.dsr_sql_area <- function(spec) {

  col  <- .dsr_levels[[spec$level]]$column
  keep <- function(col, cond) {
    paste0("\n      AND (", cond, " OR ", col, " IS NULL)")
  }

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
# settings, with a procedure in its procedure codes, blocks and ranges, or,
# when both are given, with both or either (`combine`). Diagnoses are searched
# in the principal diagnosis or in any diagnosis field (always any for ED,
# which has no principal diagnosis field), and procedures in the principal
# procedure (Procedure1, with its block in BlockProcedure1) or in any of them.
# Codes are compared without dots, hyphens or outer spaces. A range keeps
# codes from its first code up to its last, plus every code that starts with
# the last; a single code keeps every code that starts with it. Blocks are
# compared as numbers. Morphology codes (M8140/3) are skipped. Empty when
# there are no codes.
#' @noRd
.dsr_sql_codes <- function(spec) {

  ds <- .dsr_datasets[[spec$dataset]]

  # Tests of c.Code against codes and ranges, and rows of a VALUES list, k
  # to a line
  tests <- function(from, to) {
    ifelse(from == to,
           sprintf("c.Code LIKE '%s%%'", from),
           sprintf("(c.Code >= '%s' AND (c.Code <= '%s' OR c.Code LIKE '%s%%'))",
                   from, to, to))
  }
  rows <- function(values, k) {
    vapply(split(values, ceiling(seq_along(values) / k)), paste, character(1),
           collapse = ", ")
  }

  exists <- character(0)

  codes <- spec$codes
  if (!is.null(codes) && nrow(codes)) {
    fields <- if (spec$diagnosis == "all") ds$diagnoses else ds$diagnoses[1]
    exists <- c(exists, paste0(
      "EXISTS (",
      "\n          SELECT 1",
      "\n          FROM (VALUES ",
      paste(rows(paste0("(", fields, ")"), 5),
            collapse = ",\n                       "), ") AS d (Code)",
      "\n          CROSS APPLY (VALUES (REPLACE(LTRIM(RTRIM(d.Code)), '.', ''))) AS c (Code)",
      "\n          WHERE c.Code NOT LIKE '%/%'",
      "\n            AND (", paste(tests(codes$From, codes$To),
                                 collapse = "\n                 OR "), ")",
      "\n      )"
    ))
  }

  procs <- spec$procedures
  if (!is.null(procs) && nrow(procs) && length(ds$procedures)) {
    n <- if (spec$procedure == "all") length(ds$procedures) else 1L
    pairs <- paste0("(", ds$procedures[seq_len(n)], ", ", ds$blocks[seq_len(n)],
                    ")")
    code <- procs$Type == "code"
    blocks <- ifelse(procs$From == procs$To,
                     sprintf("p.Block = %s", procs$From),
                     sprintf("p.Block BETWEEN %s AND %s", procs$From, procs$To))
    exists <- c(exists, paste0(
      "EXISTS (",
      "\n          SELECT 1",
      "\n          FROM (VALUES ",
      paste(rows(pairs, 3), collapse = ",\n                       "),
      ") AS p (Code, Block)",
      "\n          CROSS APPLY (VALUES (REPLACE(REPLACE(LTRIM(RTRIM(p.Code)), '-', ''), '.', ''))) AS c (Code)",
      "\n          WHERE ", paste(c(tests(procs$From[code], procs$To[code]),
                                   blocks[!code]),
                                 collapse = "\n             OR "),
      "\n      )"
    ))
  }

  if (!length(exists)) return("")
  paste0(
    "\n      AND ", ds$icd10,
    "\n      AND ",
    if (length(exists) == 1) {
      exists
    } else if (spec$combine == "and") {
      paste(exists, collapse = "\n      AND ")
    } else {
      paste0("(", paste(exists, collapse = "\n        OR "), ")")
    }
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


# ICD-10-AM diagnosis codes and ACHI procedure codes for the code searches,
# each with its title, and for procedures its ACHI block, in the latest
# edition that holds it, and the first and last editions that hold it. The
# table stacks every edition it holds; editions are read from
# ICD10AMVersion, as its StartDate and EndDate are not reliable.
#' @noRd
.dsr_sql_icd <- function() {
  "SELECT VarType, Code, Description, ProcedureBlock, BlockDescription,
       FirstEdition, LastEdition
FROM (
    SELECT VarType, Code, Description, ProcedureBlock, BlockDescription,
           MIN(ICD10AMVersion) OVER (PARTITION BY VarType, Code) AS FirstEdition,
           ICD10AMVersion AS LastEdition,
           ROW_NUMBER() OVER (PARTITION BY VarType, Code
                              ORDER BY ICD10AMVersion DESC) AS Latest
    FROM Analysis.dbo.ICD10AM
    WHERE VarType IN ('Diagnosis', 'Procedure')
) AS d
WHERE Latest = 1"
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

# Years the slider selects to begin with: the default year and the two
# before it, within the years offered, so that both ends can be seen
#' @noRd
.dsr_default_years <- function(range) {
  c(max(range$min, range$default - 2L), range$default)
}


# ── Descriptions and code ───────────────────────────────────────────────────

# The events, population and areas of the results in words, with the years
# and the ages unless `years` or `ages` is FALSE, and the standard population
# when it is named
#' @noRd
.dsr_describe <- function(spec, standard_name = NULL, years = TRUE,
                          ages = TRUE) {
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
  level <- .dsr_area_label(spec$level)
  if (spec$level == "state") level <- tolower(level)
  ages  <- if (ages) .dsr_ages_text(spec$ages)
  # The diagnoses and procedures searched, e.g. "principal diagnosis J45 or
  # J46 and any procedure [1826]"
  either <- function(x) {
    if (length(x) > 1) {
      paste(paste(x[-length(x)], collapse = ", "), "or", x[length(x)])
    } else {
      x
    }
  }
  searched <- function(labels, scope, what) {
    if (length(labels)) {
      paste(if (scope == "all") "any" else "principal", what, either(labels))
    }
  }
  codes <- c(searched(spec$codes$Label, spec$diagnosis, "diagnosis"),
             searched(spec$procedures$Label, spec$procedure, "procedure"))
  diagnosis <- if (length(codes)) {
    paste0(", ", paste(codes, collapse = paste0(" ", spec$combine, " ")))
  }
  paste0(
    .dsr_datasets[[spec$dataset]]$label, diagnosis, ", ",
    .dsr_scopes[[spec$scope]]$describe,
    if (!is.null(ages)) paste0(" aged ", ages),
    ", by ", level, years,
    if (!is.null(standard_name)) paste0(", standardised to ", standard_name)
  )
}


# R code that reproduces the calculator's results
#' @noRd
.dsr_code <- function(spec, by_sex = FALSE, age_specific = FALSE,
                      multiplier = 100000, standard_name = NULL,
                      suppress = 0, min_events = 0, min_population = 0) {

  num <- function(x) format(x, scientific = FALSE)
  args <- paste0(
    "  by = c(\"", .dsr_time(spec), "\", \"Area\"), by_sex = ",
    if (by_sex) "TRUE" else "FALSE",
    ", multiplier = ", num(multiplier),
    if (suppress > 0) paste0(", suppress = ", num(suppress)),
    "\n"
  )
  # The minimums for presenting a DSR apply to dsr_calculate() only
  minimums <- c(if (min_events > 0) paste0("min_events = ", num(min_events)),
                if (min_population > 0) {
                  paste0("min_population = ", num(min_population))
                })
  dsr_args <- if (length(minimums)) {
    paste0(sub("\n$", ",\n", args), "  ", paste(minimums, collapse = ", "),
           "\n")
  } else {
    args
  }

  # Same rows as .dsr_truncate()
  truncation <- if (!is.null(.dsr_ages_text(spec$ages))) {
    groups <- if (spec$ages[1] == spec$ages[2]) {
      spec$ages[1]
    } else {
      paste0(spec$ages[1], ":", spec$ages[2])
    }
    paste0(
      "# Ages ", .dsr_ages_text(spec$ages), ", age groups ", groups,
      ". Events with no recorded age\n",
      "# stay in, to be reported as left out of the rates.\n",
      "events     <- events[is.na(events$AgeGroup) | events$AgeGroup %in% ",
      groups, ", ]\n",
      "population <- population[population$AgeGroup %in% ", groups, ", ]\n\n"
    )
  }

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
    truncation,
    pooling,
    "rates <- actepir::dsr_calculate(\n",
    "  events, population, standard,\n",
    dsr_args,
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
  # The reason a DSR is not presented rides in a hidden last column
  note <- !age && !is.null(df$DSRNote)
  df <- df[, c(id_cols, "Events", "Population", rate_cols,
               if (note) "DSRNote")]

  heads <- c(Year = "Year", Period = "Period", Area = area_label, Sex = "Sex",
             AgeGroupName = "Age group")[id_cols]
  sketch <- htmltools::tags$table(
    htmltools::tags$thead(
      htmltools::tags$tr(
        lapply(c(heads, "Events", "Population"),
               function(h) htmltools::tags$th(rowspan = 2, h)),
        lapply(rate_heads,
               function(h) htmltools::tags$th(colspan = 3, h,
                                               style = "text-align: center;")),
        if (note) htmltools::tags$th(rowspan = 2, "Note")
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
  # A DSR that is not presented reads n.p., with the reason on pointing
  dsr <- DT::JS(sprintf(
    "function(data, type, row) {
       var note = row[%d];
       if (data === null) {
         if (type !== 'display') return -1;
         return note ? '<span title=\"' + note + '\">n.p.</span>' : '';
       }
       return type === 'display' ?
         DTWidget.formatRound(data, %d, 3, ',', '.', null) : data;
     }", ncol(df) - 1L, digits))
  dsr_cols <- intersect(c("DSR", "DSRLower", "DSRUpper"), rate_cols)

  defs <- list(list(targets = match("Events", names(df)) - 1L,
                    render = events))
  if (note) {
    defs <- c(defs, list(
      list(targets = match(dsr_cols, names(df)) - 1L, render = dsr),
      list(targets = ncol(df) - 1L, visible = FALSE)
    ))
  }

  tbl <- DT::datatable(
    df,
    container = sketch,
    rownames  = FALSE,
    selection = "none",
    class     = "compact stripe hover",
    style     = "bootstrap5",
    options   = list(
      dom        = if (long) "ftip" else "ft",
      paging     = long,
      pageLength = 100,
      scrollX    = TRUE,
      autoWidth  = FALSE,
      columnDefs = defs
    )
  )
  tbl <- DT::formatRound(tbl, "Population", digits = 0, mark = ",")
  DT::formatRound(tbl, if (note) setdiff(rate_cols, dsr_cols) else rate_cols,
                  digits = digits, mark = ",")

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
  # A DSR that is not presented reads n.p.
  out <- c(out, lapply(names(rates), function(col) {
    x <- ifelse(is.na(df[[col]]), "",
                formatC(df[[col]], format = "f", digits = digits))
    if (startsWith(col, "DSR") && !is.null(df$DSRNote)) {
      x[!is.na(df$DSRNote)] <- "n.p."
    }
    x
  }))

  lines <- c(paste(c(heads, "Events", "Population", rates), collapse = "\t"),
             do.call(paste, c(out, sep = "\t")))
  paste0(paste(lines, collapse = "\n"), "\n")

}


# Events and populations by age group, with the age groups across: one row of
# each for every group and sex, from the same counts as the rates
# (.dsr_strata()), and their total over the ages. Events below `suppress`,
# zero included, are NA, as in the rates, in each age group and in the total.
#' @noRd
.dsr_counts <- function(events, population, by, by_sex = FALSE,
                        suppress = 0) {

  d    <- .dsr_strata(events, population, by, by_sex)$data
  ages <- sort(unique(d$AgeGroup))
  keys <- unique(d[c(by, "Sex")])
  keys <- keys[.dsr_order(keys, c(by, "Sex")), , drop = FALSE]
  k    <- .dsr_keys(d, keys, c(by, "Sex"))
  cell <- cbind(match(k$x, k$y), match(d$AgeGroup, ages))

  # Rows alternate events and population; an age group with no population
  # row counts 0 of each
  n <- nrow(keys)
  counts <- matrix(0, 2 * n, length(ages),
                   dimnames = list(NULL, .dsr_age_labels[ages]))
  counts[cbind(2 * cell[, 1] - 1, cell[, 2])] <- d$Events
  counts[cbind(2 * cell[, 1], cell[, 2])]     <- d$Population
  total <- rowSums(counts)

  is_events <- rep(c(TRUE, FALSE), n)
  counts[counts < suppress & is_events] <- NA
  total[total < suppress & is_events] <- NA

  out <- keys[rep(seq_len(n), each = 2), , drop = FALSE]
  out$Count <- rep(c("Events", "Population"), n)
  out <- cbind(out, as.data.frame(counts, check.names = FALSE))
  out[["All ages"]] <- total
  rownames(out) <- NULL
  out

}


# DataTable of .dsr_counts(): suppressed events read "<suppress", and
# populations under min_population and event totals under min_events, the
# minimums for a DSR, are highlighted, each in its own colour
#' @noRd
.dsr_counts_datatable <- function(df, area_label, suppress = 0,
                                  min_events = 0, min_population = 0) {

  time  <- if ("Period" %in% names(df)) "Period" else "Year"
  ages  <- setdiff(names(df), c(time, "Area", "Sex", "Count", "All ages"))
  heads <- c(time, area_label, "Sex", "Count")
  sketch <- htmltools::tags$table(
    htmltools::tags$thead(
      htmltools::tags$tr(
        lapply(heads, function(h) htmltools::tags$th(rowspan = 2, h)),
        htmltools::tags$th(colspan = length(ages), "Age group",
                           style = "text-align: center;"),
        htmltools::tags$th(rowspan = 2, "All ages")
      ),
      htmltools::tags$tr(lapply(ages, htmltools::tags$th))
    )
  )

  # row[3], the Count column, says whether the row is events or population. A
  # cell is highlighted when it is a population under min_population (in an
  # age group) or an event total under min_events, suppressed totals included
  # when the suppression threshold is no higher than min_events.
  count <- function(total) {
    DT::JS(sprintf(
      "function(data, type, row) {
         var events = row[3] === 'Events';
         if (type !== 'display') return data === null ? -1 : data;
         var text = data === null ? '&lt;%s' :
           DTWidget.formatRound(data, 0, 3, ',', '.', null);
         var short = %s ?
           (events && %s > 0 && (data === null ? %s <= %s : data < %s)) :
           (!events && %s > 0 && data < %s);
         if (!short) return text;
         return '<span class=\"dsr-short dsr-short-' +
           (events ? 'events' : 'pop') + '\">' + text + '</span>';
       }",
      .fmt_count(suppress), if (total) "true" else "false",
      min_events, suppress, min_events, min_events,
      min_population, min_population))
  }

  long <- nrow(df) > 100
  DT::datatable(
    df,
    container = sketch,
    rownames  = FALSE,
    selection = "none",
    class     = "compact stripe hover",
    style     = "bootstrap5",
    options   = list(
      dom        = if (long) "ftip" else "ft",
      paging     = long,
      pageLength = 100,
      ordering   = FALSE,
      scrollX    = TRUE,
      autoWidth  = FALSE,
      columnDefs = list(
        list(targets = match(ages, names(df)) - 1L, render = count(FALSE),
             className = "dt-right"),
        list(targets = ncol(df) - 1L, render = count(TRUE),
             className = "dt-right")
      )
    )
  )

}


# .dsr_counts() as tab-separated text for pasting into a spreadsheet, with
# suppressed events written as "<suppress"
#' @noRd
.dsr_counts_tsv <- function(df, area_label, suppress = 0) {

  time <- if ("Period" %in% names(df)) "Period" else "Year"
  cols <- setdiff(names(df), c(time, "Area", "Sex", "Count"))
  # An en dash keeps Excel from reading 5-9 or 2016-2018 as a date
  dash <- function(x) gsub("-", "\u2013", x, fixed = TRUE)

  out <- list(dash(as.character(df[[time]])), as.character(df$Area),
              df$Sex, df$Count)
  out <- c(out, lapply(cols, function(col) {
    ifelse(is.na(df[[col]]), paste0("<", .fmt_count(suppress)),
           format(df[[col]], scientific = FALSE, trim = TRUE))
  }))

  lines <- c(paste(c(time, area_label, "Sex", "Count", dash(cols)),
                   collapse = "\t"),
             do.call(paste, c(out, sep = "\t")))
  paste0(paste(lines, collapse = "\n"), "\n")

}


# A box for diagnosis or procedure codes. A code, block or range is added as
# typed, when it matches create_filter (a JavaScript regular expression).
# Other text lists the codes whose titles match it (.dsr_icd_search()),
# fetched from the URL the server sends for the box, and Enter or Tab adds
# the one highlighted. Suggestions are matched again in the browser on their
# words, and none are listed while the box is empty, as it would otherwise
# list every suggestion fetched so far. Tags show the code, with the title on
# hover.
#' @noRd
.dsr_codes_box <- function(id, label, placeholder, create_filter) {
  close_if_empty <- I(
    "function() { if (!this.$control_input.val().trim()) this.close(); }"
  )
  shiny::selectizeInput(
    id, label,
    choices = NULL, multiple = TRUE, width = "100%",
    options = list(
      create           = TRUE,
      createOnBlur     = TRUE,
      createFilter     = I(create_filter),
      addPrecedence    = TRUE,
      selectOnTab      = TRUE,
      persist          = FALSE,
      delimiter        = ",",
      splitOn          = I("/\\s*[,;]+\\s*/"),
      plugins          = list("remove_button"),
      placeholder      = placeholder,
      valueField       = "value",
      labelField       = "value",
      searchField      = "words",
      sortField        = "rank",
      maxOptions       = 100,
      openOnFocus      = FALSE,
      closeAfterSelect = TRUE,
      load = I("function(query, callback) {
        var url = (window.dsrSearchUrls || {})[this.$input.attr('id')];
        if (!url || !query.trim()) return callback();
        $.getJSON(url, {q: query})
          .done(callback).fail(function() { callback(); });
      }"),
      score = I("function(search) {
        var words = search.toLowerCase().normalize('NFD')
          .replace(/[\\u0300-\\u036f]/g, '').replace(/\\./g, '')
          .replace(/(\\d{5})-(?=\\d)/g, '$1')
          .replace(/[^a-z0-9]+/g, ' ').trim().split(' ').filter(Boolean);
        return function(item) {
          if (!words.length || !item.words) return 0;
          for (var i = 0; i < words.length; i++) {
            if (item.words.indexOf(' ' + words[i]) < 0) return 0;
          }
          return 1;
        };
      }"),
      onType         = close_if_empty,
      onDropdownOpen = close_if_empty,
      render = I("{
        option: function(item, escape) {
          return '<div class=\"option\"><span class=\"dsr-code\">' +
            escape(item.value) + '</span> ' + escape(item.title || '') +
            '</div>';
        },
        item: function(item, escape) {
          return '<div title=\"' + escape(item.title || '') + '\">' +
            escape(item.value) + '</div>';
        }
      }")
    )
  )
}


# Settings of the Explore tab's map and funnel plot: what to show, the year
# or period, sex and the ages of age-specific rates, with input IDs that
# start with id
#' @noRd
.dsr_map_controls <- function(id) {
  name <- function(x) paste0(id, "_", x)
  shiny::tagList(
    shiny::div(
      class = "selector-row dsr-row",
      shiny::selectInput(
        name("value"), "Show",
        choices = c("Age-standardised rate" = "DSR",
                    "Crude rate" = "Crude",
                    "Age-specific rate" = "AgeRate",
                    "Events" = "Events"),
        width = "100%"
      ),
      shiny::selectInput(name("year"), "Year", choices = NULL,
                         width = "100%"),
      shiny::selectInput(name("sex"), "Sex", choices = "Persons",
                         width = "100%")
    ),
    shiny::conditionalPanel(
      sprintf("input.%s == 'AgeRate'", name("value")),
      shiny::sliderInput(
        name("ages"), "Ages mapped",
        min = 1, max = length(.dsr_age_labels),
        value = c(1, length(.dsr_age_labels)),
        step = 1, ticks = FALSE, width = "100%"
      )
    )
  )
}


# An area's figures in words, from its row of the results, or of the rates
# for an age range when band is TRUE
#' @noRd
.dsr_area_text <- function(row, band, suppress, multiplier) {
  digits <- if (multiplier >= 100000) 1 else 2
  num <- function(v) formatC(v, format = "f", digits = digits, big.mark = ",")
  if (!nrow(row)) {
    "no results."
  } else if (isTRUE(row$Suppressed[1])) {
    paste0("fewer than ", .fmt_count(suppress),
           " events (suppressed); population ",
           .fmt_count(row$Population[1]), ".")
  } else if (band) {
    paste0(.fmt_count(row$Events[1]), " events, population ",
           .fmt_count(row$Population[1]), ", rate ",
           if (is.na(row$Crude[1])) "not calculated" else paste0(
             num(row$Crude[1]), " (95% CI ", num(row$CrudeLower[1]),
             " to ", num(row$CrudeUpper[1]), ")"),
           ".")
  } else {
    paste0(.fmt_count(row$Events[1]), " events, population ",
           .fmt_count(row$Population[1]), ", crude rate ",
           num(row$Crude[1]), ", DSR ",
           if (!is.null(row$DSRNote) && !is.na(row$DSRNote[1])) {
             paste0("not presented (", row$DSRNote[1], ")")
           } else if (is.na(row$DSR[1])) "not calculated" else paste0(
             num(row$DSR[1]), " (95% CI ", num(row$DSRLower[1]), " to ",
             num(row$DSRUpper[1]), ")"),
           ".")
  }
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
      !requireNamespace("bslib", quietly = TRUE) ||
      !requireNamespace("DT", quietly = TRUE)) {
    stop(
      "The 'shiny', 'bslib', and 'DT' packages are required for episerver_dsr().\n",
      "Install them with: install.packages(c('shiny', 'bslib', 'DT'))",
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

  # ICD-10-AM diagnosis and ACHI procedure titles for the code searches, by
  # the input ID of their box. NULL when they cannot be read, and codes are
  # then typed without suggestions.
  icd <- try_cached("icd", .dsr_sql_icd())
  titles <- list(codes      = .dsr_icd_titles(icd, "Diagnosis"),
                 procedures = .dsr_icd_titles(icd, "Procedure"))

  # Initial year range for the default settings; the slider follows the
  # settings once the app is running
  this_year <- as.integer(format(Sys.Date(), "%Y"))
  init <- year_range("ED", "ACT", "state")
  if (is.null(init)) {
    init <- list(min = this_year - 10L, max = this_year, default = this_year - 1L)
  }

  code_button_label <- if (in_rstudio) "Insert Code" else "Copy Code"

  # ── UI ────────────────────────────────────────────────────────────────────
  # The settings are in a sidebar, in sections that open and close, and the
  # results in tabs on the top bar, so that the results have the rest of the
  # window
  ui <- bslib::page_navbar(

    title        = shiny::span(class = "epi-brand",
                               .epi_logo(class = "epi-logo"), "DSR Calculator"),
    window_title = "DSR Calculator",
    id           = "view",
    theme        = .epi_theme(),
    fillable     = FALSE,

    header = shiny::tagList(

      .epi_app_style(),

      # Calculator-only rules: the top bar, the sidebar, wrapping selector
      # rows, notes, the error bar, table headings and the diagnosis code tags
      shiny::tags$style(shiny::HTML("
        /* Top bar: the logo at the brand's minimum size, a crest 60px
           across, with clear space equal to the height of its ACT
           lettering, 22px */
        .navbar { --bs-navbar-padding-y: 0; }
        .navbar > .container-fluid { padding-left: 22px; padding-right: 22px; }
        .navbar-brand { display: flex; padding: 22px 0; margin-right: 22px; }
        .epi-brand { display: flex; align-items: center; gap: 22px; }
        .epi-logo { height: 60px; }
        /* Sidebar: Lavender tint, with Calculate and Copy Code in view at
           its foot across its full width. The bar stays above what
           scrolls under it, such as a focused section heading (z-index 3)
           and slider handles, and below open dropdowns (1000). */
        :root { --bslib-sidebar-bg: #f7f4fd; }
        .sidebar .accordion { --bs-accordion-bg: transparent; }
        .dsr-actions {
          --dsr-pad: var(--bslib-sidebar-padding, var(--bslib-spacer, 1.5rem));
          position: sticky; bottom: 0; z-index: 20;
          display: flex; gap: 8px;
          margin: 0 calc(-1 * var(--dsr-pad));
          padding: 10px var(--dsr-pad);
          background-color: var(--bslib-sidebar-bg);
          border-top: 1px solid #e6e5e5;
        }
        .dsr-row { flex-wrap: wrap; }
        .dsr-row > * { min-width: 200px; }
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
        .selectize-control.multi .selectize-input > div {
          background: var(--epi-info-bg); color: var(--epi-primary);
          border-radius: 3px;
        }
        .dsr-code { font-weight: 600; color: var(--epi-primary); }
        .dsr-codes div { margin-bottom: 2px; }
        .radio.disabled label { color: #999; }
        .explore-row { display: flex; flex-wrap: wrap; gap: 12px; }
        .explore-panel { flex: 1 1 400px; min-width: 0; }
        .explore-panel .ex-linked { stroke: #320557 !important;
                                    stroke-width: 2px !important; }
        /* The Download menu's choices in the brand purples */
        .dropdown-menu {
          --bs-dropdown-link-hover-bg: var(--epi-info-bg);
          --bs-dropdown-link-active-bg: var(--epi-accent);
        }
        /* The plots' toolbars in Iris, Nightshade on hover, in place of
           ggiraph's blue; lasso deselection stays red */
        .explore-panel .ggiraph-toolbar-icon.neutral {
          stroke: var(--epi-accent); fill: var(--epi-accent);
        }
        .explore-panel .ggiraph-toolbar-icon.neutral:hover {
          stroke: var(--epi-primary); fill: var(--epi-primary);
        }
        .ex-table { font-size: 12px; border-collapse: collapse;
                    margin: 6px 0 10px 0; }
        .ex-table th, .ex-table td { padding: 3px 8px;
                                     border-bottom: 1px solid #e6e5e5; }
        .ex-table th { color: var(--epi-primary); font-weight: 600;
                       text-align: left; }
        .ex-table td.num { text-align: right; }
        .help { max-width: 900px; font-size: 13px; color: #333740; }
        .help h5 { color: var(--epi-primary); font-weight: 600;
                   margin: 18px 0 6px 0; }
        .help h5:first-child { margin-top: 4px; }
        .help-scroll { overflow-x: auto; }
        .help-table td, .help-table th { vertical-align: top; }
        .help-table code { white-space: normal; overflow-wrap: break-word; }
        .help-formula { background: #f7f4fd; border-left: 3px solid #DCC8FA;
                        padding: 4px 10px; margin: 4px 0 8px 0;
                        font-family: Georgia, 'Times New Roman', serif; }
        .help-refs li { margin-bottom: 4px; }
        /* Counts by age: below a minimum for a DSR. Populations in
           Tuggeranong yellow, event totals in Inner North blue, each mixed
           75% with white. */
        .dsr-short { padding: 1px 4px; border-radius: 2px; }
        .dsr-short-pop { background: #F1ECC2; }
        .dsr-short-events { background: #BFE3F1; }
        /* Shown in place of Shiny's grey overlay once Done stops the app */
        .epi-stopped { position: fixed; inset: 0; z-index: 100000;
                       display: flex; flex-direction: column;
                       align-items: center; justify-content: center;
                       background: #fff; color: var(--epi-primary); }
      ")),

      # Clipboard handler for the code and table copy buttons
      .epi_copy_script(),

      # Where the codes boxes fetch their suggestions, by input ID
      shiny::tags$script(shiny::HTML("
        Shiny.addCustomMessageHandler('dsr_search_urls', function(urls) {
          window.dsrSearchUrls = urls;
        });
      ")),

      # Slider labels stay within their slider, clear of the one beside it,
      # and the age sliders, which hold age group codes (1 to 18), show the
      # age groups. Shiny starts each slider when it binds the input, and puts
      # number labels back each time the server updates it, just after this
      # event.
      shiny::tags$script(shiny::HTML(sprintf("
        (function() {
          var labels = %s;
          function setUp(el) {
            var slider = $(el).data('ionRangeSlider');
            var ids = ['years', 'ages', 'ex_ages'];
            if (!slider || ids.indexOf(el.id) < 0) {
              return;
            }
            var options = {force_edges: true};
            if (el.id !== 'years') {
              options.prettify_enabled = true;
              options.prettify = function(n) { return labels[n - 1]; };
            }
            slider.update(options);
          }
          $(document).on('shiny:bound', function(e) { setUp(e.target); });
          $(document).on('shiny:updateinput', function(e) {
            setTimeout(function() { setUp(e.target); }, 0);
          });
        })();
      ", jsonlite::toJSON(.dsr_age_labels)))),

      # Pointing at an area in the Explore tab's map or funnel plot outlines
      # it in both, matched on the data-id that ggiraph gives each area
      shiny::tags$script(shiny::HTML("
        (function() {
          var marks = '#ex_map [data-id], #ex_funnel [data-id]';
          $(document).on('mouseover', marks, function() {
            var id = this.getAttribute('data-id');
            $(marks).filter(function() {
              return this.getAttribute('data-id') === id;
            }).addClass('ex-linked');
          });
          $(document).on('mouseout', marks, function() {
            $('.ex-linked').removeClass('ex-linked');
          });
        })();
      ")),

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

      # A dataset without procedures (ED) disables the procedure codes and
      # their options; their entries stay for the other dataset
      shiny::tags$script(shiny::HTML(sprintf("
        (function() {
          var none = %s;
          function sync() {
            var box = $('#procedures')[0];
            if (!box || !box.selectize) return;
            var off = none.indexOf($('#dataset').val()) >= 0;
            if (off) box.selectize.disable(); else box.selectize.enable();
            $('input[name=procedure], input[name=combine]')
              .prop('disabled', off).closest('.radio')
              .toggleClass('disabled', off);
          }
          $(document).on('change', '#dataset', sync);
          $(document).on('shiny:bound', function(e) {
            if (e.target.id === 'procedures') sync();
          });
          $(sync);
        })();
      ", jsonlite::toJSON(names(Filter(function(d) !length(d$procedures),
                                       .dsr_datasets)))))),

      # Done stops the app. A browser tab stays open afterwards, so it says
      # so instead of greying out.
      shiny::tags$script(shiny::HTML("
        (function() {
          var stopping = false;
          $(document).on('click', '#done', function() { stopping = true; });
          $(document).on('shiny:disconnected', function() {
            if (!stopping) return;
            $('<div class=\"epi-stopped\">')
              .append('<h4>The DSR Calculator has stopped.</h4>')
              .append('<p>Start it again with actepir::episerver_dsr().</p>')
              .appendTo('body');
          });
        })();
      ")),

      # Info bar: what is shown, what was left out, and any warnings
      shiny::conditionalPanel("input.view !== 'help'",
                              shiny::uiOutput("info_bar"))
    ),

    # Not resizable: the resize handle would cover the sidebar's scrollbar
    sidebar = bslib::sidebar(
      id = "settings", width = 340, open = "desktop", resizable = FALSE,
      bslib::accordion(
        id = "settings_sections", open = TRUE, multiple = TRUE,
        bslib::accordion_panel(
          "Events", value = "events",
          shiny::selectInput(
            "dataset", "Dataset",
            choices = c("ED presentations" = "ED", "APC separations" = "APC"),
            width = "100%"
          ),
          .dsr_codes_box(
            "codes", "Diagnosis codes", "All events. Type a code or condition",
            paste0("/^\\s*[a-z](\\d[\\da-z.]*)?[*%]?(\\s*(-|\\u2013|\\u2014|",
                   "\\s+to\\s+)\\s*([a-z][\\da-z.]*[*%]?)?)?\\s*$/i")
          ),
          shiny::uiOutput("codes_note"),
          shiny::radioButtons(
            "diagnosis", "Diagnosis search",
            choices = c("Principal diagnosis" = "principal",
                        "Any diagnosis" = "all")
          ),
          .dsr_codes_box(
            "procedures", "Procedure codes",
            "All events. Type a code or procedure",
            paste0("/^\\s*(block\\s*)?\\[?\\d[\\d\\s\\[\\]*%\\u2013\\u2014-]*",
                   "(\\s+to\\s+(block\\s*)?\\[?\\d[\\d\\s\\[\\]*%-]*)?$/i")
          ),
          shiny::uiOutput("procedures_note"),
          shiny::radioButtons(
            "procedure", "Procedure search",
            choices = c("Principal procedure" = "principal",
                        "Any procedure" = "all")
          ),
          shiny::radioButtons(
            "combine", "With diagnoses and procedures, count events with",
            choices = c("a diagnosis and a procedure" = "and",
                        "a diagnosis or a procedure" = "or")
          )
        ),
        bslib::accordion_panel(
          "Population", value = "population",
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
              vapply(names(.dsr_levels), .dsr_area_label, character(1))
            ),
            width = "100%"
          )
        ),
        bslib::accordion_panel(
          "Time periods", value = "time",
          shiny::sliderInput(
            "years", "Calendar years",
            min = init$min, max = init$max,
            value = .dsr_default_years(init),
            step = 1, sep = "", ticks = FALSE, width = "100%"
          ),
          shiny::selectInput(
            "pool", "Pool years",
            choices = c("None" = "1", "2 years" = "2", "3 years" = "3",
                        "4 years" = "4", "5 years" = "5",
                        "All selected years" = "all"),
            width = "100%"
          ),
          shiny::uiOutput("years_note")
        ),
        bslib::accordion_panel(
          "Rate details", value = "rate",
          shiny::sliderInput(
            "ages", "Ages",
            min = 1, max = length(.dsr_age_labels),
            value = c(1, length(.dsr_age_labels)),
            step = 1, ticks = FALSE, width = "100%"
          ),
          shiny::selectInput(
            "standard", "Standard population",
            choices  = std_choices,
            selected = if ("101" %in% std_choices) "101" else std_choices[1],
            width = "100%"
          ),
          shiny::selectInput(
            "multiplier", "Rate per",
            choices  = c("1,000" = "1000", "10,000" = "10000",
                         "100,000" = "100000"),
            selected = "100000",
            width = "100%"
          ),
          shiny::numericInput(
            "suppress", "Suppress counts below",
            value = 5, min = 0, step = 1, width = "100%"
          ),
          shiny::numericInput(
            "min_events", "Minimum events for a DSR",
            value = 20, min = 0, step = 1, width = "100%"
          ),
          shiny::numericInput(
            "min_population", "Minimum population per age group",
            value = 30, min = 0, step = 1, width = "100%"
          ),
          shiny::div(
            class = "epi-note",
            "Below these, age-standardised rates are not presented (n.p.), ",
            "as the AIHW (2011) principles advise. 0 turns a check off."
          ),
          shiny::checkboxInput("by_sex", "Male and female", value = FALSE)
        )
      ),
      shiny::div(
        class = "dsr-actions",
        shiny::actionButton(
          "calculate", "Calculate",
          icon  = shiny::icon("calculator"),
          class = "btn-sm btn-primary"
        ),
        shiny::actionButton(
          "insert_code", code_button_label,
          icon  = shiny::icon("terminal"),
          class = "btn-sm btn-primary"
        )
      )
    ),

    bslib::nav_panel(
      "Tables", value = "tables",
      # The rates or the age-specific rates, with a button that copies the
      # table shown
      bslib::navset_pill(
        id = "table",
        bslib::nav_panel("Rates", value = "rates",
                         DT::dataTableOutput("summary_table")),
        bslib::nav_panel("Age-specific rates", value = "age",
                         DT::dataTableOutput("age_table")),
        bslib::nav_panel("Counts by age", value = "counts",
                         shiny::uiOutput("counts_note"),
                         DT::dataTableOutput("counts_table")),
        bslib::nav_spacer(),
        bslib::nav_item(
          shiny::actionButton("copy_table", "Copy table",
                              icon = shiny::icon("copy"),
                              class = "btn-sm btn-primary")
        )
      ),
      shiny::uiOutput("footnote")
    ),

    bslib::nav_panel(
      "Explore", value = "explore",
      .dsr_map_controls("ex"),
      shiny::div(
        class = "options-row",
        shiny::selectInput(
          "ex_colour", "Colour the map by",
          choices = c("Value" = "value",
                      "Position in the funnel plot" = "position"),
          width = "220px"
        ),
        shiny::checkboxInput("ex_overdispersion",
                             "Allow for overdispersion", value = TRUE,
                             width = "auto"),
        shiny::actionButton("ex_zoom", "Zoom to selected",
                            icon = shiny::icon("magnifying-glass-plus"),
                            class = "btn-sm btn-primary"),
        shiny::actionButton("ex_reset", "Show all",
                            icon = shiny::icon("expand"),
                            class = "btn-sm btn-primary"),
        shiny::actionButton("ex_clear", "Clear selection",
                            icon = shiny::icon("xmark"),
                            class = "btn-sm btn-primary"),
        # Download: a Bootstrap dropdown of the four files
        shiny::div(
          class = "dropdown",
          shiny::tags$button(
            id = "ex_download", type = "button",
            class = "btn btn-sm btn-primary dropdown-toggle",
            `data-bs-toggle` = "dropdown", `aria-expanded` = "false",
            shiny::icon("download"), "Download"
          ),
          shiny::tags$ul(
            class = "dropdown-menu",
            shiny::tags$li(shiny::downloadLink("ex_map_pdf", "Map as PDF",
                                               class = "dropdown-item")),
            shiny::tags$li(shiny::downloadLink("ex_map_png", "Map as PNG",
                                               class = "dropdown-item")),
            shiny::tags$li(shiny::tags$hr(class = "dropdown-divider")),
            shiny::tags$li(shiny::downloadLink("ex_funnel_pdf",
                                               "Funnel plot as PDF",
                                               class = "dropdown-item")),
            shiny::tags$li(shiny::downloadLink("ex_funnel_png",
                                               "Funnel plot as PNG",
                                               class = "dropdown-item"))
          )
        )
      ),
      shiny::uiOutput("ex_note"),
      shiny::uiOutput("ex_heading"),
      shiny::div(
        class = "explore-row",
        shiny::div(class = "explore-panel",
                   ggiraph::girafeOutput("ex_map")),
        shiny::div(class = "explore-panel",
                   ggiraph::girafeOutput("ex_funnel"))
      ),
      shiny::uiOutput("ex_method"),
      shiny::uiOutput("ex_selected")
    ),

    bslib::nav_panel("Help", value = "help", .dsr_help()),

    bslib::nav_spacer(),
    bslib::nav_item(
      shiny::actionButton("done", "Done", class = "btn-sm btn-primary")
    )
  )

  # ── Server ────────────────────────────────────────────────────────────────
  server <- function(input, output, session) {

    rv <- shiny::reactiveValues(fetched = NULL, error = NULL)

    # The codes boxes fetch their suggestions from these URLs
    ids <- names(Filter(Negate(is.null), titles))
    urls <- lapply(stats::setNames(ids, ids), function(id) {
      session$registerDataObj(paste0(id, "_search"), titles[[id]],
                              .dsr_icd_response)
    })
    session$sendCustomMessage("dsr_search_urls", urls)

    # Why the diagnosis or procedure codes cannot be read, or NULL
    codes_error <- shiny::reactive({
      tryCatch({
        .dsr_parse_codes(input$codes)
        NULL
      }, error = function(e) conditionMessage(e))
    })
    procedures_error <- shiny::reactive({
      tryCatch({
        .dsr_parse_procedures(input$procedures)
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
                              pool = pool(),
                              ages = if (is.null(input$ages)) {
                                c(1L, length(.dsr_age_labels))
                              } else {
                                input$ages
                              },
                              procedures = input$procedures,
                              procedure = if (is.null(input$procedure)) {
                                "principal"
                              } else {
                                input$procedure
                              },
                              combine = if (is.null(input$combine)) {
                                "and"
                              } else {
                                input$combine
                              }),
             error = NULL),
        error = function(e) list(spec = NULL, error = conditionMessage(e))
      )
    })
    spec <- shiny::reactive(settings()$spec)

    # A cleared or negative threshold means no suppression, or no check of the
    # minimums for a DSR, which the info bar then reflects. Counts are whole
    # numbers, so a fractional threshold is rounded up to the equivalent
    # whole one.
    threshold <- function(s) {
      if (is.null(s) || is.na(s) || s < 0) 0 else ceiling(s)
    }
    suppress       <- shiny::reactive(threshold(input$suppress))
    min_events     <- shiny::reactive(threshold(input$min_events))
    min_population <- shiny::reactive(threshold(input$min_population))

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
        sel <- .dsr_default_years(r)
      }
      sel <- pmin(pmax(sel, r$min), r$max)
      shiny::updateSliderInput(session, "years",
                               min = r$min, max = r$max, value = sel)
    }, ignoreNULL = FALSE)

    # Each code, block and range entered in a box, with its titles
    entered <- function(values, parse, icd) {
      if (is.null(icd) || !length(values)) return(NULL)
      d <- .dsr_icd_describe(parse(values), icd)
      shiny::div(class = "epi-note dsr-codes",
                 lapply(seq_len(nrow(d)), function(i) {
                   shiny::div(shiny::span(class = "dsr-code", d$Label[i]),
                              d$Text[i])
                 }))
    }

    output$codes_note <- shiny::renderUI({
      err <- codes_error()
      if (!is.null(err)) {
        return(shiny::div(class = "epi-note epi-error", err))
      }
      dataset <- if (is.null(input$dataset)) "ED" else input$dataset
      ds <- .dsr_datasets[[dataset]]
      shiny::tagList(
        entered(input$codes, .dsr_parse_codes, titles$codes),
        shiny::div(
          class = "epi-note",
          "Leave empty for all events. A partial code such as J45 matches ",
          "every code that starts with it, and a range such as C13-C15.45 ",
          "includes both ends. Only ICD-10 coded records are searched.",
          if (is.null(titles$codes)) {
            paste0(" Searching by condition is unavailable, as the ICD-10-AM ",
                   "titles could not be read from Analysis.dbo.ICD10AM.")
          } else {
            paste0(" Type words from a condition, such as myocardial, to ",
                   "choose from the codes whose titles contain them.")
          },
          if (!ds$principal) {
            paste0(" ", dataset, " searches all its diagnosis fields (",
                   paste(ds$diagnoses, collapse = ", "), "), as none of them ",
                   "holds the principal diagnosis in every year.")
          }
        )
      )
    })

    output$procedures_note <- shiny::renderUI({
      err <- procedures_error()
      if (!is.null(err)) {
        return(shiny::div(class = "epi-note epi-error", err))
      }
      dataset <- if (is.null(input$dataset)) "ED" else input$dataset
      ds <- .dsr_datasets[[dataset]]
      if (!length(ds$procedures)) {
        with <- Filter(function(d) length(d$procedures), .dsr_datasets)
        return(shiny::div(class = "epi-note", paste0(
          ds$label, " have no procedures, so procedure codes apply to ",
          paste(vapply(with, `[[`, character(1), "label"), collapse = " and "),
          " only."
        )))
      }
      shiny::tagList(
        entered(input$procedures, .dsr_parse_procedures, titles$procedures),
        shiny::div(
          class = "epi-note",
          "Leave empty for all events. A code such as 11012-00, or a partial ",
          "code such as 11012, matches every code that starts with it; a ",
          "block such as 1826, shown as [1826], matches the procedures in ",
          "that ACHI block; and a range such as 11000-00 to 11027-00 or ",
          "1820-1830 includes both ends.",
          if (is.null(titles$procedures)) {
            paste0(" Searching by procedure is unavailable, as the ACHI ",
                   "titles could not be read from Analysis.dbo.ICD10AM.")
          } else {
            paste0(" Type words from a procedure, such as electromyography, ",
                   "to choose from the codes and blocks whose titles contain ",
                   "them.")
          }
        )
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
        events     <- .dsr_pool(.dsr_truncate(f$events, s$ages), s)
        population <- .dsr_pool(.dsr_truncate(f$population, s$ages), s)
        by         <- c(.dsr_time(s), "Area")
        summary <- suppressMessages(dsr_calculate(
          events, population, standard,
          by = by, by_sex = isTRUE(input$by_sex),
          multiplier = mult, suppress = supp,
          min_events = min_events(), min_population = min_population()
        ))
        list(spec = s, summary = summary, events = events,
             population = population, standard = standard, by = by,
             multiplier = mult, suppress = supp,
             min_events = min_events(), min_population = min_population(),
             total = sum(events$Events), error = NULL)
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
      ages <- .dsr_ages_text(s$ages)
      notes <- list(shiny::p(
        shiny::strong(.dsr_describe(s, std_name(s$standard)), .noWS = "after"),
        paste0(". Rates per ", .fmt_count(r$multiplier),
               if (!is.null(ages)) paste0(" population aged ", ages), ".")
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

      if (nrow(s$codes) || nrow(s$procedures)) {
        missing <- c(
          if (nrow(s$codes)) {
            paste(if (s$diagnosis == "all") "diagnosis" else
                    "principal diagnosis", "recorded cannot match the diagnoses")
          },
          if (nrow(s$procedures)) {
            paste(if (s$procedure == "all") "procedure" else
                    "principal procedure", "recorded cannot match the procedures")
          }
        )
        notes <- c(notes, list(shiny::p(
          "Code search: ICD-10 coded records only; events with no ",
          paste(missing, collapse = ", and events with no "), "."
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

      # DSRs not presented: below the minimums, which the AIHW (2011)
      # principles advise, or undefined
      note <- r$summary$DSRNote
      undefined <- note %in% "events in an age group with no population"
      short <- note[!is.na(note) & !undefined]
      if (length(short)) {
        n <- table(factor(short, unique(short)))
        notes <- c(notes, list(shiny::p(class = "epi-warn", paste0(
          "Age-standardised rates are not presented (n.p.) for ",
          length(short), " of ", nrow(r$summary), " rows: ",
          paste0(n, " with ", names(n), collapse = ", "),
          ", as the AIHW (2011) principles advise. Pooling years or choosing ",
          "a larger area raises the counts; the crude and age-specific rates ",
          "are still shown."
        ))))
      }
      if (any(undefined)) {
        notes <- c(notes, list(shiny::p(class = "epi-warn", paste0(
          "The DSR could not be calculated for ", sum(undefined), " row(s) ",
          "with events in an age group that has no population."
        ))))
      }

      now <- spec()
      keys <- c("dataset", "scope", "level", "years", "codes", "diagnosis",
                "pool", "ages", "procedures", "procedure", "combine")
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
      .dsr_datatable(r$summary, .dsr_area_label(r$spec$level), r$multiplier,
                     suppress = r$suppress)
    })

    output$age_table <- DT::renderDataTable({
      r <- results()
      shiny::req(r, is.null(r$error))
      .dsr_datatable(age_results(), .dsr_area_label(r$spec$level),
                     r$multiplier, age = TRUE, suppress = r$suppress)
    })

    output$footnote <- shiny::renderUI({
      r <- results()
      shiny::req(r, is.null(r$error))
      shiny::div(
        class = "epi-note",
        style = "margin-top: 6px;",
        "Crude and age-specific rates have exact Poisson confidence intervals; ",
        "DSRs have Dobson et al. (1991) intervals. A blank rate is suppressed ",
        "or could not be calculated, and n.p. marks an age-standardised rate ",
        "that is not presented: point at it for the reason. Persons include ",
        "sex not stated and other sex."
      )
    })

    # Events and populations by age group, with the ages across
    counts <- shiny::reactive({
      r <- results()
      shiny::req(r, is.null(r$error))
      .dsr_counts(r$events, r$population, r$by, by_sex = isTRUE(input$by_sex),
                  suppress = r$suppress)
    })

    output$counts_table <- DT::renderDataTable({
      r <- results()
      shiny::req(r, is.null(r$error))
      .dsr_counts_datatable(counts(), .dsr_area_label(r$spec$level),
                            suppress = r$suppress, min_events = r$min_events,
                            min_population = r$min_population)
    })

    output$counts_note <- shiny::renderUI({
      r <- results()
      shiny::req(r, is.null(r$error))
      # Each in the colour it has in the table
      key <- function(type, text) {
        shiny::span(class = paste0("dsr-short dsr-short-", type), text,
                    .noWS = "outside")
      }
      short <- c(
        if (r$min_population > 0) {
          list(key("pop", paste0("populations under ",
                                 .fmt_count(r$min_population),
                                 " in an age group")))
        },
        if (r$min_events > 0) {
          list(key("events", paste0("event totals under ",
                                    .fmt_count(r$min_events))))
        }
      )
      shiny::div(
        class = "epi-note", style = "margin: 8px 0 6px 0;",
        "The events and population of each age group in the rates. ",
        if (length(short)) {
          shiny::tagList(
            "Highlighted: ",
            if (length(short) == 2) {
              list(short[[1]], " and ", short[[2]])
            } else {
              short
            },
            ", below the minimums for an age-standardised rate. "
          )
        },
        if (r$suppress > 0) {
          paste0("Events under ", .fmt_count(r$suppress), " are suppressed.")
        }
      )
    })

    # Copy table copies the table shown as tab-separated text, ready for Excel
    shiny::observeEvent(input$copy_table, {
      if (is.null(rv$fetched)) {
        shiny::showNotification("Press Calculate first.", duration = 3,
                                type = "warning")
        return(invisible(NULL))
      }
      r <- results()
      shiny::req(is.null(r$error))
      label <- .dsr_area_label(r$spec$level)
      text <- if (identical(input$table, "age")) {
        .dsr_tsv(age_results(), label, r$multiplier, age = TRUE,
                 suppress = r$suppress)
      } else if (identical(input$table, "counts")) {
        .dsr_counts_tsv(counts(), label, suppress = r$suppress)
      } else {
        .dsr_tsv(r$summary, label, r$multiplier, suppress = r$suppress)
      }
      .epi_copy_text(session, text, "Table copied to the clipboard")
    })

    # ── Explore: interactive map and funnel plot ───────────────────────────
    # The tab's settings are the inputs ex_* (.dsr_map_controls())

    # Ages mapped, kept within the ages calculated
    within_ages <- function(a, ages) {
      a <- if (is.null(a)) ages else as.integer(a)
      c(max(a[1], ages[1]), min(a[2], ages[2]))
    }
    ages_words <- function(ages) {
      words <- .dsr_ages_text(ages)
      if (is.null(words)) "all ages" else paste("aged", words)
    }

    # Areas are compared at SA3 and SA2 only: at state level the ACT is one
    # area, and the ACT and surrounds two blocks of SA3s
    comparable <- function(s) s$level != "state"

    # Boundaries are downloaded when the map is first shown for a level and
    # population, then kept for the session (and on disk by .dsr_boundaries)
    boundaries <- shiny::reactive({
      r <- results()
      shiny::req(r, is.null(r$error), comparable(r$spec))
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

    # The tab's settings: the rows of the results to show, the ages and rates
    # of age-specific rates, and the measure's label
    map_settings <- function(id) {

      name <- function(x) paste0(id, "_", x)
      get  <- function(x) input[[name(x)]]

      # Year or period, sex and ages mapped follow the results
      shiny::observeEvent(results(), {
        r <- results()
        shiny::req(r, is.null(r$error))
        times <- sort(unique(r$summary[[r$by[1]]]), decreasing = TRUE)
        sexes <- intersect(c("Persons", "Male", "Female"),
                           unique(r$summary$Sex))
        year <- get("year")
        sex  <- get("sex")
        shiny::updateSelectInput(
          session, name("year"), label = r$by[1], choices = times,
          selected = if (isTRUE(year %in% times)) year else times[1]
        )
        shiny::updateSelectInput(
          session, name("sex"), choices = sexes,
          selected = if (isTRUE(sex %in% sexes)) sex else "Persons"
        )
        ages <- r$spec$ages
        now  <- within_ages(get("ages"), ages)
        if (now[1] > now[2]) now <- ages
        shiny::updateSliderInput(session, name("ages"), min = ages[1],
                                 max = ages[2], value = now)
      })

      # Ages of age-specific rates
      ages <- shiny::reactive({
        r <- results()
        shiny::req(r, is.null(r$error))
        a <- within_ages(get("ages"), r$spec$ages)
        shiny::req(a[1] <= a[2])
        a
      })

      # Age-specific rates: the events and population of the ages mapped give
      # the rate for that age range, suppressed as in the tables
      band <- shiny::reactive({
        r <- results()
        shiny::req(r, is.null(r$error))
        a <- ages()
        suppressMessages(dsr_calculate(
          .dsr_truncate(r$events, a), .dsr_truncate(r$population, a),
          r$standard, by = r$by, by_sex = isTRUE(input$by_sex),
          multiplier = r$multiplier, suppress = r$suppress
        ))
      })

      # The rows of one year or period and sex, and the value shown
      values <- shiny::reactive({
        r <- results()
        shiny::req(r, is.null(r$error), comparable(r$spec), get("year"),
                   get("sex"), get("value"))
        is_band <- get("value") == "AgeRate"
        rates <- if (is_band) band() else r$summary
        d <- rates[as.character(rates[[r$by[1]]]) == get("year") &
                     rates$Sex == get("sex"), ]
        value <- if (is_band) "Crude" else get("value")
        list(r = r, rows = d, band = is_band,
             values = data.frame(Area = d$Area, Value = d[[value]],
                                 stringsAsFactors = FALSE))
      })

      # The age groups of the value shown: those mapped for age-specific
      # rates, otherwise those of the rates. Titles, axes, tooltips and file
      # names give them with the measure, and subtitles leave them out, so
      # the map states one age range whatever it shows.
      value_ages <- function(r) {
        if (get("value") == "AgeRate") ages() else r$spec$ages
      }
      # ... in words: "aged 20 to 44", "all ages" for age-specific rates of
      # every age, and NULL for other measures of every age
      ages_label <- function(r) {
        if (get("value") == "AgeRate") {
          ages_words(ages())
        } else if (!is.null(.dsr_ages_text(r$spec$ages))) {
          ages_words(r$spec$ages)
        }
      }

      # The measure shown, e.g. "Age-specific rate per 100,000, aged 20 to 44"
      label <- function(r) {
        per <- paste0(" per ", .fmt_count(r$multiplier))
        a   <- ages_label(r)
        paste0(switch(get("value"),
                      DSR     = paste0("Age-standardised rate", per),
                      Crude   = paste0("Crude rate", per),
                      AgeRate = paste0("Age-specific rate", per),
                      Events  = "Events"),
               if (!is.null(a)) paste0(", ", a))
      }

      title  <- function(r) {
        paste0(label(r), ", ", get("year"), ", ", tolower(get("sex")))
      }
      legend <- function() {
        switch(get("value"), DSR = "DSR", Crude = "Crude rate",
               AgeRate = "Rate", Events = "Events")
      }

      list(get = get, ages = ages, band = band, values = values,
           value_ages = value_ages, ages_label = ages_label, label = label,
           title = title, legend = legend)
    }

    # The note under a map on what grey areas are, and the boundaries' credit
    map_caption <- function(r, value) {
      grey <- c(
        if (r$suppress > 0) {
          paste0("fewer than ", .fmt_count(r$suppress), " events (suppressed)")
        },
        if (value == "DSR" && any(!is.na(r$summary$DSRNote))) {
          "DSR not presented (n.p.)"
        },
        "no value"
      )
      paste0(
        "Grey: ",
        if (length(grey) > 1) {
          paste(paste(grey[-length(grey)], collapse = ", "), "or",
                grey[length(grey)])
        } else {
          grey
        },
        ". Boundaries: ABS ", .dsr_asgs$name, ", CC BY 4.0."
      )
    }

    ex <- map_settings("ex")

    # Areas selected in either plot, shown selected in both, and the extent
    # the map is zoomed to (NULL for all its areas)
    ex_selected <- shiny::reactiveVal(character(0))
    ex_zoom     <- shiny::reactiveVal(NULL)
    shiny::observeEvent(results(), ex_zoom(NULL))

    # Funnel plot data for the tab's settings. Events are compared with those
    # expected from the age-specific rates of the areas not suppressed.
    ex_funnel <- shiny::reactive({
      m <- ex$values()
      r <- m$r
      value <- ex$get("value")
      expected <- if (value == "Events") {
        st <- .dsr_strata(r$events, r$population, r$by, by_sex = TRUE)$data
        st <- st[as.character(st[[r$by[1]]]) == ex$get("year") &
                   st$Sex == ex$get("sex"), , drop = FALSE]
        .dsr_funnel_expected(st, m$rows$Area[!m$rows$Suppressed])
      }
      .dsr_funnel(m$rows, value, multiplier = r$multiplier,
                  years = r$spec$pool, expected = expected,
                  overdispersion = !isFALSE(input$ex_overdispersion))
    })

    # Area names from the boundaries, when they could be downloaded
    ex_names <- shiny::reactive({
      r <- results()
      shiny::req(r, is.null(r$error))
      if (!comparable(r$spec)) return(NULL)
      b <- boundaries()
      if (!is.null(b$error)) return(NULL)
      stats::setNames(b$polys$Name, b$polys$Area)[!duplicated(b$polys$Area)]
    })

    # Tooltip of each area: its name, figures and position in the funnel plot
    ex_tooltips <- shiny::reactive({
      m <- ex$values()
      f <- ex_funnel()
      nm <- ex_names()
      areas <- unique(c(names(nm), m$rows$Area))
      ages <- ex$ages_label(m$r)
      vapply(areas, function(a) {
        row  <- m$rows[m$rows$Area %in% a, , drop = FALSE]
        name <- if (a %in% names(nm)) nm[[a]] else NA
        i    <- match(a, f$points$Area)
        head <- paste0(area_label(m$r$spec), " ", a,
                       if (!is.na(name) && name != a) paste0(" ", name),
                       if (!is.null(ages)) paste0(", ", ages))
        lines <- c(
          paste0("<b>", htmltools::htmlEscape(head), "</b>"),
          htmltools::htmlEscape(
            .dsr_area_text(row, m$band, m$r$suppress, m$r$multiplier)
          ),
          if (f$measure == "Events" && !is.na(i)) {
            paste0("Expected events ",
                   formatC(f$points$Expected[i], format = "f", digits = 1,
                           big.mark = ","),
                   ", observed / expected ",
                   formatC(f$points$Value[i], format = "f", digits = 2))
          },
          if (!is.na(i)) paste0("Funnel plot: ", f$points$Position[i])
        )
        paste(lines, collapse = "<br>")
      }, character(1))
    })

    # File names of the saved plots, e.g.
    # dsr-map-agerate-aged-20-to-44-2023-persons.pdf
    ex_file <- function(what, ext) {
      ages <- ex$ages_label(results())
      if (!is.null(ages)) ages <- paste0("-", gsub(" ", "-", ages))
      paste0("dsr-", what, "-", tolower(ex$get("value")), ages, "-",
             ex$get("year"), "-", tolower(ex$get("sex")), ".", ext)
    }

    # Both plots are SVG drawn by ggiraph, with the same selection, hover and
    # zoom. `hidden` names the toolbar buttons left out. The PNG download is
    # left out of both, as Download saves both plots; saveaspng = FALSE would
    # not hide it, as girafe() drops the setting.
    ex_girafe <- function(p, hidden = "saveaspng") {
      p <- p + ggplot2::theme(plot.margin = ggplot2::margin(26, 12, 8, 8))
      ggiraph::girafe(
        ggobj = p, width_svg = 6.5, height_svg = 6,
        options = list(
          ggiraph::opts_selection(
            type = "multiple", selected = shiny::isolate(ex_selected()),
            css = "stroke:#320557;stroke-width:2.5px;"
          ),
          ggiraph::opts_selection_inv(css = "opacity:0.35;"),
          ggiraph::opts_hover(css = "stroke:#320557;stroke-width:2px;"),
          ggiraph::opts_zoom(min = 1, max = 10),
          ggiraph::opts_toolbar(position = "topright", hidden = hidden),
          ggiraph::opts_tooltip(
            css = paste0("background:#ffffff;color:#333740;",
                         "border:1px solid #c8c3dd;border-radius:4px;",
                         "padding:6px 8px;font-size:12px;"),
            opacity = 1
          ),
          ggiraph::opts_sizing(rescale = TRUE)
        )
      )
    }

    # The measure, year and sex, and the settings: over both plots, and on
    # the plots saved as PDF
    ex_titles <- function() {
      m <- ex$values()
      s <- m$r$spec
      list(title    = ex$title(m$r),
           subtitle = .dsr_describe(s, if (ex$get("value") == "DSR") {
             std_name(s$standard)
           }, years = FALSE, ages = FALSE))
    }

    output$ex_heading <- shiny::renderUI({
      t <- ex_titles()
      shiny::div(
        class = "epi-table-head", style = "display: block;",
        shiny::h5(t$title),
        shiny::div(class = "epi-note", style = "margin: 2px 0 0 0;",
                   t$subtitle)
      )
    })

    # The map, coloured by value or by position in the funnel plot, and
    # zoomed to the selected areas when asked. On the page the heading is
    # over both plots, so only a PDF takes a title and subtitle.
    ex_map_plot <- function(title = NULL, subtitle = NULL) {
      b <- boundaries()
      shiny::req(is.null(b$error))
      m <- ex$values()
      f <- ex_funnel()
      by_position <- identical(input$ex_colour, "position")
      values <- if (by_position) {
        data.frame(
          Area  = if (is.null(f$points)) character(0) else f$points$Area,
          Value = if (is.null(f$points)) {
            factor(character(0), levels = .dsr_funnel_positions)
          } else {
            f$points$Position
          },
          stringsAsFactors = FALSE
        )
      } else {
        m$values
      }
      view <- ex_zoom()
      .dsr_map_plot(
        b$polys, values,
        legend   = if (by_position) "Funnel plot" else ex$legend(),
        title    = title,
        subtitle = subtitle,
        caption  = if (by_position) {
          paste0("Grey: not in the funnel plot. Boundaries: ABS ",
                 .dsr_asgs$name, ", CC BY 4.0.")
        } else {
          map_caption(m$r, ex$get("value"))
        },
        xlim = view$xlim, ylim = view$ylim,
        tooltips = ex_tooltips()
      )
    }

    output$ex_map <- ggiraph::renderGirafe(ex_girafe(ex_map_plot()))

    # Axis titles of the funnel plot for the measure: a rate is named as in
    # the title, against the population of the same ages
    ex_axes <- function(f) {
      r <- ex$values()$r
      a <- .dsr_ages_text(ex$value_ages(r))
      year <- if (f$years > 1) " a year"
      population <- paste0("Population", if (!is.null(a)) paste0(" aged ", a),
                           year)
      switch(
        f$measure,
        DSR     = list(x = paste0("Population derived from events and DSR",
                                  year),
                       y = ex$label(r)),
        Crude   = list(x = population, y = ex$label(r)),
        AgeRate = list(x = population, y = ex$label(r)),
        Events  = list(x = "Expected events",
                       y = "Observed / expected events")
      )
    }

    ex_funnel_plot <- function(title = NULL, subtitle = NULL) {
      f <- ex_funnel()
      shiny::req(!is.null(f$points), nrow(f$points) >= 2)
      tips <- ex_tooltips()
      f$points$Tooltip <- unname(tips[f$points$Area])
      ax <- ex_axes(f)
      .dsr_funnel_plot(
        f, xlab = ax$x, ylab = ax$y, title = title, subtitle = subtitle,
        caption  = paste0(
          "Line: average of the areas plotted. Curves: 95% and 99.8% ",
          "control limits", if (f$adjusted) ", allowing for overdispersion",
          "."
        )
      )
    }

    # The funnel plot has no zoom with rectangle: there it zoomed out, and
    # only reset pan/zoom brought the plot back
    output$ex_funnel <- ggiraph::renderGirafe(
      ex_girafe(ex_funnel_plot(), hidden = c("saveaspng", "zoom_rect"))
    )

    # Download: the map or the funnel plot as PDF (vector) or PNG, titled with
    # the heading. The map takes the shape of the area in view.
    ex_download <- function(what, ext) {
      shiny::downloadHandler(
        filename = function() ex_file(what, ext),
        content  = function(file) {
          t <- ex_titles()
          if (what == "map") {
            p    <- ex_map_plot(t$title, t$subtitle)
            view <- ex_zoom()
            size <- .dsr_map_size(p$data, view$xlim, view$ylim)
          } else {
            p    <- ex_funnel_plot(t$title, t$subtitle)
            size <- c(width = 8, height = 7)
          }
          ggplot2::ggsave(file, p, width = size[["width"]],
                          height = size[["height"]], dpi = 200, bg = "white",
                          device = ext)
        }
      )
    }
    output$ex_map_pdf    <- ex_download("map", "pdf")
    output$ex_map_png    <- ex_download("map", "png")
    output$ex_funnel_pdf <- ex_download("funnel", "pdf")
    output$ex_funnel_png <- ex_download("funnel", "png")
    # The links are in a closed menu: they need their addresses while hidden
    downloads <- c("ex_map_pdf", "ex_map_png", "ex_funnel_pdf", "ex_funnel_png")
    for (id in downloads) {
      shiny::outputOptions(output, id, suspendWhenHidden = FALSE)
    }

    # A selection in either plot is made in the other too
    ex_select <- function(ids) {
      if (is.null(ids)) ids <- character(0)
      if (setequal(ids, ex_selected())) return(invisible(NULL))
      ex_selected(ids)
      session$sendCustomMessage("ex_map_set", ids)
      session$sendCustomMessage("ex_funnel_set", ids)
    }
    shiny::observeEvent(input$ex_map_selected,
                        ex_select(input$ex_map_selected), ignoreNULL = FALSE)
    shiny::observeEvent(input$ex_funnel_selected,
                        ex_select(input$ex_funnel_selected), ignoreNULL = FALSE)
    shiny::observeEvent(input$ex_clear, ex_select(character(0)))

    # Zoom the map to the selected areas, or back to all its areas
    shiny::observeEvent(input$ex_zoom, {
      b <- boundaries()
      p <- b$polys[b$polys$Area %in% ex_selected(), , drop = FALSE]
      if (!nrow(p)) {
        shiny::showNotification(
          "Select areas in the map or the funnel plot first.",
          duration = 4, type = "message"
        )
        return(invisible(NULL))
      }
      pad <- 0.1 * max(diff(range(p$x)), diff(range(p$y)), 0.05)
      ex_zoom(list(xlim = range(p$x) + c(-pad, pad),
                   ylim = range(p$y) + c(-pad, pad)))
    })
    shiny::observeEvent(input$ex_reset, ex_zoom(NULL))

    output$ex_note <- shiny::renderUI({
      note <- function(...) shiny::div(class = "epi-note", ...)
      if (is.null(rv$fetched)) {
        return(note("Press Calculate, then the map and funnel plot show ",
                    "the results."))
      }
      s <- rv$fetched$spec
      if (!comparable(s)) {
        return(note(paste0("Choose SA3 or SA2 to compare areas within ",
                           .dsr_scopes[[s$scope]]$region, ".")))
      }
      b <- boundaries()
      if (!is.null(b$error)) {
        return(shiny::div(
          class = "epi-note epi-error",
          "The boundaries could not be downloaded from the ABS ",
          "(geo.abs.gov.au), so only the funnel plot is shown: ", b$error
        ))
      }
      note("Point at an area or a point for its figures. Click areas or ",
           "points, or draw round them with the lasso in a plot's toolbar, ",
           "to select them in both plots. The toolbar also zooms and pans. ",
           "Download saves either plot as PDF or PNG.")
    })

    # How the funnel plot is made, and how much the areas vary
    output$ex_method <- shiny::renderUI({
      f <- ex_funnel()
      shiny::req(!is.null(f$points), nrow(f$points) >= 2)
      digits <- if (f$measure == "Events") 2 else
        if (f$multiplier >= 100000) 1 else 2
      centre <- formatC(f$centre, format = "f", digits = digits,
                        big.mark = ",")
      what <- switch(
        f$measure,
        DSR     = paste0("Each area's DSR is plotted against the population ",
                         "that would give it as a crude rate from the area's ",
                         "events (events / DSR), as in OHID's funnel plots"),
        Crude   = "Each area's crude rate is plotted against its population",
        AgeRate = paste0("Each area's rate is plotted against its ",
                         "population of those ages"),
        Events  = paste0("Each area's events are divided by the events ",
                         "expected from the age-specific rates of all the ",
                         "areas plotted, and plotted against the expected ",
                         "events")
      )
      left_out <- c(
        if (f$suppressed) paste0(f$suppressed, " suppressed"),
        if (f$no_value) paste0(f$no_value, " with no value"),
        if (f$not_presented) {
          paste0(f$not_presented, " whose DSR is not presented (n.p.)")
        }
      )
      od <- f$od
      spread <- if (!is.null(od)) {
        over <- od$p < 0.05
        paste0(
          if (over) {
            paste0("The areas vary more than chance alone would explain ",
                   "(overdispersion ratio ")
          } else {
            paste0("The areas vary no more than chance would explain ",
                   "(overdispersion ratio ")
          },
          formatC(od$phi, format = "f", digits = 1), ", p ",
          if (od$p < 0.001) {
            "< 0.001"
          } else if (od$p > 0.99) {
            "> 0.99"
          } else {
            paste0("= ", signif(od$p, 2))
          }, "). ",
          if (f$adjusted) {
            paste0("The limits allow for this, so areas outside them stand ",
                   "out from the usual variation between areas.")
          } else if (!isFALSE(input$ex_overdispersion)) {
            "So there is nothing to allow for, and the limits are unchanged."
          } else if (over) {
            paste0("Areas outside the limits differ from the average by more ",
                   "than chance; tick Allow for overdispersion to see which ",
                   "also stand out from the usual variation between areas.")
          },
          if (od$n < 10) {
            " With fewer than 10 areas this estimate is unreliable."
          }
        )
      }
      shiny::div(
        class = "epi-note",
        style = "margin-top: 8px;",
        paste0(
          what, ". The line is the average of the areas plotted (", centre,
          "), and the curves are 95% and 99.8% control limits",
          if (f$adjusted) ", allowing for overdispersion" else
            ", from exact Poisson tests",
          ". ", nrow(f$points), " areas are plotted",
          if (length(left_out)) {
            n <- length(left_out)
            paste0("; not plotted: ", if (n > 1) {
              paste(paste(left_out[-n], collapse = ", "), "and", left_out[n])
            } else {
              left_out
            })
          },
          ". ", spread
        )
      )
    })

    # The selected areas' figures
    output$ex_selected <- shiny::renderUI({
      ids <- ex_selected()
      shiny::req(length(ids))
      m <- ex$values()
      f <- ex_funnel()
      nm <- ex_names()
      digits <- if (m$r$multiplier >= 100000) 1 else 2
      num <- function(v, d = digits) {
        ifelse(is.na(v), "", formatC(v, format = "f", digits = d,
                                     big.mark = ","))
      }
      # Events are shown as observed / expected, as in the funnel plot
      ratio <- f$measure == "Events"
      rows <- lapply(ids, function(a) {
        row <- m$rows[m$rows$Area %in% a, , drop = FALSE]
        i <- match(a, f$points$Area)
        val <- if (ratio) f$points$Value[i] else
          m$values$Value[match(a, m$values$Area)]
        shiny::tags$tr(
          shiny::tags$td(a),
          shiny::tags$td(if (a %in% names(nm)) nm[[a]] else ""),
          shiny::tags$td(class = "num", if (nrow(row)) {
            if (isTRUE(row$Suppressed[1])) {
              paste0("<", .fmt_count(m$r$suppress))
            } else {
              .fmt_count(row$Events[1])
            }
          }),
          shiny::tags$td(class = "num",
                         if (nrow(row)) .fmt_count(row$Population[1])),
          shiny::tags$td(class = "num", num(val, if (ratio) 2 else digits)),
          shiny::tags$td(if (is.na(i)) "Not plotted" else
            as.character(f$points$Position[i]))
        )
      })
      shiny::tags$table(
        class = "ex-table",
        shiny::tags$thead(shiny::tags$tr(
          shiny::tags$th(area_label(m$r$spec)), shiny::tags$th("Name"),
          shiny::tags$th("Events"), shiny::tags$th("Population"),
          shiny::tags$th(if (ratio) "Observed / expected" else ex$legend()),
          shiny::tags$th("Funnel plot")
        )),
        shiny::tags$tbody(rows)
      )
    })

    # ── Insert / copy code button ──────────────────────────────────────────
    shiny::observeEvent(input$insert_code, {
      st <- settings()
      s  <- st$spec
      if (is.null(s)) {
        shiny::showNotification(st$error, duration = 6, type = "warning")
        return(invisible(NULL))
      }
      # The code adds the age-specific rates when their table is shown
      code <- .dsr_code(
        s,
        by_sex        = isTRUE(input$by_sex),
        age_specific  = identical(input$table, "age"),
        multiplier    = as.numeric(input$multiplier),
        standard_name = std_name(s$standard),
        suppress      = suppress(),
        min_events    = min_events(),
        min_population = min_population()
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
