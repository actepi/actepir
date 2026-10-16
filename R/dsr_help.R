# Help tab of the DSR Calculator: the data, formulas and sources behind every
# figure it shows. Tables, fields, the ASGS edition, the ACT and surrounds,
# thresholds and boundary services are filled in from the definitions the
# calculator uses, so the help follows the code.
#' @noRd
.dsr_help <- function() {

  # Inline tags sit against the text around them, and adjacent strings are
  # joined as written, so no stray spaces appear before punctuation
  code   <- function(x) shiny::tags$code(x, .noWS = "outside")
  em     <- function(x) shiny::em(x, .noWS = "outside")
  strong <- function(x) shiny::strong(x, .noWS = "outside")
  html   <- shiny::HTML
  joined <- function(tag) {
    function(...) {
      out <- list()
      for (x in list(...)) {
        if (is.numeric(x)) x <- as.character(x)
        n <- length(out)
        if (is.character(x) && !inherits(x, "html") && n &&
            is.character(out[[n]]) && !inherits(out[[n]], "html")) {
          out[[n]] <- paste0(out[[n]], x)
        } else {
          out[[n + 1]] <- x
        }
      }
      tag(out)
    }
  }
  para <- joined(shiny::p)
  item <- joined(shiny::tags$li)
  formula <- function(x) shiny::div(class = "help-formula", html(x))
  table <- function(head, rows) {
    shiny::div(
      class = "help-scroll",
      shiny::tags$table(
        class = "ex-table help-table",
        shiny::tags$thead(shiny::tags$tr(lapply(head, shiny::tags$th))),
        shiny::tags$tbody(lapply(rows, function(r) {
          shiny::tags$tr(lapply(r, shiny::tags$td))
        }))
      )
    )
  }

  fields <- function(d) {
    f <- d$diagnoses
    if (length(f) > 4) paste(f[1], "to", f[length(f)]) else paste(f, collapse = ", ")
  }
  surrounds <- paste0(names(.dsr_surrounds), " ", .dsr_surrounds,
                      collapse = ", ")
  offset <- function(scope) {
    paste0(format(.dsr_boundary_offset(scope), scientific = FALSE), " degrees")
  }
  p_95  <- .dsr_funnel_p[["95%"]]
  p_998 <- .dsr_funnel_p[["99.8%"]]

  shiny::div(
    class = "help",

    shiny::h5("About the calculator"),
    para(
      "The DSR Calculator counts events in EpiServer, matches them to the ",
      "estimated resident population of the same calendar year, and ",
      "calculates crude rates, age-specific rates and directly ",
      "age-standardised rates (DSRs) with 95% confidence intervals. The ",
      "rates are calculated by ", code("actepir::dsr_calculate()"), " and ",
      code("actepir::dsr_age_specific()"), ", and Copy Code gives R code that ",
      "reproduces the table shown. The sections below set out the data, the ",
      "formulas and the sources."
    ),

    shiny::h5("Events"),
    table(
      c("Dataset", "Table", "Calendar year of", "Age in years",
        "Diagnosis fields", "ICD-10 records"),
      lapply(.dsr_datasets, function(d) {
        list(d$label, code(paste0("Analysis.dbo.", d$table)), code(d$date),
             code(d$age), fields(d), code(d$icd10))
      })
    ),
    para(
      "Events are counted by calendar year, sex, 5-year age group and area ",
      "of residence. The age group is floor(age / 5) + 1 for ages 0 to 84 and ",
      "18 for 85 and over, matching AgeGroup05Code in the population tables. ",
      "Diagnosis codes are ICD-10-AM codes, compared without dots or spaces. ",
      "A code matches every code that starts with it; a range runs from its ",
      "first code to its last and includes every code that starts with the ",
      "last. For APC the principal diagnosis is ",
      code(.dsr_datasets$APC$diagnoses[1]), ". ED has no field that holds the ",
      "principal diagnosis in every year, so all its fields are searched. ",
      "Searching by condition finds the codes whose titles have a word ",
      "starting with each word typed. The titles come from ",
      code("Analysis.dbo.ICD10AM"), ", each from the latest edition that ",
      "holds the code, and a code that some editions lack is labelled with ",
      "the editions that hold it."
    ),

    shiny::h5("Populations and areas"),
    para(
      "Populations are estimated resident populations from the 5-year ",
      "tables ", code("ERP5_STE"), ", ", code("ERP5_SA3"), " and ",
      code("ERP5_SA2"), " (ACT or Australian) in Analysis.dbo: ERPCount by ",
      "ERPYear, SexCode, AgeGroup05Code and EntityCode. Each year's events are ",
      "matched to the population of the same year. Standard populations are ",
      "StdPopValue by AgeGroup05Code in ", code("Analysis.dbo.StandardPops"),
      "; the Australian 2001 standard population is the estimated resident ",
      "population of Australia at 30 June 2001, as the AIHW (2011) recommends."
    ),
    para(
      "Area of residence comes from ",
      code(.dsr_levels$state$column), ", ", code(.dsr_levels$sa3$column),
      " and ", code(.dsr_levels$sa2$column), ". SA3s and SA2s are areas of ",
      "the Australian Statistical Geography Standard, ", .dsr_asgs$name,
      ": the edition of the population tables, which do not record it ",
      "themselves. The ACT and surrounds are the ACT and the NSW ",
      "SA3s that share a border with it (", surrounds, "), with their SA2s, ",
      "from the Australian tables; at state level the ACT and ",
      .dsr_surrounds_label, " are each summed from their SA3s."
    ),
    para(
      "Events with no recorded age, no recorded area of residence, or an ",
      "area missing from the population table are left out of the rates and ",
      "counted in the information bar."
    ),

    shiny::h5("Rates and confidence intervals"),
    para(html(paste0(
      "For an area, year (or period) and sex, O<sub>i</sub> is the number of ",
      "events and N<sub>i</sub> the population in age group i, O and N are ",
      "their totals, and k is the multiplier (Rate per)."
    ))),
    formula("Crude rate: R = k O / N"),
    formula(paste0(
      "95% CI (exact Poisson, Garwood 1936): ",
      "k &chi;<sup>2</sup><sub>0.025</sub>(2O) / (2N) to ",
      "k &chi;<sup>2</sup><sub>0.975</sub>(2O + 2) / (2N)"
    )),
    formula(paste0(
      "Age-specific rate: r<sub>i</sub> = k O<sub>i</sub> / N<sub>i</sub>, ",
      "with the same exact limits"
    )),
    formula(paste0(
      "DSR = k &Sigma; w<sub>i</sub> O<sub>i</sub> / N<sub>i</sub>, where ",
      "w<sub>i</sub> = S<sub>i</sub> / &Sigma; S<sub>j</sub>"
    )),
    para(html(paste0(
      "S<sub>i</sub> is the standard population of age group i. The sum of ",
      "S<sub>j</sub> runs over the age groups in the population, within the ",
      "ages chosen, so the weights sum to 1."
    ))),
    formula(paste0(
      "Variance: V = &Sigma; w<sub>i</sub><sup>2</sup> O<sub>i</sub> / ",
      "N<sub>i</sub><sup>2</sup>; 95% CI (Dobson et al. 1991): ",
      "DSR + k &radic;(V / O) (O<sub>L</sub> &minus; O) to ",
      "DSR + k &radic;(V / O) (O<sub>U</sub> &minus; O)"
    )),
    para(html(paste0(
      "O<sub>L</sub> and O<sub>U</sub> are the exact Poisson limits of O. A ",
      "negative lower limit is set to 0. The DSR is not calculated when there ",
      "are events in an age group with no population."
    ))),
    para(
      strong("When a DSR is not presented."), " The AIHW (2011) principles ",
      "for direct age-standardisation, which the ABS applies (ABS 2011), ",
      "advise against age-standardising when a population has fewer than 20 ",
      "events in total (Principle 3) or a population under 30 in any one age ",
      "group (Principle 2). Such a DSR is shown as n.p. (not presented), with ",
      "the reason on pointing at it, and its counts and crude rate are kept. ",
      "Combining years (Pool years) or choosing a larger area raises the ",
      "events; otherwise the principles suggest reporting the counts, crude ",
      "rates and age-specific rates instead. The minimums are set under Rate ",
      "details; 0 turns a check off. Counts by age in the Tables tab shows the ",
      "events and population of each age group, with populations and event ",
      "totals under the minimums highlighted."
    ),
    para(
      strong("Ages."), " Limiting the rates to an age range keeps the ",
      "events, populations and standard population of those age groups only. ",
      "The weights are then the standard's shares within the range (a ",
      "truncated DSR), and rates are per population of those ages."
    ),
    para(html(paste0(
      "<strong>Age-specific rate for an age range</strong> (maps): ",
      "k &Sigma; O<sub>i</sub> / &Sigma; N<sub>i</sub> over the age groups ",
      "in the range. It is not age-standardised."
    ))),
    para(
      strong("Pooled years."), " Events and populations are summed over ",
      "the years of a period after each year's events are matched to that ",
      "year's population, so rates are per person-year."
    ),
    para(
      strong("Suppression."), " Rows with fewer events than the ",
      "threshold have their events and rates withheld; their populations are ",
      "shown. Each row is judged on its own count; complementary suppression ",
      "is not applied."
    ),

    shiny::h5("Funnel plots"),
    para(html(paste0(
      "Each area a is compared with the average &theta; of the areas plotted. ",
      "Its observed events O<sub>a</sub> are tested against the events ",
      "E<sub>a</sub> expected at the average, taking O<sub>a</sub> to follow ",
      "a Poisson distribution with mean E<sub>a</sub>."
    ))),
    table(
      c("Show", "y-axis", "x-axis", "Expected events", "Average"),
      list(
        list("Crude rate, age-specific rate", "Rate",
             html("Population N<sub>a</sub> (of the ages mapped)"),
             html("&theta; N<sub>a</sub>"),
             html("&theta; = &Sigma; O / &Sigma; N")),
        list("Age-standardised rate", "DSR",
             html("Derived population N*<sub>a</sub> = k O<sub>a</sub> / DSR<sub>a</sub>"),
             html("&theta; N*<sub>a</sub>"),
             html("&theta; = &Sigma; O / &Sigma; N*")),
        list("Events", html("O<sub>a</sub> / E<sub>a</sub>"),
             html("E<sub>a</sub>"),
             html(paste0("E<sub>a</sub> = &Sigma; N<sub>ai</sub> ",
                         "&lambda;<sub>i</sub>, &lambda;<sub>i</sub> = ",
                         "&Sigma;<sub>b</sub> O<sub>bi</sub> / ",
                         "&Sigma;<sub>b</sub> N<sub>bi</sub>")),
             "1")
      )
    ),
    para(
      "DSRs follow the method of the Office for Health Improvement and ",
      "Disparities (OHID) in Fingertips: the derived population is the ",
      "population that would give the DSR as a crude rate from the area's ",
      "events. Areas whose DSR is not presented (see Rates and confidence ",
      "intervals) are neither plotted nor counted in the average; OHID leaves ",
      "out DSRs from fewer than 10 events. A count alone cannot be ",
      "compared between areas of different sizes, so events are divided by ",
      "the events expected from the age-specific rates of all the areas ",
      "plotted (indirect standardisation); the expected events then total the ",
      "observed. For pooled periods the x-axis shows the population a year. ",
      "Suppressed areas are neither plotted nor counted in the average."
    ),
    para(html(paste0(
      "<strong>Control limits.</strong> With X following a Poisson ",
      "distribution with mean E<sub>a</sub>, an area is above the 95% limit ",
      "when P(X &ge; O<sub>a</sub>) &lt; ", p_95, " and above the 99.8% limit ",
      "when it is below ", p_998, ", and below the limits when ",
      "P(X &le; O<sub>a</sub>) is below the same values. The curves pass ",
      "through the expected events E at which each count o is just ",
      "significant: E = G<sup>&minus;1</sup>(p; o) for the upper limits and ",
      "E = G<sup>&minus;1</sup>(1 &minus; p; o + 1) for the lower limits, ",
      "where G<sup>&minus;1</sup>(q; s) is the q quantile of a gamma ",
      "distribution with shape s. They are drawn at o / E times the average."
    ))),
    para(
      strong("Overdispersion."), " When areas vary more than chance ",
      "alone would explain, most fall outside the Poisson limits. Allow for ",
      "overdispersion, ticked by default, widens the limits by ",
      "Spiegelhalter's (2005) additive random effects method:"
    ),
    formula(paste0(
      "z<sub>a</sub> = 2(&radic;O<sub>a</sub> &minus; &radic;E<sub>a</sub>), ",
      "winsorised at the 10th and 90th percentiles"
    )),
    formula(paste0(
      "&phi; = (1/n) &Sigma; z<sub>a</sub><sup>2</sup>, tested by comparing ",
      "n&phi; with a &chi;<sup>2</sup> distribution on n &minus; 1 degrees ",
      "of freedom"
    )),
    formula(paste0(
      "&tau;<sup>2</sup> = max{0, (n&phi; &minus; (n &minus; 1)) / ",
      "(&Sigma; w<sub>a</sub> &minus; &Sigma; w<sub>a</sub><sup>2</sup> / ",
      "&Sigma; w<sub>a</sub>)}, where w<sub>a</sub> = 4E<sub>a</sub>"
    )),
    formula(paste0(
      "Limits: (1 &plusmn; z<sub>p</sub> &radic;(1 / (4E) + ",
      "&tau;<sup>2</sup>))<sup>2</sup> times the average, where ",
      "z<sub>p</sub> is ", formatC(stats::qnorm(1 - p_95), format = "f",
                                   digits = 3),
      " (95%) or ", formatC(stats::qnorm(1 - p_998), format = "f", digits = 3),
      " (99.8%)"
    )),
    para(html(paste0(
      "When &tau;<sup>2</sup> is 0 the exact limits are kept. With fewer ",
      "than 10 areas the estimate is unreliable."
    ))),
    para(
      strong("Reading the plot."), " An area outside the limits ",
      "differs from the average by more than chance (or, allowing for ",
      "overdispersion, by more than the usual variation between areas). ",
      "Among many areas, about 1 in 20 falls outside the 95% limits by chance ",
      "alone, and about 1 in 500 outside the 99.8% limits. Areas above the ",
      "limits are coral (99.8%) and yellow (95%), areas below them blue ",
      "(99.8%) and green (95%), from the ACT Government regional colours, ",
      "with larger triangles beyond the 99.8% limits."
    ),

    shiny::h5("Maps and boundaries"),
    para(
      "Boundaries are the Australian Bureau of Statistics (ABS) digital ",
      "boundaries of ", .dsr_asgs$name, ", copyright Commonwealth of ",
      "Australia, licensed under Creative Commons Attribution 4.0 (CC BY ",
      "4.0). They are downloaded as GeoJSON from the ABS ArcGIS REST services:"
    ),
    table(
      c("Level", "Boundary layer"),
      lapply(names(.dsr_levels), function(level) {
        list(.dsr_area_label(level), code(.dsr_abs_layer(level)))
      })
    ),
    para(
      "Field names are read from each layer. The boundaries are requested in ",
      "longitude and latitude (WGS 84, EPSG:4326) and simplified by the ",
      "server to within ", offset("ACT"), " for ACT residents and ",
      offset("AUS"), " for wider areas. Each level and population is ",
      "downloaded once and kept in ",
      code(tools::R_user_dir("actepir", which = "cache")), ". Maps are drawn ",
      "in longitude and latitude, with the aspect corrected for latitude ",
      "(ggplot2's coord_quickmap()); the Australian view leaves out ",
      "Christmas, Cocos (Keeling) and Norfolk Islands."
    ),

    shiny::h5("Software"),
    para(
      "R packages: shiny and miniUI (the app), DT (tables), DBI and odbc ",
      "(EpiServer), ggplot2 (maps and plots), ggiraph (the interactive map ",
      "and funnel plot) and jsonlite (boundaries). The funnel plots follow ",
      "OHID's R package PHEindicatormethods, with exact Poisson limits where ",
      "it approximates them from 10 events (Byar's method), and the ",
      "overdispersion estimate matches the NHS-R Community's FunnelPlotR."
    ),

    shiny::h5("References"),
    shiny::tags$ul(
      class = "help-refs",
      item(
        "Australian Bureau of Statistics (2011). ",
        em("Deaths, Australia, 2010."),
        " Appendix 1: Principles on the use of direct age-standardisation. ",
        "Cat. no. 3302.0. Canberra: ABS."
      ),
      item(
        "Australian Bureau of Statistics (2021). ",
        em(paste0("Australian Statistical Geography Standard (ASGS) ",
                  "Edition 3, July 2021 - June 2026.")),
        " Canberra: ABS. Digital boundary files and ArcGIS REST services, ",
        "CC BY 4.0."
      ),
      item(
        "Australian Institute of Health and Welfare (2011). ",
        em(paste0("Principles on the use of direct age-standardisation in ",
                  "administrative data collections: for measuring the gap ",
                  "between Indigenous and non-Indigenous Australians.")),
        " Cat. no. CSI 12. Canberra: AIHW."
      ),
      item(
        "Dobson AJ, Kuulasmaa K, Eberle E, Scherer J (1991). Confidence ",
        "intervals for weighted sums of Poisson parameters. ",
        em("Statistics in Medicine"), ", 10(3), 457-462."
      ),
      item(
        "Garwood F (1936). Fiducial limits for the Poisson distribution. ",
        em("Biometrika"), ", 28(3/4), 437-442."
      ),
      item(
        "Public Health England (2017). ",
        em("Technical guide: funnel plots."),
        " Public Health Data Science. Implemented in the R package ",
        "PHEindicatormethods (OHID)."
      ),
      item(
        "Spiegelhalter DJ (2005). Funnel plots for comparing institutional ",
        "performance. ", em("Statistics in Medicine"),
        ", 24(8), 1185-1202."
      ),
      item(
        "Spiegelhalter DJ (2005). Handling over-dispersion of performance ",
        "indicators. ", em("Quality and Safety in Health Care"),
        ", 14(5), 347-351."
      ),
      item(
        "ACT Government (2025). ",
        em("Brand Guardian at-a-glance,"),
        " version 1.0 (internal). Regional colours."
      )
    )
  )

}
