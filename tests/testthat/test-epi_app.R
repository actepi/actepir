test_that("primary buttons go to Iris on hover", {

  css <- as.character(.epi_app_style())
  rule <- function(selector) {
    regmatches(css, regexpr(paste0(selector, "[^{]*\\{[^}]*\\}"), css))
  }

  expect_match(css, "--epi-accent:\\s*#562C8C")
  expect_match(rule("\\.btn-primary:hover"),
               "background-color: var\\(--epi-accent\\)")
  # No app uses miniUI, so its title bar and Done rules have gone
  expect_no_match(css, "#done", fixed = TRUE)
  expect_no_match(css, ".gadget", fixed = TRUE)

})

test_that("a click leaves no highlight, but the keyboard's focus shows", {

  css <- as.character(.epi_app_style())
  rule <- function(selector) {
    regmatches(css, regexpr(paste0(selector, "[^{]*\\{[^}]*\\}"), css))
  }

  # Buttons are Iris for the keyboard's focus only, not after a click
  expect_match(rule("\\.btn-primary:hover"), ":focus-visible")
  expect_no_match(rule("\\.btn-primary:hover"), ":focus[,\\s{]")
  # Focus rings go after a click on buttons, section headings, ticks and
  # radio buttons, and a closed dropdown is plain
  ring <- rule("\\.btn:focus:not\\(:focus-visible\\)")
  for (target in c("\\.accordion-button:focus:not\\(:focus-visible\\)",
                   "\\.checkbox input:focus:not\\(:focus-visible\\)",
                   "\\.radio input:focus:not\\(:focus-visible\\)")) {
    expect_match(ring, target)
  }
  expect_match(ring, "box-shadow: none")
  expect_match(rule(paste0("\\.selectize-control\\.single \\.selectize-input",
                           "\\.focus:not\\(\\.dropdown-active\\)")),
               "box-shadow: none !important")

})

test_that("notifications look like the info bar, not Shiny's blue and grey", {

  css <- as.character(.epi_app_style())
  rule <- function(selector) {
    regmatches(css, regexpr(paste0(selector, "\\s*\\{[^}]*\\}"), css))
  }

  base <- rule("#shiny-notification-panel \\.shiny-notification")
  expect_match(base, "background-color: var\\(--epi-info-bg\\)")
  expect_match(base, "border-left: 3px solid var\\(--epi-primary\\)")
  expect_match(rule("#shiny-notification-panel \\.shiny-notification-warning"),
               "#8a5300")
  expect_match(rule("#shiny-notification-panel \\.shiny-notification-error"),
               "#b00020")
  expect_match(rule("#shiny-notification-panel \\.progress-bar"),
               "var\\(--epi-primary\\)")

})

test_that("problem reports go to healthinfo and describe the session", {

  expect_equal(.epi_contact, "healthinfo@act.gov.au")

  # Proxy variables are named, never given, as they can hold a password
  withr::local_envvar(https_proxy = "http://user:secret@proxy.example:8080")
  lines <- .epi_session_report()
  expect_match(lines[1], paste0("^actepir ", utils::packageVersion("actepir")))
  expect_match(lines[2], R.version.string, fixed = TRUE)
  expect_true(any(grepl("^Proxy variables set: .*https_proxy", lines)))
  expect_false(any(grepl("secret", lines, fixed = TRUE)))
  expect_true(any(grepl("^Library paths: ", lines)))
  expect_true(any(grepl("^ODBC drivers: ", lines)))
  # Every package loaded, each with its version on the same line
  at <- match("Packages loaded:", lines)
  packages <- paste(lines[-seq_len(at)], collapse = " ")
  expect_match(packages, paste0("shiny ", utils::packageVersion("shiny")),
               fixed = TRUE)
  expect_no_match(packages, "_", fixed = TRUE)
  expect_true(all(nchar(lines[-seq_len(at)]) <= 76))
  expect_false(any(grepl("^Browser: ", lines)))

  client <- list(agent = "Mozilla/5.0 Edg/130.0", width = 1440, height = 900,
                 ratio = 1, errors = list(), script_errors = list("x is null"))
  lines <- .epi_session_report(client)
  expect_true("Browser: Mozilla/5.0 Edg/130.0" %in% lines)
  expect_true("Errors shown in the page: none" %in% lines)
  expect_true("Script errors: x is null" %in% lines)

})

test_that("a problem report sets out what happened, the settings and the session", {

  report <- .epi_problem_report(
    "DSR Calculator", "Calculator", "The map was blank.\nIt had been fine.",
    state = c(Calculated = "yes", "EpiServer error" = "none"),
    inputs = list(dataset = "APC", codes = c("I21", "J45"),
                  years = c(2022, 2024), by_sex = TRUE, ex_ages = NULL),
    code = "rates <- dsr_calculate(events, population, standard)\n",
    session = c("actepir 0.0.18", "R version 4.4.1")
  )
  lines <- strsplit(report, "\n", fixed = TRUE)[[1]]
  expect_match(lines[1], "^DSR Calculator problem report, \\d{4}-\\d{2}-\\d{2}")
  for (heading in c("What happened", "Calculator", "Settings",
                    "R code for these settings", "R session")) {
    at <- match(heading, lines)
    expect_false(is.na(at), label = heading)
    expect_equal(lines[at + 1], strrep("-", nchar(heading)))
  }
  expect_match(report, "The map was blank.\nIt had been fine.", fixed = TRUE)
  expect_match(report, "EpiServer error: none", fixed = TRUE)
  # Settings are written as R arguments, ready for session$setInputs()
  expect_true(all(c("dataset = \"APC\"", "codes = c(\"I21\", \"J45\")",
                    "years = c(2022, 2024)", "by_sex = TRUE",
                    "ex_ages = NULL") %in% lines))
  expect_match(report, "rates <- dsr_calculate(", fixed = TRUE)

  # The preview leaves out what happened, and the code without valid settings
  preview <- .epi_problem_report("DSR Calculator", "Calculator", NULL,
                                 c(Calculated = "no"), list(), NULL,
                                 "actepir 0.0.18")
  expect_no_match(preview, "What happened", fixed = TRUE)
  expect_no_match(preview, "R code for these settings", fixed = TRUE)
  expect_match(.epi_problem_report("DSR Calculator", "Calculator", " ",
                                   c(Calculated = "no"), list(), NULL,
                                   "actepir 0.0.18"),
               "What happened\n-------------\n(Not described.)", fixed = TRUE)

})

test_that("controls and sections take their spacing and type from shared tokens", {

  css <- as.character(.epi_app_style())
  rule <- function(selector) {
    regmatches(css, regexpr(paste0(selector, "[^{]*\\{[^}]*\\}"), css))
  }
  token <- function(name) {
    sub(";.*", "", sub(paste0(".*--", name, ":\\s*"), "", css))
  }

  # The tokens hold the spacing chosen for the DSR Calculator's sidebar
  expect_equal(token("epi-font"), "15px")
  expect_equal(token("epi-font-box"), "14px")
  expect_equal(token("epi-gap"), "10px")
  expect_equal(token("epi-pad"), "16px")
  expect_equal(token("epi-head-pad"), "10px")
  expect_equal(token("bslib-sidebar-padding"), "var(--epi-pad)")

  # Rules on the classes of Shiny, Bootstrap 3 and 5, bslib and
  # shinydashboard apply them, so a new control needs no CSS of its own
  expect_match(rule("\\.shiny-input-container \\{"), "var\\(--epi-gap\\)")
  expect_match(rule("\\.shiny-input-container > \\.control-label"),
               "font-size: var\\(--epi-font\\)")
  boxes <- rule("\\.shiny-input-container input\\.form-control,")
  expect_match(boxes, "\\.selectize-input")
  expect_match(boxes, "font-size: var\\(--epi-font-box\\)")
  expect_match(rule("\\.accordion \\{"),
               "--bs-accordion-btn-bg: var\\(--epi-info-bg\\)")
  expect_match(rule("\\.accordion \\{"),
               "--bs-accordion-btn-padding-y: var\\(--epi-head-pad\\)")
  expect_match(rule("\\.accordion-button \\{"), "font-weight: 600")
  expect_match(rule("\\.box > \\.box-header \\{"),
               "background-color: var\\(--epi-info-bg\\)")
  expect_match(rule("\\.box > \\.box-body"), "var\\(--epi-body-pad\\)")
  expect_match(rule("\\.bslib-sidebar-layout > \\.sidebar > \\.sidebar-content \\{"),
               "gap: 0")

  # A bslib sidebar's resize handle sits outside the sidebar, clear of its
  # scrollbar
  expect_match(rule(paste0("\\.bslib-sidebar-layout:not\\(\\.sidebar-right\\) > ",
                           "\\.bslib-sidebar-resize-handle \\{")), "left: 0")

})

test_that("the top bar has the name at the left and the logo at the right", {

  css <- as.character(.epi_app_style())
  # The lockup 56px tall, with 24px above and below in the 104px bar; the
  # name stands apart from the tabs
  expect_match(css, ".navbar .epi-logo { display: block; height: 56px; margin: 24px 0 24px 22px; }",
               fixed = TRUE)
  expect_match(css, ".navbar > .container-fluid { padding-left: 22px; padding-right: 22px; }",
               fixed = TRUE)
  expect_match(css, ".navbar-brand { margin-right: 48px; }", fixed = TRUE)

})

test_that("a tab's own settings are a panel, distinct from the app's sidebar", {

  layout <- .epi_tab_settings(
    "tab_settings", shiny::tagList(shiny::selectInput("a", "A", 1:2)),
    shiny::div(id = "tab_results")
  )
  html <- as.character(layout)
  # The class is on the layout itself, where the CSS looks for it
  expect_match(html, "^<div class=\"bslib-sidebar-layout[^\"]* epi-tab-layout\"")
  expect_match(html, "<aside id=\"tab_settings\" class=\"sidebar")
  expect_match(html, "data-open-desktop=\"always\"", fixed = TRUE)
  expect_match(html, "--_sidebar-width:240px", fixed = TRUE)
  expect_no_match(html, "data-resizable", fixed = TRUE)
  expect_match(html, "id=\"tab_results\"", fixed = TRUE)

  # Without the sidebar's dividing border, as tall as its settings and held
  # in view while the results scroll
  css <- as.character(.epi_app_style())
  expect_match(css, ".epi-tab-layout { --bslib-sidebar-vert-border: none; }",
               fixed = TRUE)
  expect_match(css, "(?s)\\.epi-tab-layout > \\.sidebar \\{[^}]*position: sticky",
               perl = TRUE)

})

test_that("a setting's notes sit behind an information button beside its label", {

  field <- .epi_with_help(
    shiny::radioButtons("diagnosis", "Diagnosis search", c(a = "a", b = "b")),
    "Notes on ", "the setting."
  )
  html <- as.character(field)
  expect_match(html, "^<div class=\"epi-field\">")
  # The notes follow the label, before the choices, and start hidden
  expect_match(html, paste0(
    "(?s)<label class=\"control-label\" id=\"diagnosis-label\" ",
    "for=\"diagnosis\">Diagnosis search</label>\\s*",
    "<details id=\"diagnosis_help\" class=\"epi-help\">\\s*",
    "<summary title=\"About diagnosis search\" ",
    "aria-label=\"About diagnosis search\">.*?</summary>\\s*",
    "<div class=\"epi-note\">\\s*Notes on\\s+the setting.\\s*</div>\\s*",
    "</details>\\s*",
    "<div class=\"shiny-options-group\">"), perl = TRUE)
  expect_no_match(html, "<details[^>]* open", perl = TRUE)

})

test_that("table sort arrows have a fixed size, so they cannot overlap", {

  css <- as.character(.epi_app_style())
  arrows <- regmatches(css, regexpr(
    "th\\[class\\*='sorting'\\]:before[^{]*\\{[^}]*\\}", css
  ))

  expect_match(arrows, "th\\[class\\*='sorting'\\]:after")
  expect_match(arrows, "font-size: 9px !important")
  expect_match(arrows, "line-height: 9px !important")

})

test_that("the bslib theme uses the brand purples and fonts", {

  # Reading the variables compiles Sass; keep sass from caching it in the
  # user's cache folder
  withr::local_options(sass.cache = FALSE)
  theme <- .epi_theme()
  vars  <- bslib::bs_get_variables(
    theme, c("primary", "navbar-bg", "link-color", "component-active-bg",
             "font-family-base", "headings-font-family")
  )

  expect_equal(unname(vars[c("primary", "navbar-bg")]),
               c("#320557", "#320557"))
  expect_equal(unname(vars[c("link-color", "component-active-bg")]),
               c("#562C8C", "#562C8C"))
  # Source Sans 3 and Montserrat, with the brand's alternatives where they
  # are not installed
  expect_match(vars[["font-family-base"]], "^\"Source Sans 3\".*Calibri")
  expect_match(vars[["headings-font-family"]], "^Montserrat.*Arial")

})

test_that("the theme and app colours are the brand's Nightshade and Iris", {

  brand <- testthat::test_path("..", "..", "dev", "reference", "act-brand.md")
  skip_if_not(file.exists(brand), "dev/reference/act-brand.md is not here")
  lines <- readLines(brand, warn = FALSE)
  hex <- function(name) {
    line <- grep(paste0("^\\| ", name, " \\|"), lines, value = TRUE)[1]
    toupper(regmatches(line, regexpr("#[0-9A-Fa-f]{6}", line)))
  }

  withr::local_options(sass.cache = FALSE)
  vars <- bslib::bs_get_variables(.epi_theme(), c("primary", "link-color"))
  expect_equal(toupper(unname(vars)), c(hex("Nightshade"), hex("Iris")))

  css <- as.character(.epi_app_style())
  primary <- regmatches(css, regexpr("--epi-primary:\\s*#[0-9A-Fa-f]{6}", css))
  accent  <- regmatches(css, regexpr("--epi-accent:\\s*#[0-9A-Fa-f]{6}", css))
  expect_equal(toupper(sub(".*#", "#", c(primary, accent))),
               c(hex("Nightshade"), hex("Iris")))

})

test_that("the logo is embedded in the page", {

  logo <- .epi_logo(class = "epi-logo")

  expect_s3_class(logo, "shiny.tag")
  expect_equal(logo$name, "img")
  expect_match(logo$attribs$src, "^data:image/svg\\+xml;base64,[A-Za-z0-9+/=]+$")
  expect_equal(logo$attribs$alt, "actepir")
  expect_equal(logo$attribs$class, "epi-logo")

})

test_that("the apps' lockup is the hex sticker's pyramid and name", {

  lockup <- readLines(system.file("www", "logo-lockup.svg", package = "actepir"),
                      warn = FALSE)
  # The sticker's master is installed in help/figures, and is in man/figures
  # when the package is loaded from source
  master <- system.file("help", "figures", "logo.svg", package = "actepir")
  if (!nzchar(master)) master <- test_path("..", "..", "man", "figures", "logo.svg")
  skip_if_not(file.exists(master), "the hex sticker's master is not available")
  master <- readLines(master, warn = FALSE)

  # The same bars, in the same colours, and the same outlined name
  pick <- function(x, pattern) trimws(grep(pattern, x, value = TRUE))
  for (pattern in c("<rect ", "<g id=\"(males|females)\"")) {
    expect_equal(pick(lockup, pattern), pick(master, pattern))
  }
  d <- function(x) sub(".* d=\"([^\"]+)\".*", "\\1", pick(x, "id=\"name\""))
  expect_equal(d(lockup), d(master))
  expect_match(pick(lockup, "id=\"name\""), "fill=\"#FFFFFF\"", fixed = TRUE)

})

test_that("each app has its own add-in and opens in the web browser", {

  dcf <- read.dcf(system.file("rstudio", "addins.dcf", package = "actepir"))
  apps <- dcf[dcf[, "Binding"] %in% c("episerver_dsr", "episerver_browse"), ,
              drop = FALSE]
  expect_equal(unname(apps[, "Name"]),
               c("Open DSR Calculator", "Open Dataset Browser"))
  expect_true(all(dcf[, "Binding"] %in% getNamespaceExports("actepir")))
  expect_false("addin_browse" %in% getNamespaceExports("actepir"))

  # No display is offered: both open in the web browser
  for (fn in list(episerver_dsr, episerver_browse, .run_app)) {
    expect_false("display" %in% names(formals(fn)))
  }

})

test_that("a background app runs in one unsupervised process, reused while it serves", {

  # A server on a free port stands in for the app's
  port <- httpuv::randomPort()
  server <- httpuv::startServer("127.0.0.1", port, list())
  withr::defer(httpuv::stopServer(server))
  withr::defer(suppressWarnings(rm(list = c("test_proc", "test_url", "test_port"),
                                   envir = .actepir_env)))
  withr::local_options(browser = function(url) invisible(NULL))

  # Each process started, by whether it was supervised. As under AppLocker,
  # a supervised start fails after its process has started.
  spawned <- logical()
  local_mocked_bindings(r_bg = function(func, args, ..., supervise = FALSE) {
    spawned <<- c(spawned, supervise)
    writeLines(as.character(port), args$portfile)
    if (supervise) stop("system error 1260")
    list(is_alive = function() TRUE, kill = function() invisible(TRUE))
  }, .package = "callr")
  run <- function() {
    capture_messages(.run_app("test", "Test app", "test_app",
                              "episerver_browse_app"))
  }

  msgs <- run()
  expect_identical(spawned, FALSE)
  expect_match(msgs[1], paste0("Test app running in a background process at ",
                               "http://127.0.0.1:", port), fixed = TRUE)
  expect_no_match(paste(msgs, collapse = ""), "supervision")
  expect_equal(.actepir_env$test_port, port)

  # Opened again while it serves, the app is reused
  msgs <- run()
  expect_length(spawned, 1)
  expect_match(msgs, "Test app is already running", fixed = TRUE)

})

test_that("selected table rows are Lavender with Nightshade text and an Iris bar", {

  css <- as.character(.epi_app_style())
  rule <- function(selector) {
    regmatches(css, regexpr(paste0(selector, "[^{]*\\{[^}]*\\}"), css))
  }

  # In Bootstrap 5, bslib sets DataTables' variables on .table.dataTable
  vars <- rule(":root, \\.table\\.dataTable\\.dataTable \\{")
  expect_match(vars, "--dt-row-selected: 220, 200, 250;")
  expect_match(vars, "--dt-row-selected-text: 50, 5, 87;")
  expect_match(css, "--epi-select-bg:\\s*#DCC8FA")
  expect_match(rule("table\\.dataTable tbody tr\\.selected > td:first-child"),
               "inset 4px 0 0 var\\(--epi-accent\\)")

})
