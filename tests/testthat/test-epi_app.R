test_that("primary buttons and Done go to Iris on hover", {

  css <- as.character(.epi_app_style())
  rule <- function(selector) {
    regmatches(css, regexpr(paste0(selector, "[^{]*\\{[^}]*\\}"), css))
  }

  expect_match(css, "--epi-accent:\\s*#562C8C")
  expect_match(rule("\\.btn-primary:hover"),
               "background-color: var\\(--epi-accent\\)")
  expect_match(rule("#done\\.btn-primary:hover"),
               "background-color: var\\(--epi-accent\\)")

})

test_that("a click leaves no highlight, but the keyboard's focus shows", {

  css <- as.character(.epi_app_style())
  rule <- function(selector) {
    regmatches(css, regexpr(paste0(selector, "[^{]*\\{[^}]*\\}"), css))
  }

  # Buttons are Iris for the keyboard's focus only, not after a click
  expect_match(rule("\\.btn-primary:hover"), ":focus-visible")
  expect_no_match(rule("\\.btn-primary:hover"), ":focus[,\\s{]")
  expect_no_match(rule("#done\\.btn-primary:hover"), ":focus[,\\s{]")
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

test_that("problem reports name the maintainer and describe the session", {

  m <- .epi_maintainer()
  expect_equal(m$name, "Warren Holroyd")
  expect_match(m$email, "^[^@<> ]+@[^@<> ]+$")

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
  expect_match(logo$attribs$src, "^data:image/png;base64,[A-Za-z0-9+/=]+$")
  expect_equal(logo$attribs$alt, "ACT Government")
  expect_equal(logo$attribs$class, "epi-logo")

})

test_that("each app has its own add-in and opens in the web browser", {

  dcf <- read.dcf(system.file("rstudio", "addins.dcf", package = "actepir"))
  apps <- dcf[dcf[, "Binding"] %in% c("episerver_dsr", "episerver_browse"), ,
              drop = FALSE]
  expect_equal(unname(apps[, "Name"]),
               c("Open DSR Calculator", "Open EpiServer Browser"))
  expect_true(all(dcf[, "Binding"] %in% getNamespaceExports("actepir")))
  expect_false("addin_browse" %in% getNamespaceExports("actepir"))

  # No display is offered: both open in the web browser
  for (fn in list(episerver_dsr, episerver_browse, .run_app)) {
    expect_false("display" %in% names(formals(fn)))
  }

})
