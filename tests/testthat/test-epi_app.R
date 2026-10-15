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
