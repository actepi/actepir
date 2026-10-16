# tests/testthat/test-dsr_help.R
#
# Offline tests for the Help tab of the DSR Calculator: it describes the
# definitions the calculator uses, with their current values.

library(testthat)
library(actepir)

test_that("the help gives the data, formulas and sources the calculator uses", {

  html <- as.character(.dsr_help())
  text <- gsub("\\s+", " ", gsub("<[^>]+>", " ", html))

  # Tables and fields
  for (d in .dsr_datasets) {
    expect_match(text, paste0("Analysis.dbo.", d$table), fixed = TRUE)
    expect_match(text, d$date, fixed = TRUE)
    expect_match(text, d$age, fixed = TRUE)
  }
  expect_match(text, "Diagnosis1 to Diagnosis100", fixed = TRUE)
  expect_match(text, "Analysis.dbo.ICD10AM", fixed = TRUE)
  expect_match(text, paste("Procedure1 to Procedure50, with their blocks in",
                           "BlockProcedure1 to BlockProcedure50"), fixed = TRUE)
  expect_match(text, "so procedure codes apply to APC only", fixed = TRUE)
  expect_match(text, "Analysis.dbo.StandardPops", fixed = TRUE)
  for (col in vapply(.dsr_levels, `[[`, character(1), "column")) {
    expect_match(text, col, fixed = TRUE)
  }

  # Areas, the ACT and surrounds, and the boundaries
  expect_match(text, .dsr_asgs$name, fixed = TRUE)
  for (code in names(.dsr_surrounds)) {
    expect_match(text, paste(code, .dsr_surrounds[[code]]), fixed = TRUE)
  }
  for (level in names(.dsr_levels)) {
    expect_match(text, .dsr_abs_layer(level), fixed = TRUE)
  }
  expect_match(text, "within 0.0002 degrees for ACT residents and 0.002 degrees",
               fixed = TRUE)
  expect_match(text, tools::R_user_dir("actepir", which = "cache"), fixed = TRUE)
  expect_match(text, "CC BY 4.0", fixed = TRUE)

  # Formulas and thresholds
  expect_match(html, "w<sub>i</sub> = S<sub>i</sub> / &Sigma; S<sub>j</sub>",
               fixed = TRUE)
  expect_match(text, "Dobson et al. 1991", fixed = TRUE)
  expect_match(text, "fewer than 20 events in total (Principle 3)",
               fixed = TRUE)
  expect_match(text, "a population under 30 in any one age group (Principle 2)",
               fixed = TRUE)
  expect_match(text, "Areas whose DSR is not presented", fixed = TRUE)
  expect_match(text, "P(X &ge; O a ) &lt; 0.025", fixed = TRUE)
  expect_match(text, "below 0.001", fixed = TRUE)
  expect_match(text, "z p is 1.960 (95%) or 3.090 (99.8%)", fixed = TRUE)
  expect_match(text, "ticked by default", fixed = TRUE)

  # References
  for (ref in c("Garwood F (1936)", "Spiegelhalter DJ (2005). Funnel plots",
                "Spiegelhalter DJ (2005). Handling over-dispersion",
                "Cat. no. CSI 12", "Cat. no. 3302.0",
                "Public Health England (2017)",
                "Australian Statistical Geography Standard (ASGS) Edition 3")) {
    expect_match(text, ref, fixed = TRUE)
  }

})

test_that("funnel positions use the ACT Government's regional colours", {

  # dev/ is left out of the built package, so this runs from the repository
  brand <- testthat::test_path("..", "..", "dev", "reference", "act-brand.md")
  skip_if_not(file.exists(brand), "dev/reference/act-brand.md is not here")
  lines <- readLines(brand, warn = FALSE)
  start <- grep("^## Regional colours", lines)
  end   <- grep("^## ", lines)
  end   <- min(end[end > start])
  region <- regmatches(lines[start:end],
                       regexpr("#[0-9A-Fa-f]{6}", lines[start:end]))

  outside <- .dsr_funnel_colours[names(.dsr_funnel_colours) != "Within 95% limits"]
  expect_length(region, 5)
  expect_true(all(toupper(outside) %in% toupper(region)))
  expect_equal(unname(.dsr_funnel_colours["Within 95% limits"]), "#CBCBC6")

})
