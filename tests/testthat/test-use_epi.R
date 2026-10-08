# tests/testthat/test-use_epi.R
#
# Offline coverage for the Quarto scaffolding. Nothing here renders: the tests
# read the files the functions write, which is all that can be checked without
# Quarto installed.

library(testthat)
library(actepir)

# ── Internal helpers ────────────────────────────────────────────────────────

test_that(".as_flag coerces wizard widget values", {

  expect_true(.as_flag(TRUE))
  expect_false(.as_flag(FALSE))

  # The RStudio project wizard hands every widget value over as a string.
  expect_true(.as_flag("TRUE"))
  expect_false(.as_flag("FALSE"))

  # Anything that is not a logical or a parseable string is off.
  expect_false(.as_flag(NULL))
  expect_false(.as_flag(NA))
  expect_false(.as_flag("maybe"))

  # Only the first element of a string value is read.
  expect_true(.as_flag(c("TRUE", "FALSE")))

})

test_that(".strip_block keeps or drops a tagged block", {

  tmpl <- c("before", "<!-- BEGIN:x -->", "inside", "<!-- END:x -->", "after")

  # keep = TRUE removes the markers only
  expect_equal(.strip_block(tmpl, "x", keep = TRUE), c("before", "inside", "after"))

  # keep = FALSE removes the markers and everything between them
  expect_equal(.strip_block(tmpl, "x", keep = FALSE), c("before", "after"))

})

test_that(".strip_block handles repeated tags and mixed comment syntax", {

  # The same tag appears in the YAML header and again in the body of the
  # skeleton, with different comment syntax around each marker.
  tmpl <- c("a", "# BEGIN:y", "1", "# END:y",
            "b", "<!-- BEGIN:y -->", "2", "<!-- END:y -->", "c")

  expect_equal(.strip_block(tmpl, "y", keep = TRUE),  c("a", "1", "b", "2", "c"))
  expect_equal(.strip_block(tmpl, "y", keep = FALSE), c("a", "b", "c"))

})

test_that(".strip_block leaves malformed markers alone", {

  # An unclosed block would otherwise swallow the rest of the document.
  unclosed <- c("a", "# BEGIN:z", "1")
  expect_equal(.strip_block(unclosed, "z", keep = FALSE), unclosed)

  # END before BEGIN
  inverted <- c("# END:z", "a", "# BEGIN:z")
  expect_equal(.strip_block(inverted, "z", keep = FALSE), inverted)

  # A tag that is not in the template at all
  expect_equal(.strip_block(c("a", "b"), "z", keep = FALSE), c("a", "b"))

})

test_that(".strip_block does not confuse bib with nobib", {

  tmpl <- c("<!-- BEGIN:bib -->", "cited", "<!-- END:bib -->",
            "<!-- BEGIN:nobib -->", "uncited", "<!-- END:nobib -->")

  expect_equal(.strip_block(tmpl, "bib", keep = FALSE),
               c("<!-- BEGIN:nobib -->", "uncited", "<!-- END:nobib -->"))

})

test_that(".resolve_data_flags passes a consistent set through", {

  expect_equal(.resolve_data_flags(TRUE, TRUE, FALSE),
               list(conn = TRUE, sel = TRUE, sql = FALSE))

  expect_equal(.resolve_data_flags("TRUE", "FALSE", "TRUE"),
               list(conn = TRUE, sel = FALSE, sql = TRUE))

})

test_that(".resolve_data_flags drops sel and sql without conn", {

  # Both chunks reference the con121 object the connection chunk creates.
  expect_message(
    flags <- .resolve_data_flags(FALSE, TRUE, TRUE),
    regexp = "both need the odbc connection chunk"
  )
  expect_equal(flags, list(conn = FALSE, sel = FALSE, sql = FALSE))

  # Nothing to reconcile, so nothing to report.
  expect_message(.resolve_data_flags(FALSE, FALSE, FALSE), regexp = NA)

})

test_that(".epi_asset_dir finds the installed assets", {

  src <- .epi_asset_dir()

  expect_true(dir.exists(src))
  expect_true(dir.exists(file.path(src, "themes")))
  expect_true(file.exists(file.path(src, "skeleton", "analysis.qmd")))
  expect_true(file.exists(file.path(src, "skeleton", "references.yaml")))

})

test_that(".epi_available_themes lists the bundled themes", {

  themes <- .epi_available_themes(.epi_asset_dir())

  expect_type(themes, "character")
  expect_true(all(c("acthcsd", "acthd") %in% themes))
  expect_false(any(grepl("\\.scss$", themes)))

})

test_that(".resolve_theme accepts a bundled theme and rejects others", {

  src <- .epi_asset_dir()

  expect_equal(.resolve_theme(src, "acthd"), "acthd")

  expect_error(.resolve_theme(src, "notatheme"),
               regexp = "No such theme")

  # The error names what is available, so the analyst can correct it.
  expect_error(.resolve_theme(src, "notatheme"), regexp = "acthcsd")

})

test_that(".write_epi_qmd substitutes placeholders and strips markers", {

  dir <- withr::local_tempdir()
  qmd <- file.path(dir, "out.qmd")

  .write_epi_qmd(qmd, .epi_asset_dir(), "acthd",
                 list(conn = TRUE, sel = TRUE, sql = TRUE),
                 bib = TRUE, title = "A Worked Title")

  lines <- readLines(qmd, warn = FALSE)

  expect_true(any(grepl('title: "A Worked Title"', lines, fixed = TRUE)))
  expect_true(any(grepl("theme: acthd.scss", lines, fixed = TRUE)))
  expect_true(any(grepl(format(Sys.Date()), lines, fixed = TRUE)))

  # No placeholder or block marker may survive into the written document.
  expect_false(any(grepl("{{", lines, fixed = TRUE)))
  expect_false(any(grepl("BEGIN:", lines, fixed = TRUE)))
  expect_false(any(grepl("END:", lines, fixed = TRUE)))

})

test_that(".write_epi_qmd keeps only the requested blocks", {

  dir <- withr::local_tempdir()
  src <- .epi_asset_dir()

  all_on <- file.path(dir, "all.qmd")
  .write_epi_qmd(all_on, src, "acthcsd",
                 list(conn = TRUE, sel = TRUE, sql = TRUE),
                 bib = TRUE, title = "All")
  on <- readLines(all_on, warn = FALSE)

  expect_true(any(grepl("label: conn121",     on, fixed = TRUE)))
  expect_true(any(grepl("label: lazy_alias",  on, fixed = TRUE)))
  expect_true(any(grepl("label: selection-sql", on, fixed = TRUE)))
  expect_true(any(grepl("bibliography: references.yaml", on, fixed = TRUE)))

  all_off <- file.path(dir, "none.qmd")
  .write_epi_qmd(all_off, src, "acthcsd",
                 list(conn = FALSE, sel = FALSE, sql = FALSE),
                 bib = FALSE, title = "None")
  off <- readLines(all_off, warn = FALSE)

  expect_false(any(grepl("label: conn121",      off, fixed = TRUE)))
  expect_false(any(grepl("label: lazy_alias",   off, fixed = TRUE)))
  expect_false(any(grepl("label: selection-sql", off, fixed = TRUE)))
  expect_false(any(grepl("bibliography: references.yaml", off, fixed = TRUE)))

  # bib and nobib are mutually exclusive prose, so exactly one must remain.
  expect_true(any(grepl("All analysis is performed in RStudio.", off, fixed = TRUE)))
  expect_false(any(grepl("All analysis is performed in RStudio.", on, fixed = TRUE)))

})

# ── use_epi_analysis() ──────────────────────────────────────────────────────

test_that("use_epi_analysis scaffolds a whole project", {

  dir <- withr::local_tempdir()
  proj <- file.path(dir, "rsv-severity-2026")

  qmd <- suppressMessages(use_epi_analysis(proj, title = "RSV Severity"))

  expect_equal(qmd, file.path(normalizePath(proj, winslash = "/"),
                              "rsv-severity-2026.qmd"))
  expect_true(file.exists(qmd))

  # Every bundled theme is copied in, so switching is a one-line edit.
  expect_true(file.exists(file.path(proj, "acthcsd.scss")))
  expect_true(file.exists(file.path(proj, "acthd.scss")))

  expect_true(file.exists(file.path(proj, "references.yaml")))
  expect_true(file.exists(file.path(proj, "rsv-severity-2026.Rproj")))

  for (d in c("data", "output", "images")) {
    expect_true(dir.exists(file.path(proj, d)))
    expect_true(file.exists(file.path(proj, d, ".gitkeep")))
  }

  lines <- readLines(qmd, warn = FALSE)
  expect_true(any(grepl('title: "RSV Severity"', lines, fixed = TRUE)))
  expect_true(any(grepl("theme: acthcsd.scss", lines, fixed = TRUE)))

})

test_that("use_epi_analysis derives name and title from the directory", {

  dir <- withr::local_tempdir()
  proj <- file.path(dir, "flu-report-2026")

  qmd <- suppressMessages(use_epi_analysis(proj))

  expect_equal(basename(qmd), "flu-report-2026.qmd")
  expect_true(any(grepl('title: "Flu Report 2026"',
                        readLines(qmd, warn = FALSE), fixed = TRUE)))

})

test_that("use_epi_analysis honours the optional-section arguments", {

  dir <- withr::local_tempdir()
  proj <- file.path(dir, "minimal")

  qmd <- suppressMessages(
    use_epi_analysis(proj, conn = FALSE, sel = FALSE, bib = FALSE, dirs = FALSE)
  )
  lines <- readLines(qmd, warn = FALSE)

  expect_false(any(grepl("label: conn121", lines, fixed = TRUE)))
  expect_false(file.exists(file.path(proj, "references.yaml")))
  expect_false(dir.exists(file.path(proj, "data")))
  expect_false(dir.exists(file.path(proj, "output")))
  expect_false(dir.exists(file.path(proj, "images")))

})

test_that("use_epi_analysis reports when it drops dependent chunks", {

  dir <- withr::local_tempdir()

  msgs <- capture_messages(
    use_epi_analysis(file.path(dir, "p"), sel = TRUE, conn = FALSE)
  )

  expect_match(msgs, "dbplyr selection", all = FALSE)

})

test_that("use_epi_analysis refuses to replace a document unless told to", {

  dir <- withr::local_tempdir()
  proj <- file.path(dir, "guarded")

  suppressMessages(use_epi_analysis(proj, name = "report"))
  writeLines("analyst's own work", file.path(proj, "report.qmd"))

  expect_error(suppressMessages(use_epi_analysis(proj, name = "report")),
               regexp = "already exists")
  expect_equal(readLines(file.path(proj, "report.qmd"), warn = FALSE),
               "analyst's own work")

  suppressMessages(use_epi_analysis(proj, name = "report", overwrite = TRUE))
  expect_gt(length(readLines(file.path(proj, "report.qmd"), warn = FALSE)), 1)

})

test_that("use_epi_analysis rejects an unknown theme before writing anything", {

  dir <- withr::local_tempdir()
  proj <- file.path(dir, "badtheme")

  expect_error(use_epi_analysis(proj, theme = "notatheme"),
               regexp = "No such theme")
  expect_false(dir.exists(proj))

})

# ── use_epi_document() ──────────────────────────────────────────────────────

test_that("use_epi_document writes one document and its theme", {

  dir <- withr::local_tempdir()

  qmd <- suppressMessages(
    use_epi_document("subgroup analysis", path = dir, theme = "acthd")
  )

  expect_equal(basename(qmd), "subgroup-analysis.qmd")
  expect_true(file.exists(qmd))

  # Only the chosen theme travels with the document.
  expect_true(file.exists(file.path(dir, "acthd.scss")))
  expect_false(file.exists(file.path(dir, "acthcsd.scss")))

  # No project furniture.
  expect_false(dir.exists(file.path(dir, "data")))
  expect_length(list.files(dir, pattern = "\\.Rproj$"), 0)

  expect_true(any(grepl('title: "Subgroup Analysis"',
                        readLines(qmd, warn = FALSE), fixed = TRUE)))

})

test_that("use_epi_document requires a name", {

  dir <- withr::local_tempdir()

  expect_error(use_epi_document(path = dir), regexp = "supply a document")
  expect_error(use_epi_document("",   path = dir), regexp = "supply a document")
  expect_error(use_epi_document("  ", path = dir), regexp = "supply a document")

})

test_that("use_epi_document leaves a customised theme and bibliography alone", {

  dir <- withr::local_tempdir()

  writeLines("// customised by the analyst", file.path(dir, "acthcsd.scss"))
  writeLines("references: []", file.path(dir, "references.yaml"))

  suppressMessages(use_epi_document("adhoc", path = dir))

  expect_equal(readLines(file.path(dir, "acthcsd.scss"), warn = FALSE),
               "// customised by the analyst")
  expect_equal(readLines(file.path(dir, "references.yaml"), warn = FALSE),
               "references: []")

})

test_that("use_epi_document refuses to replace a document unless told to", {

  dir <- withr::local_tempdir()

  suppressMessages(use_epi_document("adhoc", path = dir))
  writeLines("analyst's own work", file.path(dir, "adhoc.qmd"))

  expect_error(suppressMessages(use_epi_document("adhoc", path = dir)),
               regexp = "already exists")
  expect_equal(readLines(file.path(dir, "adhoc.qmd"), warn = FALSE),
               "analyst's own work")

  suppressMessages(use_epi_document("adhoc", path = dir, overwrite = TRUE))
  expect_gt(length(readLines(file.path(dir, "adhoc.qmd"), warn = FALSE)), 1)

})

test_that("use_epi_document writes no bibliography when bib is off", {

  dir <- withr::local_tempdir()

  qmd <- suppressMessages(use_epi_document("nobib", path = dir, bib = FALSE))

  expect_false(file.exists(file.path(dir, "references.yaml")))
  expect_false(any(grepl("bibliography: references.yaml",
                         readLines(qmd, warn = FALSE), fixed = TRUE)))

})

# ── use_epi_themes() ────────────────────────────────────────────────────────

test_that("use_epi_themes copies every theme by default", {

  dir <- withr::local_tempdir()

  written <- expect_invisible(suppressMessages(use_epi_themes(dir)))

  expect_setequal(written, .epi_available_themes(.epi_asset_dir()))
  expect_true(file.exists(file.path(dir, "acthcsd.scss")))
  expect_true(file.exists(file.path(dir, "acthd.scss")))

})

test_that("use_epi_themes copies only the requested themes", {

  dir <- withr::local_tempdir()

  suppressMessages(use_epi_themes(dir, which = "acthcsd"))

  expect_true(file.exists(file.path(dir, "acthcsd.scss")))
  expect_false(file.exists(file.path(dir, "acthd.scss")))

})

test_that("use_epi_themes refreshes by default and respects overwrite = FALSE", {

  dir <- withr::local_tempdir()
  scss <- file.path(dir, "acthcsd.scss")

  writeLines("// stale", scss)
  suppressMessages(use_epi_themes(dir, which = "acthcsd", overwrite = FALSE))
  expect_equal(readLines(scss, warn = FALSE), "// stale")

  suppressMessages(use_epi_themes(dir, which = "acthcsd"))
  expect_gt(length(readLines(scss, warn = FALSE)), 1)

})

test_that("use_epi_themes rejects an unknown theme", {

  dir <- withr::local_tempdir()

  expect_error(use_epi_themes(dir, which = "notatheme"), regexp = "No such theme")
  expect_error(use_epi_themes(dir, which = c("acthcsd", "notatheme")),
               regexp = "notatheme")

  # Nothing is written when the request cannot be honoured in full.
  expect_length(list.files(dir, pattern = "\\.scss$"), 0)

})

test_that("use_epi_themes creates the target directory", {

  dir <- withr::local_tempdir()
  target <- file.path(dir, "new", "nested")

  suppressMessages(use_epi_themes(target, which = "acthd"))

  expect_true(file.exists(file.path(target, "acthd.scss")))

})
