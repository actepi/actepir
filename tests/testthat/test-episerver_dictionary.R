# tests/testthat/test-episerver_dictionary.R
#
# Offline tests for episerver_dictionary(), dictionary_logo() and the Dataset
# Browser's Download menu. The queries go to fake_browse_episerver()
# (helper-browse.R); Quarto is mocked, except in the test that compiles a
# PDF, which runs where Quarto is found. The live tests are in
# test-episerver_dictionary_live.R.

library(testthat)
library(actepir)

subtitle <- "Epidemiology Section, ACT Health and Community Services Directorate"

# A PNG's signature and chunks, with zeros for the CRCs, which are not read:
# IHDR of the size and colour type given, a tRNS chunk if asked, and image
# data of `bytes` bytes
fake_png <- function(path, width, height, colour = 6L, trns = FALSE,
                     bytes = 16L) {
  int <- function(x) as.raw(c(x %/% 2^24, x %/% 2^16 %% 256, x %/% 256 %% 256,
                              x %% 256))
  chunk <- function(type, data) c(int(length(data)), charToRaw(type), data,
                                  as.raw(c(0, 0, 0, 0)))
  writeBin(c(
    as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a)),
    chunk("IHDR", c(int(width), int(height), as.raw(c(8, colour, 0, 0, 0)))),
    if (trns) chunk("tRNS", as.raw(c(0, 0))),
    chunk("IDAT", raw(bytes)),
    chunk("IEND", raw(0))
  ), path)
  path
}

# A lazy table of db.schema.table whose connection is never used
lazy_table <- function(table, schema = "dbo", db = "Analysis") {
  path <- structure(sprintf("[%s].[%s].[%s]", db, schema, table),
                    class = c("dbplyr_table_path", "character"))
  structure(list(lazy_query = list(x = list(x = path)),
                 src = list(con = "lazy connection")),
            class = c("tbl_sql", "tbl_lazy", "tbl"))
}

test_that("a dictionary holds the table's columns and labels, as the browser shows them", {

  fake <- fake_browse_episerver()
  d <- .dict_build(fake$query, "Analysis", "dbo", "apc")

  expect_equal(d$table, "APC")
  expect_equal(d$name, "Analysis.dbo.APC")
  expect_equal(d$type, "table")
  expect_equal(d$source, "ref.DataProperties")
  expect_equal(d$label, "Admitted patient care")
  expect_equal(d$notes, c("Coded first.", "Coded second.", "Not coded."))
  expect_equal(d$links$Table, "APC_Extra")

  # The columns in the table's order, with the browser's types and labels
  cols <- d$columns
  expect_equal(nrow(cols), 43)
  expect_equal(cols$position, 1:43)
  sex <- cols[cols$column == "Sex", ]
  expect_equal(sex$description, "Sex of the patient")
  expect_equal(sex$values[[1]]$Code, c("1", "2", "9", "10"))
  expect_equal(sex$values[[1]]$Text, c("Male", "Female", "Not stated", "Other"))
  expect_equal(cols$notes[[match("AgeYrs", cols$column)]],
               c("Second note.", "Tenth note.", "Uncoded note."))
  shown <- .browse_columns(fake$query(.browse_sql_columns("Analysis", "dbo", "APC")),
                           .lbl_table(.lbl_read(fake$query, "Analysis", "dbo", "APC")$rows),
                           values = TRUE)
  expect_equal(cols$type, shown$Type)

  # A view; a table without labels; one label table named
  expect_equal(.dict_build(fake$query, "Analysis", "dbo", "vw_ED")$type, "view")
  plain <- .dict_build(fake$query, "Analysis", "dbo", "Plain")
  expect_true(is.na(plain$source))
  expect_true(is.na(plain$label))
  expect_true(all(is.na(plain$columns$description)))
  named <- .dict_build(fake$query, "Analysis", "dbo", "APC",
                       c("Analysis", "ref", "DataLabels"))
  expect_equal(named$source, "ref.DataLabels")
  expect_equal(named$columns$description[named$columns$column == "Sex"],
               "Old sex label")
  expect_error(.dict_build(fake$query, "Analysis", "dbo", "Nowhere"),
               "Analysis.dbo.Nowhere is not a table or view that you can read",
               fixed = TRUE)

  # A label table of another database is named with its database
  other <- .dict_assemble(list(db = "HospitalAPC", schema = "dbo", table = "APC",
                               type = "table"),
                          fake$query(.browse_sql_columns("Analysis", "dbo", "APC")),
                          .lbl_normalise(NULL),
                          data.frame(TABLE_SCHEMA = "ref", TABLE_NAME = "DataLabels"),
                          "Analysis")
  expect_equal(other$source, "Analysis.ref.DataLabels")

})

test_that("the dictionary as data has a row per value label", {

  d <- .dict_build(fake_browse_episerver()$query, "Analysis", "dbo", "APC")
  flat <- .dict_flat(d)
  expect_named(flat, c("table", "position", "column", "description", "type",
                       "code", "value_label", "notes"))
  expect_equal(nrow(flat), 43 - 1 + 4)
  expect_equal(flat$code[flat$column == "Sex"], c("1", "2", "9", "10"))
  expect_true(is.na(flat$code[flat$column == "PatientID"]))
  expect_equal(flat$notes[flat$column == "AgeYrs"],
               "Second note.\nTenth note.\nUncoded note.")
  expect_equal(unique(flat$table), "Analysis.dbo.APC")

})

test_that("the files' text falls back to the table's name, and limits the PDF's value labels", {

  fake <- fake_browse_episerver()
  day <- as.Date("2026-10-06")
  apc <- .dict_build(fake$query, "Analysis", "dbo", "APC")
  plain <- .dict_build(fake$query, "Analysis", "dbo", "Plain")

  expect_equal(.dict_text(apc, day)[c("heading", "subheading", "source", "date")],
               list(heading = "Admitted patient care",
                    subheading = "Analysis.dbo.APC: a table of 43 columns",
                    source = "Labels from ref.DataProperties.",
                    date = "6 October 2026"))
  expect_equal(.dict_text(plain, day)[c("heading", "subheading", "source")],
               list(heading = "Plain",
                    subheading = "Analysis.dbo.Plain: a table of 1 column",
                    source = "No label table holds labels for this table."))

  data <- .dict_data(apc, "Data dictionary", subtitle, 2, "logo.svg", day)
  sex <- data$columns[[match("Sex", apc$columns$column)]]
  expect_equal(unlist(sex$values), c("1 = Male", "2 = Female"))
  expect_equal(sex$more, "and 2 more value labels; see the HTML dictionary")
  all <- .dict_data(apc, "Data dictionary", subtitle, Inf)$columns
  expect_length(all[[match("Sex", apc$columns$column)]]$values, 4)
  expect_true(is.na(all[[match("Sex", apc$columns$column)]]$more))
  expect_true(data$has_notes)
  expect_false(.dict_data(plain, "Data dictionary", subtitle)$has_notes)

  # In JSON, lists stay arrays, even of one item or none
  json <- jsonlite::toJSON(data, auto_unbox = TRUE, null = "null", na = "null")
  back <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  expect_equal(back$columns[[1]]$values, list())
  expect_equal(back$notes[[1]], list("Coded first."))
  expect_equal(back$logo, "logo.svg")

})

test_that("the HTML file stands alone, escapes the labels and holds every value label", {

  withr::local_envvar(R_USER_CONFIG_DIR = withr::local_tempdir())
  d <- .dict_build(fake_browse_episerver()$query, "Analysis", "dbo", "APC")
  d$columns$description[2] <- "<script>alert(1)</script> & co"
  d$columns$values[[2]]$Text[1] <- "Ends </script> here"
  file <- file.path(withr::local_tempdir(), "APC_dictionary.html")
  .dict_html(d, file, "Data dictionary", subtitle, as.Date("2026-10-26"))
  html <- paste(readLines(file, encoding = "UTF-8"), collapse = "\n")

  expect_match(html, "^<!DOCTYPE html>\n<html lang=\"en-AU\">\n<head>")
  expect_match(html, "<title>Admitted patient care - Data dictionary</title>",
               fixed = TRUE)
  expect_match(html, "<style>/* inst/dictionary/dictionary.css", fixed = TRUE)
  # Nothing is fetched from elsewhere: the logo is written in
  expect_no_match(html, "<link|src=\"http|@import")
  expect_match(html, "<img src=\"data:image/svg+xml;base64,", fixed = TRUE)
  # Text from the label tables is escaped, in the page and in the JSON
  expect_match(html, "&lt;script&gt;alert(1)&lt;/script&gt; &amp; co", fixed = TRUE)
  expect_no_match(html, "<script>alert", fixed = TRUE)
  json <- regmatches(html, regexpr("(?s)id=\"dictionary-data\">.*?</script>", html,
                                   perl = TRUE))
  json <- sub("^id=\"dictionary-data\">(.*)</script>$", "\\1", json)
  expect_no_match(json, "<", fixed = TRUE)
  data <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  expect_equal(data$columns[[2]]$values[[1]], "1 = Ends </script> here")
  expect_equal(lengths(regmatches(html, gregexpr("</script>", html, fixed = TRUE))), 2)
  # Every value label, folded, and the search
  expect_match(html, "<summary>4 value labels</summary>", fixed = TRUE)
  expect_match(html, "<td>10</td>\\s*<td>Other</td>")
  expect_match(html, "<input type=\"search\" id=\"search\"", fixed = TRUE)
  expect_match(html, "<footer>Analysis.dbo.APC  |  Labels from ref.DataProperties.  |  26 October 2026</footer>",
               fixed = TRUE)

  # A logo that has been set is written in instead
  dictionary_logo_quiet <- function(...) suppressMessages(dictionary_logo(...))
  dictionary_logo_quiet(fake_png(tempfile(fileext = ".png"), 1400, 430))
  .dict_html(d, file, "Data dictionary", subtitle)
  expect_match(paste(readLines(file), collapse = ""),
               "<img src=\"data:image/png;base64,[^\"]+\" alt=\"Logo\"")

})

test_that("the PDF is compiled from the template, the dictionary as JSON and the logo", {

  withr::local_envvar(R_USER_CONFIG_DIR = withr::local_tempdir())
  d <- .dict_build(fake_browse_episerver()$query, "Analysis", "dbo", "APC")
  out <- file.path(withr::local_tempdir(), "APC_dictionary.pdf")
  seen <- NULL
  local_mocked_bindings(.dict_run_quarto = function(quarto, typ, pdf) {
    seen <<- list(quarto = quarto, dir = dirname(typ),
                  files = sort(list.files(dirname(typ))),
                  data = jsonlite::read_json(file.path(dirname(typ),
                                                       "dictionary.json")))
    writeLines("%PDF-1.7", pdf)
    character()
  })

  .dict_pdf(d, out, "Data dictionary", subtitle, 2, quarto = "quarto")
  expect_equal(readLines(out), "%PDF-1.7")
  expect_equal(seen$quarto, "quarto")
  expect_equal(seen$files, c("dictionary.json", "dictionary.typ", "logo.svg"))
  expect_equal(seen$data$logo, "logo.svg")
  expect_equal(seen$data$heading, "Admitted patient care")
  expect_length(seen$data$columns[[2]]$values, 2)
  expect_false(dir.exists(seen$dir))

  # Typst's messages are passed on when it makes no PDF
  local_mocked_bindings(.dict_run_quarto = function(quarto, typ, pdf) {
    "error: unknown font"
  })
  expect_error(.dict_pdf(d, out, "Data dictionary", subtitle, quarto = "quarto"),
               "The PDF could not be made. Typst said:\nerror: unknown font",
               fixed = TRUE)
  expect_error(.dict_pdf(d, out, "Data dictionary", subtitle, quarto = NULL),
               "The PDF needs Quarto", fixed = TRUE)

})

test_that("Quarto is found on the PATH or beside RStudio's pandoc", {

  bin <- withr::local_tempdir()
  for (f in c("quarto", "quarto.exe")) {
    file.create(file.path(bin, f))
    Sys.chmod(file.path(bin, f), "755")
  }
  rstudio <- withr::local_tempdir()
  dir.create(file.path(rstudio, "tools"))
  file.create(file.path(rstudio, "quarto.exe"))
  empty <- withr::local_tempdir()

  withr::local_envvar(PATH = bin, RSTUDIO_PANDOC = "")
  expect_equal(normalizePath(dirname(.dict_quarto())), normalizePath(bin))
  withr::local_envvar(PATH = empty, RSTUDIO_PANDOC = file.path(rstudio, "tools"))
  expect_equal(.dict_quarto(), file.path(rstudio, "quarto.exe"))
  withr::local_envvar(PATH = empty, RSTUDIO_PANDOC = "")
  expect_null(.dict_quarto())

})

test_that("episerver_dictionary() writes each table's files and returns the dictionaries", {

  fake <- fake_browse_episerver()
  connected <- 0
  disconnected <- 0
  local_mocked_bindings(episerver_connect = function(...) {
    connected <<- connected + 1
    "named connection"
  })
  local_mocked_bindings(
    dbGetQuery = function(conn, statement, ...) fake$query(statement),
    dbDisconnect = function(conn, ...) disconnected <<- disconnected + 1,
    .package = "DBI"
  )
  local_mocked_bindings(.dict_quarto = function() "quarto",
                        .dict_run_quarto = function(quarto, typ, pdf) {
                          writeLines("%PDF-1.7", pdf)
                        })
  withr::local_envvar(R_USER_CONFIG_DIR = withr::local_tempdir())
  dir <- withr::local_tempdir()

  expect_message(
    out <- episerver_dictionary(c("APC", "Analysis.dbo.Plain"), dir = dir),
    "Wrote"
  )
  expect_setequal(list.files(dir),
                  c("APC_dictionary.pdf", "APC_dictionary.html",
                    "Plain_dictionary.pdf", "Plain_dictionary.html"))
  expect_s3_class(out, "tbl_df")
  expect_equal(unique(out$table), c("Analysis.dbo.APC", "Analysis.dbo.Plain"))
  expect_equal(c(connected, disconnected), c(1, 1))

  # No files: the data alone; one label table named
  data <- episerver_dictionary("APC", formats = character(), dir = "no such folder",
                               lbl_table = "DataLabels")
  expect_equal(unique(data$description[data$column == "Sex"]), "Old sex label")
  # A lazy table brings its connection
  lazy <- episerver_dictionary(lazy_table("ED"), formats = "html", dir = dir)
  expect_true(file.exists(file.path(dir, "ED_dictionary.html")))
  expect_equal(unique(lazy$table), "Analysis.dbo.ED")
  expect_equal(connected, 2)

  # Mistakes are found before connecting
  expect_error(episerver_dictionary("APC", formats = "docx"),
               "formats can be \"pdf\" and \"html\", not \"docx\"", fixed = TRUE)
  expect_error(episerver_dictionary("dbo.APC", dir = dir),
               "not dbo.APC", fixed = TRUE)
  expect_error(episerver_dictionary(c("APC", "apc"), dir = dir),
               "The tables APC, apc have the same name", fixed = TRUE)
  expect_error(episerver_dictionary("APC", dir = file.path(dir, "none")),
               "There is no folder", fixed = TRUE)
  expect_error(episerver_dictionary("APC", max_values = -1, dir = dir),
               "max_values", fixed = TRUE)
  local_mocked_bindings(.dict_quarto = function() NULL)
  expect_error(episerver_dictionary("APC", dir = dir), "The PDF needs Quarto",
               fixed = TRUE)
  expect_equal(connected, 2)

})

test_that("dictionary_logo() keeps a logo for the user, and checks it", {

  config <- withr::local_tempdir()
  withr::local_envvar(R_USER_CONFIG_DIR = config)
  png <- function(...) fake_png(tempfile(fileext = ".png"), ...)

  expect_message(lockup <- dictionary_logo(), "show the actepir lockup")
  expect_equal(basename(lockup), "logo-lockup.svg")
  expect_equal(.dict_logo()$mime, "image/svg+xml")

  expect_no_warning(expect_message(stored <- dictionary_logo(png(1400, 430)),
                                   "The dictionaries will show"))
  expect_true(startsWith(normalizePath(stored), normalizePath(config)))
  expect_equal(.dict_logo()$mime, "image/png")
  expect_message(dictionary_logo(), "(1400 by 430 pixels)", fixed = TRUE)

  # Small, opaque or large logos are kept, with a warning
  expect_warning(suppressMessages(dictionary_logo(png(300, 90))),
                 "300 pixels wide")
  expect_warning(suppressMessages(dictionary_logo(png(1400, 430, colour = 2L))),
                 "no transparent background")
  expect_no_warning(suppressMessages(dictionary_logo(png(1400, 430, colour = 2L,
                                                         trns = TRUE))))
  expect_warning(suppressMessages(dictionary_logo(png(1400, 430,
                                                      bytes = 600 * 1024))),
                 "every HTML dictionary carries it")

  expect_error(dictionary_logo(file.path(config, "none.png")), "There is no file")
  text <- tempfile(fileext = ".png")
  writeLines("not a PNG", text)
  expect_error(dictionary_logo(text), "is not a PNG file")
  expect_error(dictionary_logo(text, remove = TRUE), "not both")

  expect_message(dictionary_logo(remove = TRUE), "actepir lockup")
  expect_false(file.exists(stored))
  expect_equal(.dict_logo()$ext, "svg")

})

test_that("the browser's Download menu gives the dictionary of the table chosen", {

  withr::local_envvar(R_USER_CONFIG_DIR = withr::local_tempdir())
  local_mocked_bindings(isAvailable = function(...) FALSE,
                        .package = "rstudioapi")
  page <- function() {
    app <- episerver_browse_app(query = fake_browse_episerver()$query)
    app$httpHandler(list(PATH_INFO = "/", REQUEST_METHOD = "GET",
                         QUERY_STRING = ""))$content
  }
  pdf_item <- function(html) {
    regmatches(html, regexpr("(?s)<ul class=\"dropdown-menu\">\\s*<li>.*?</li>",
                             html, perl = TRUE))
  }

  # Without Quarto, the PDF's item is disabled, and is not a link
  local_mocked_bindings(.dict_quarto = function() NULL)
  withr::local_options(sass.cache = FALSE)
  item <- pdf_item(page())
  expect_match(item, "<span class=\"dropdown-item disabled\" aria-disabled=\"true\">",
               fixed = TRUE)
  expect_match(item, "Dictionary as PDF (needs Quarto)", fixed = TRUE)
  expect_no_match(item, "dict_pdf", fixed = TRUE)
  local_mocked_bindings(.dict_quarto = function() "quarto",
                        .dict_run_quarto = function(quarto, typ, pdf) {
                          writeLines(readLines(sub("typ$", "json", typ)), pdf)
                        })
  expect_match(pdf_item(page()), "id=\"dict_pdf\"", fixed = TRUE)

  app <- episerver_browse_app(query = fake_browse_episerver()$query)
  shiny::testServer(app, {
    # Value labels not shown are read for the dictionary
    session$setInputs(db = "Analysis", schema = "dbo", labels_source = "auto",
                      show_values = FALSE, table = "APC")
    file <- output$dict_html
    expect_equal(basename(file), "APC_dictionary.html")
    html <- paste(readLines(file), collapse = "\n")
    expect_match(html, "<h1>Admitted patient care</h1>", fixed = TRUE)
    expect_match(html, "<summary>4 value labels</summary>", fixed = TRUE)

    # The label table chosen
    session$setInputs(labels_source = "ref.DataLabels", show_values = TRUE)
    html <- paste(readLines(output$dict_html), collapse = "\n")
    expect_match(html, "Labels from ref.DataLabels.", fixed = TRUE)
    expect_match(html, "Old sex label", fixed = TRUE)

    # The PDF, from the same dictionary
    file <- output$dict_pdf
    expect_equal(basename(file), "APC_dictionary.pdf")
    data <- jsonlite::fromJSON(file)
    expect_equal(data$source, "Labels from ref.DataLabels.")
  })

})

test_that("the PDF compiles with the Quarto found", {

  quarto <- .dict_quarto()
  skip_if(is.null(quarto), "Quarto is not installed")
  withr::local_envvar(R_USER_CONFIG_DIR = withr::local_tempdir())
  fake <- fake_browse_episerver()
  dir <- withr::local_tempdir()
  for (t in c("APC", "Plain")) {
    file <- file.path(dir, paste0(t, ".pdf"))
    .dict_pdf(.dict_build(fake$query, "Analysis", "dbo", t), file,
              "Data dictionary", subtitle, quarto = quarto)
    expect_equal(readBin(file, "raw", 4), charToRaw("%PDF"), label = t)
  }

})
