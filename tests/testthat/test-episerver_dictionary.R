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

test_that("the dictionary for Excel marks its cells as text, with a note's lines in one cell", {

  flat <- data.frame(
    table = "Analysis.dbo.APC", position = c(2L, 2L, 3L),
    column = c("Sex", "Sex", "Age<"), description = c("Sex", "Sex", "Age & years"),
    type = "int", code = c("01", "1-4", NA),
    value_label = c("Male", "Ages\t1 to 4", NA),
    notes = c(NA, NA, "First.\nSecond."), stringsAsFactors = FALSE
  )
  x <- .dict_excel(flat)
  # Text: headings, then a line for each row, tabs and line breaks as spaces
  lines <- strsplit(x$text, "\n")[[1]]
  expect_equal(lines[1], paste("Table", "Position", "Column", "Description",
                               "Type", "Code", "Value label", "Notes", sep = "\t"))
  expect_length(lines, 4)
  expect_equal(strsplit(lines[3], "\t")[[1]][6:7], c("1-4", "Ages 1 to 4"))
  expect_match(lines[4], "\tFirst. Second.$")
  # HTML: every cell but the position marked as text; text escaped
  text_cell <- "<td style='mso-number-format:\"\\@\"'>"
  rows <- regmatches(x$html, gregexpr("<tr>.*?</tr>", x$html))[[1]]
  expect_length(rows, 4)
  expect_equal(lengths(regmatches(rows[-1], gregexpr(text_cell, rows[-1], fixed = TRUE))),
               c(7, 7, 7))
  expect_match(rows[2], paste0(text_cell, "01</td>"), fixed = TRUE)
  expect_match(rows[2], "<td>2</td>", fixed = TRUE)
  expect_match(rows[4], "Age&lt;</td>", fixed = TRUE)
  expect_match(rows[4], "Age &amp; years</td>", fixed = TRUE)
  expect_match(rows[4], "First.<br style='mso-data-placement:same-cell'>Second.",
               fixed = TRUE)
  # A dictionary from the fake, and one without rows
  d <- .dict_flat(.dict_build(fake_browse_episerver()$query, "Analysis", "dbo", "APC"))
  expect_length(strsplit(.dict_excel(d)$text, "\n")[[1]], nrow(d) + 1)
  empty <- .dict_excel(.dict_flat_empty())
  expect_equal(empty$text, lines[1])
  expect_match(empty$html, "<tbody></tbody>", fixed = TRUE)

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
  expect_equal(back$logo, "logo.svg")
  # The description's paragraphs in order; the notes in groups under their
  # themes; a group without a heading (theme null) when no note has a theme
  expect_equal(back$description, list(list("First paragraph."),
                                      list("Second paragraph.")))
  expect_equal(back$note_groups, list(
    list(theme = "General", notes = list(list("Coded first."),
                                         list("Coded second."))),
    list(theme = "Data quality", notes = list(list("Not coded.")))
  ))
  ed <- .dict_data(.dict_build(fake$query, "Analysis", "dbo", "ED"),
                   "Data dictionary", subtitle)
  ed <- jsonlite::fromJSON(jsonlite::toJSON(ed, auto_unbox = TRUE, null = "null",
                                            na = "null"),
                           simplifyVector = FALSE)
  expect_null(ed$note_groups[[1]]$theme)
  expect_equal(ed$note_groups[[1]]$notes, list(list("Presentations to public hospitals.")))
  plain_data <- .dict_data(plain, "Data dictionary", subtitle)
  expect_length(plain_data$description, 0)
  expect_length(plain_data$note_groups, 0)

})

test_that("the HTML file has a Columns tab, shown first, and a Dataset information tab", {

  withr::local_envvar(R_USER_CONFIG_DIR = withr::local_tempdir())
  fake <- fake_browse_episerver()
  file <- withr::local_tempfile(fileext = ".html")
  .dict_write(.dict_build(fake$query, "Analysis", "dbo", "APC"), "html", file)
  html <- paste(readLines(file, encoding = "UTF-8"), collapse = "\n")
  # The tabs, the Columns tab chosen
  expect_match(html, paste0(
    "(?s)<div class=\"tabs\" role=\"tablist\">\\s*<button[^>]*id=\"tab-columns\"",
    "[^>]*aria-selected=\"true\"[^>]*>Columns</button>\\s*",
    "<button[^>]*id=\"tab-information\"[^>]*aria-selected=\"false\"[^>]*>",
    "Dataset information</button>"), perl = TRUE)
  expect_no_match(html, "Contents:", fixed = TRUE)
  # The dataset information: the description and the notes under their
  # themes, as points
  info <- regmatches(html, regexpr("(?s)<section id=\"information\".*?</section>",
                                   html, perl = TRUE))
  expect_match(info, paste0(
    "(?s)<h3>Dataset description</h3>\\s*<ul>\\s*<li>First paragraph.</li>\\s*",
    "<li>Second paragraph.</li>\\s*</ul>\\s*<h3>Dataset notes</h3>\\s*",
    "<h4>General</h4>\\s*<ul>\\s*<li>Coded first.</li>\\s*<li>Coded second.</li>",
    "\\s*</ul>\\s*<h4>Data quality</h4>\\s*<ul>\\s*<li>Not coded.</li>\\s*</ul>",
    "\\s*<h3>Linked tables</h3>\\s*<ul>\\s*<li>APC_Extra \\(on PatientID\\)</li>"),
    perl = TRUE)
  # The script shows the tabs; the search and buttons stick above the
  # table's headings; in print both sections show, without the tabs
  expect_match(html, 'show("columns");', fixed = TRUE)
  expect_match(html, "(?s)\\.tools \\{[^}]*position: sticky; top: 0;", perl = TRUE)
  expect_match(html, "top: var(--tools-height, 0px);", fixed = TRUE)
  expect_match(html, "main > section[hidden] { display: block; }", fixed = TRUE)

  # Without a description or notes, a line for each
  .dict_write(.dict_build(fake$query, "Analysis", "dbo", "Plain"), "html", file)
  html <- paste(readLines(file, encoding = "UTF-8"), collapse = "\n")
  expect_match(html, "<p>No dataset description available for this collection.</p>",
               fixed = TRUE)
  expect_match(html, "<p>No dataset notes available for this collection.</p>",
               fixed = TRUE)

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
  # The columns' widths are set, so long notes wrap rather than widen their
  # column, and the rules under rows are not drawn in the value labels
  cols <- regmatches(html, gregexpr("<col style=\"width: [0-9]+%\"/>", html))[[1]]
  expect_equal(as.numeric(gsub("\\D", "", cols)), c(4, 15, 24, 9, 28, 20))
  expect_match(html, "table-layout: fixed", fixed = TRUE)
  expect_match(html, "table.columns > tbody > tr > td {", fixed = TRUE)
  expect_no_match(html, "table.columns td", fixed = TRUE)

  # A logo that has been set is written in instead
  dictionary_logo_quiet <- function(...) suppressMessages(dictionary_logo(...))
  dictionary_logo_quiet(fake_png(tempfile(fileext = ".png"), 1400, 430))
  .dict_html(d, file, "Data dictionary", subtitle)
  expect_match(paste(readLines(file), collapse = ""),
               "<img src=\"data:image/png;base64,[^\"]+\" alt=\"Logo\"")

})

test_that("the PDF's template gives the dataset information, then the columns on a new page", {

  typ <- paste(readLines(system.file("dictionary", "dictionary.typ",
                                     package = "actepir")), collapse = "\n")
  expect_match(typ, paste0(
    "(?s)#section\\(\"Dataset information\"\\)\\s*",
    "#subsection\\(\"Dataset description\"\\).*",
    "#subsection\\(\"Dataset notes\"\\).*",
    "if g.theme != none \\{ theme\\(g.theme\\) \\}.*",
    "#pagebreak\\(weak: true\\)\\s*#section\\(\"Columns\"\\)"), perl = TRUE)

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
  expect_equal(normalizePath(.dict_quarto()),
               normalizePath(file.path(rstudio, "quarto.exe")))
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

  messages <- capture_messages(
    out <- episerver_dictionary(c("APC", "Analysis.dbo.Plain"), dir = dir)
  )
  expect_length(grep("^Wrote ", messages), 4)
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
  expect_message(
    lazy <- episerver_dictionary(lazy_table("ED"), formats = "html", dir = dir),
    "ED_dictionary.html", fixed = TRUE
  )
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

test_that("a database's tables and views are listed with whether they have labels", {

  fake <- fake_browse_episerver()
  listed <- .dict_tables(fake$query, "Analysis")
  expect_equal(paste(listed$schema, listed$table, sep = "."),
               c(paste0("dbo.", c("ACTGHSMYX", "APC", "Births", "Deaths", "ED",
                                  "Plain", "vw_ED")),
                 "ref.StandardPops", "stage.APC"))
  expect_equal(listed$table[!listed$labelled], c("Plain", "StandardPops"))
  # One schema; one label table named
  expect_equal(unique(.dict_tables(fake$query, "Analysis", "stage")$schema), "stage")
  named <- .dict_tables(fake$query, "Analysis", "dbo",
                        c(db = "Analysis", schema = "ref", table = "DataLabels"))
  expect_equal(named$table[named$labelled],
               c("ACTGHSMYX", "APC", "Births", "ED", "vw_ED"))
  # A database without label tables
  expect_false(any(.dict_tables(fake$query, "HospitalAPC")$labelled))

})

test_that("episerver_dictionary() writes the dictionaries of a database's tables that have labels", {

  fake <- fake_browse_episerver()
  local_mocked_bindings(episerver_connect = function(...) "named connection")
  local_mocked_bindings(
    dbGetQuery = function(conn, statement, ...) fake$query(statement),
    dbDisconnect = function(conn, ...) invisible(TRUE),
    .package = "DBI"
  )
  withr::local_envvar(R_USER_CONFIG_DIR = withr::local_tempdir())

  # Every schema, each in a folder of its name
  dir <- withr::local_tempdir()
  messages <- capture_messages(
    out <- episerver_dictionary(db = "Analysis", formats = "html", dir = dir)
  )
  expect_setequal(list.files(dir, recursive = TRUE),
                  c(paste0("dbo/", c("ACTGHSMYX", "APC", "Births", "Deaths", "ED",
                                     "vw_ED"), "_dictionary.html"),
                    "stage/APC_dictionary.html"))
  expect_match(messages, paste("7 of the 9 tables and views of Analysis have",
                               "labels; the others were left out."),
               fixed = TRUE, all = FALSE)
  expect_equal(unique(out$table)[c(1, 7)],
               c("Analysis.dbo.ACTGHSMYX", "Analysis.stage.APC"))

  # One schema, in dir itself
  dir <- withr::local_tempdir()
  messages <- capture_messages(episerver_dictionary(db = "Analysis", schema = "stage",
                                                    formats = "html", dir = dir))
  expect_match(messages, "Every table and view of Analysis.stage has labels.",
               fixed = TRUE, all = FALSE)
  expect_equal(list.files(dir), "APC_dictionary.html")

  # No labels: nothing written
  dir <- withr::local_tempdir()
  expect_message(none <- episerver_dictionary(db = "HospitalAPC", dir = dir,
                                              formats = "html"),
                 "No table or view of HospitalAPC has labels", fixed = TRUE)
  expect_length(list.files(dir), 0)
  expect_equal(nrow(none), 0)
  expect_named(none, names(out))

  expect_error(episerver_dictionary(), "Give the tables, or a database in db",
               fixed = TRUE)
  expect_error(episerver_dictionary("APC", db = "Analysis"), "but not both",
               fixed = TRUE)
  expect_error(episerver_dictionary("APC", schema = "stage"), "schema goes with db",
               fixed = TRUE)

})

test_that("dictionary_logo() keeps a logo for the user, and checks it", {

  config <- withr::local_tempdir()
  withr::local_envvar(R_USER_CONFIG_DIR = config)
  png <- function(...) fake_png(tempfile(fileext = ".png"), ...)

  expect_message(lockup <- dictionary_logo(), "show the actepir logo")
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

  expect_message(dictionary_logo(remove = TRUE), "actepir logo")
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

test_that("the dictionaries of many tables are written one by one, and failures kept", {

  withr::local_envvar(R_USER_CONFIG_DIR = withr::local_tempdir())
  fake <- fake_browse_episerver()
  listed <- .dict_tables(fake$query, "Analysis")
  expect_equal(.dict_summary(listed, "Analysis"),
               "7 of the 9 tables and views of Analysis have labels; the others were left out.")
  expect_equal(.dict_summary(listed[listed$schema == "stage", ], "Analysis.stage"),
               "Every table and view of Analysis.stage has labels.")
  expect_equal(.dict_summary(listed[listed$table == "Plain", ], "Analysis.dbo"),
               "No table or view of Analysis.dbo has labels.")
  expect_equal(.dict_summary(listed[0, ], "Analysis.x"),
               "There are no tables or views in Analysis.x that you can read.")
  targets <- .dict_targets(listed, "Analysis")
  expect_length(targets, 7)
  expect_equal(targets[[7]][c("schema", "table")], list(schema = "stage", table = "APC"))

  # A table that fails is kept apart, and the others are written
  dir <- withr::local_tempdir()
  targets <- c(targets[1:2], list(list(db = "Analysis", schema = "dbo",
                                       table = "Nowhere", label = NULL)))
  steps <- integer()
  done <- .dict_write_all(fake$query, targets, "html", dir, by_schema = TRUE,
                          keep_going = TRUE, step = function(i, n) steps <<- c(steps, i))
  expect_length(done$dictionaries, 2)
  expect_equal(names(done$failed), "Analysis.dbo.Nowhere")
  expect_match(done$failed[[1]], "is not a table or view")
  expect_equal(steps, 1:3)
  expect_setequal(list.files(dir, recursive = TRUE),
                  c("dbo/ACTGHSMYX_dictionary.html", "dbo/APC_dictionary.html"))
  expect_error(.dict_write_all(fake$query, targets, "html", dir),
               "is not a table or view")

})

test_that("the dialog for the dictionaries of all tables offers the tables, files and folder", {

  html <- function(x) as.character(x)
  dialog <- html(.browse_bulk_dialog("Analysis", "dbo", TRUE, "C:/work/dictionaries"))
  expect_match(dialog, "modal-dialog-scrollable", fixed = TRUE)
  expect_match(dialog, "Of the schema Analysis.dbo", fixed = TRUE)
  expect_match(dialog, "Of the whole database Analysis, each schema in a folder of its name",
               fixed = TRUE)
  expect_match(dialog, "<input type=\"checkbox\" name=\"bulk_formats\" value=\"pdf\" checked=\"checked\"/>",
               fixed = TRUE)
  expect_match(dialog, "value=\"C:/work/dictionaries\"", fixed = TRUE)
  expect_match(dialog, "id=\"bulk_folders\"", fixed = TRUE)
  expect_match(dialog, "id=\"bulk_write\"", fixed = TRUE)

  # Without Quarto the PDF is disabled
  plain <- html(.browse_bulk_dialog("Analysis", "dbo", FALSE, "x"))
  expect_match(plain, "value=\"pdf\" disabled", fixed = TRUE)
  expect_match(plain, "PDF (needs Quarto, which comes with RStudio)", fixed = TRUE)
  expect_match(plain, "value=\"html\" checked=\"checked\"", fixed = TRUE)

  listed <- .dict_tables(fake_browse_episerver()$query, "Analysis", "dbo")
  done <- list(dictionaries = list(1, 2),
               failed = c(Analysis.dbo.X = "The PDF could not be made."))
  shown <- html(.browse_bulk_done(listed, done, c("html", "pdf"), "C:/w", "Analysis.dbo",
                                  can_open = TRUE))
  expect_match(shown, "Wrote 2 dictionaries, as HTML and PDF, to C:/w.", fixed = TRUE)
  expect_match(shown, "6 of the 7 tables and views of Analysis.dbo have labels", fixed = TRUE)
  expect_match(shown, "<li>Analysis.dbo.X: The PDF could not be made.</li>", fixed = TRUE)
  expect_match(shown, "id=\"bulk_open\"", fixed = TRUE)
  none <- html(.browse_bulk_done(listed[0, ], list(dictionaries = list(), failed = character()),
                                 "html", "C:/w", "Analysis.x", can_open = TRUE))
  expect_match(none, "No dictionary was written.", fixed = TRUE)
  expect_no_match(none, "bulk_open", fixed = TRUE)

  # Explorer is used where R has it
  here <- withr::local_tempdir()
  opened <- NULL
  local_mocked_bindings(.browse_open_fn = function() function(x) opened <<- x)
  .browse_open_dir(here)
  expect_equal(opened, normalizePath(here))

})

test_that("the folder list offers the folders in the folder given, and above it", {

  slash <- function(path) normalizePath(path, winslash = "/")
  root <- slash(withr::local_tempdir())
  for (d in c("b", "A", "c/d", ".hidden", "$sys")) {
    dir.create(file.path(root, d), recursive = TRUE)
  }
  file.create(file.path(root, "file.txt"))
  expect_equal(.browse_subfolders(root), c("A", "b", "c"))
  expect_equal(.browse_subfolders(file.path(root, "none")), character())

  # The folder given, or the nearest folder above it that exists
  expect_equal(.browse_folder_at(root), root)
  expect_equal(.browse_folder_at(file.path(root, "c", "d")), paste0(root, "/c/d"))
  expect_equal(.browse_folder_at(file.path(root, "new", "deeper")), root)
  expect_true(is.na(.browse_folder_at("")))
  expect_true(is.na(.browse_folder_at(NULL)))

  html <- function(x) as.character(x)
  ui <- html(.browse_folder_ui(root))
  at <- function(path) paste0("data-folder=\"", path, "\"")
  pos <- vapply(paste0(root, c("/A", "/b", "/c")),
                function(p) regexpr(at(p), ui, fixed = TRUE), integer(1))
  expect_true(all(pos > 0) && !is.unsorted(pos))
  expect_match(ui, at(slash(dirname(root))), fixed = TRUE)
  expect_match(ui, at(slash("~")), fixed = TRUE)
  expect_match(ui, at(slash(getwd())), fixed = TRUE)
  expect_no_match(ui, "hidden", fixed = TRUE)
  expect_no_match(ui, "epi-note", fixed = TRUE)

  # Notes on the folder given
  expect_match(html(.browse_folder_ui(file.path(root, "new"))),
               paste0("This folder does not exist yet; it will be made. The list ",
                      "shows the folders in ", root, "."), fixed = TRUE)
  expect_match(html(.browse_folder_ui(file.path(root, "c", "d"))),
               "There are no folders in this folder.", fixed = TRUE)
  expect_match(html(.browse_folder_ui("")), "Give a folder", fixed = TRUE)
  # At the top there is no Up
  expect_match(html(.browse_folder_ui("/")), "browse-folder\" disabled>", fixed = TRUE)

})

test_that("the browser writes the dictionaries of all tables with labels to the folder given", {

  withr::local_envvar(R_USER_CONFIG_DIR = withr::local_tempdir())
  local_mocked_bindings(isAvailable = function(...) FALSE,
                        .package = "rstudioapi")
  local_mocked_bindings(.dict_quarto = function() "quarto",
                        .dict_run_quarto = function(quarto, typ, pdf) {
                          writeLines("%PDF-1.7", pdf)
                        })
  filled <- NULL
  local_mocked_bindings(updateTextInput = function(session, inputId, ..., value) {
    filled <<- c(inputId, value)
  }, .package = "shiny")
  opened <- NULL
  local_mocked_bindings(.browse_open_dir = function(folder) opened <<- folder)
  whole <- file.path(withr::local_tempdir(), "new folder")
  one <- withr::local_tempdir()

  app <- episerver_browse_app(query = fake_browse_episerver()$query)
  shiny::testServer(app, {
    session$setInputs(db = "Analysis", schema = "dbo", labels_source = "auto",
                      show_values = TRUE, table = "APC")
    expect_equal(bulk_dir(), file.path(getwd(), "dictionaries"))
    session$setInputs(dict_bulk = 1, bulk_dir = whole)
    session$elapse(500)
    expect_match(output$bulk_folders$html, "This folder does not exist yet", fixed = TRUE)
    # A click in the folder list fills the box
    session$setInputs(bulk_into = dirname(whole))
    expect_equal(filled, c("bulk_dir", dirname(whole)))

    # The whole database, as HTML and PDF, into a folder made for it
    session$setInputs(bulk_scope = "db", bulk_formats = c("html", "pdf"),
                      bulk_write = 1)
    expect_setequal(list.files(whole, recursive = TRUE), c(
      paste0("dbo/", rep(c("ACTGHSMYX", "APC", "Births", "Deaths", "ED", "vw_ED"),
                         each = 2), "_dictionary.", c("html", "pdf")),
      "stage/APC_dictionary.html", "stage/APC_dictionary.pdf"
    ))
    expect_equal(bulk_dir(), whole)
    session$setInputs(bulk_open = 1)
    expect_equal(opened, whole)

    # The schema alone, as HTML, from the label table chosen
    session$setInputs(labels_source = "ref.DataLabels", bulk_scope = "schema",
                      bulk_formats = "html", bulk_dir = one, bulk_write = 2)
    expect_setequal(list.files(one), paste0(c("ACTGHSMYX", "APC", "Births", "ED",
                                              "vw_ED"), "_dictionary.html"))
    html <- paste(readLines(file.path(one, "APC_dictionary.html")), collapse = "\n")
    expect_match(html, "Labels from ref.DataLabels.", fixed = TRUE)

    # No files chosen: nothing is written
    empty <- file.path(one, "empty")
    session$setInputs(bulk_formats = character(), bulk_dir = empty, bulk_write = 3)
    expect_false(dir.exists(empty))
  })

})

test_that("the browser copies the dictionary for Excel, and sets its logo through dictionary_logo()", {

  withr::local_envvar(R_USER_CONFIG_DIR = withr::local_tempdir())
  local_mocked_bindings(isAvailable = function(...) FALSE,
                        .package = "rstudioapi")
  copied <- NULL
  local_mocked_bindings(.epi_copy_text = function(session, text, message,
                                                  html = NULL) {
    copied <<- list(text = text, html = html, message = message)
  })
  upload <- function(path, name) {
    data.frame(name = name, size = file.size(path), type = "image/png",
               datapath = path, stringsAsFactors = FALSE)
  }
  dir <- withr::local_tempdir()

  app <- episerver_browse_app(query = fake_browse_episerver()$query)
  shiny::testServer(app, {
    session$setInputs(db = "Analysis", schema = "dbo", labels_source = "auto",
                      show_values = FALSE, table = "APC")

    # The dictionary as a table, with the value labels that are not shown
    session$setInputs(dict_excel = 1)
    expect_equal(copied$message, "Dictionary copied to the clipboard")
    expect_equal(copied, c(.dict_excel(.dict_flat(dictionary())),
                           list(message = copied$message)))
    expect_match(copied$text, "\tSex\tSex of the patient\tint\t9\tNot stated\t")

    # The logo: the actepir logo, then a PNG chosen, with what it warns of,
    # a file that is not a PNG, and the actepir logo again
    session$setInputs(dict_logo = 1)
    expect_match(output$logo_shown$html, "data:image/svg+xml;base64,", fixed = TRUE)
    expect_match(output$logo_shown$html, "The dictionaries show the actepir logo.",
                 fixed = TRUE)
    session$setInputs(logo_file = upload(fake_png(file.path(dir, "0.png"), 1400, 430),
                                         "white.png"))
    expect_true(file.exists(.dict_logo_file()))
    expect_match(output$logo_shown$html, "data:image/png;base64,", fixed = TRUE)
    expect_match(output$logo_shown$html,
                 "The dictionaries show your logo, 1400 by 430 pixels.", fixed = TRUE)
    expect_no_match(output$logo_shown$html, "epi-warn", fixed = TRUE)
    session$setInputs(logo_file = upload(fake_png(file.path(dir, "1.png"), 300, 90,
                                                  colour = 2L), "small.png"))
    expect_match(output$logo_shown$html, "The logo is 300 pixels wide", fixed = TRUE)
    expect_match(output$logo_shown$html, "no transparent background", fixed = TRUE)
    text <- file.path(dir, "2.png")
    writeLines("not a PNG", text)
    session$setInputs(logo_file = upload(text, "notes.png"))
    expect_match(output$logo_shown$html, "notes.png is not a PNG file.", fixed = TRUE)
    expect_no_match(output$logo_shown$html, dir, fixed = TRUE)
    expect_match(output$logo_shown$html, "300 by 90 pixels", fixed = TRUE)
    session$setInputs(logo_reset = 1)
    expect_false(file.exists(.dict_logo_file()))
    expect_match(output$logo_shown$html, "The dictionaries show the actepir logo.",
                 fixed = TRUE)
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
