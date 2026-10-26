# tests/testthat/test-episerver_browse.R
#
# Offline tests for the Dataset Browser: its SQL, lists, columns' table and
# code, the page, and the app's server logic. The app runs against
# fake_browse_episerver() (helper-browse.R), which answers the browser's SQL
# from synthetic label tables, so nothing here needs EpiServer. The live
# queries are tested in test-episerver_browse_live.R.

library(testthat)
library(actepir)

strip_html <- function(x) {
  trimws(gsub("\\s+", " ", gsub("<[^>]+>", " ", as.vector(unclass(x)))))
}

# ── Functions ───────────────────────────────────────────────────────────────

test_that("episerver_browse() passes connection arguments to the app", {

  args <- NULL
  local_mocked_bindings(.run_app = function(...) args <<- list(...))
  episerver_browse(driver = "ODBC Driver 18 for SQL Server", encrypt = "No")
  expect_equal(args$key, "browse")
  expect_equal(args$title, "Dataset Browser")
  expect_equal(args$factory, "episerver_browse_app")
  expect_equal(args$factory_args,
               list(driver = "ODBC Driver 18 for SQL Server",
                    max_attempts = NULL, encrypt = "No"))
  expect_true(args$background)

  stopped <- NULL
  local_mocked_bindings(.stop_app = function(...) stopped <<- list(...))
  episerver_browse_stop()
  expect_equal(stopped, list("browse", "Dataset Browser"))

})

# ── SQL ─────────────────────────────────────────────────────────────────────

test_that("the catalogue queries quote names and leave out label tables", {

  expect_match(.browse_sql_databases(), "FROM sys.databases WHERE state_desc = 'ONLINE'",
               fixed = TRUE)
  expect_match(.browse_sql_schemas("My]DB"),
               "FROM [My]]DB].INFORMATION_SCHEMA.TABLES", fixed = TRUE)

  tables <- .browse_sql_tables("Analysis", "O'Brien")
  expect_match(tables, "SELECT TABLE_NAME, TABLE_TYPE", fixed = TRUE)
  expect_match(tables, "WHERE TABLE_SCHEMA = N'O''Brien'", fixed = TRUE)
  # Tables and views, but not the label tables
  expect_match(tables, "TABLE_TYPE IN ('BASE TABLE', 'VIEW')", fixed = TRUE)
  expect_match(tables, "TABLE_NAME NOT LIKE '%DataLabels%'", fixed = TRUE)
  expect_match(tables, "TABLE_NAME NOT IN ('DataProperties', 'DataPropertyTables')",
               fixed = TRUE)

  columns <- .browse_sql_columns("Analysis", "dbo", "APC")
  expect_match(columns, "FROM [Analysis].INFORMATION_SCHEMA.COLUMNS", fixed = TRUE)
  expect_match(columns, "TABLE_SCHEMA = N'dbo'", fixed = TRUE)
  expect_match(columns, "TABLE_NAME   = N'APC'", fixed = TRUE)
  expect_match(columns, "ORDER BY ORDINAL_POSITION", fixed = TRUE)

})

# ── Lists, columns and code ─────────────────────────────────────────────────

test_that("system databases and those starting with an underscore are hidden", {

  expect_equal(.browse_databases(c("_Archive", "Analysis", "HospitalAPC",
                                   "master", "MODEL", "msdb", "tempdb")),
               c("Analysis", "HospitalAPC"))
  expect_equal(.browse_databases(NULL), "Analysis")
  expect_equal(.browse_databases("tempdb"), "Analysis")

})

test_that("the Table list groups tables and views, with their dataset labels", {

  tables <- data.frame(TABLE_NAME = c("APC", "ED", "Plain", "vw_ED"),
                       TABLE_TYPE = c("BASE TABLE", "BASE TABLE", "BASE TABLE",
                                      "VIEW"))
  choices <- .browse_table_choices(tables, c(apc = "Admitted patient care",
                                             vw_ed = "ED view"))
  expect_equal(choices, list(
    Tables = c("APC\tAdmitted patient care" = "APC", ED = "ED",
               Plain = "Plain"),
    Views  = c("vw_ED\tED view" = "vw_ED")
  ))
  # Without views there is no Views group; without tables, no choices
  expect_named(.browse_table_choices(tables[1:2, ], character()), "Tables")
  expect_equal(.browse_table_choices(tables[0, ], character()), list())
  expect_equal(.browse_table_choices(NULL, character()), list())

})

test_that("types show lengths, and precision and scale when there is a scale", {

  expect_equal(
    .browse_type(c("int", "varchar", "nvarchar", "decimal", "decimal", "float",
                   "datetime"),
                 c(NA, 10, -1, NA, NA, NA, NA),
                 c(10, NA, NA, 7, 10, 53, NA),
                 c(0, NA, NA, 1, 0, NA, NA)),
    c("int", "varchar(10)", "nvarchar(max)", "decimal(7,1)", "decimal",
      "float", "datetime")
  )

})

test_that("the columns' table escapes text and adds value labels and notes", {

  cols <- data.frame(ord = 1:3, col = c("Sex", "Age<", "id"),
                     dtype = c("int", "int", "varchar"),
                     charlen = c(NA, NA, 5), numprec = c(10, 10, NA),
                     numscale = c(0, 0, NA))
  found <- .lbl_normalise(data.frame(
    ColumnName = c("SEX", "Age<", "Age<", "sex", "sex"),
    PropertyType = c("VariableLabel", "VariableLabel", "VariableNote",
                     "ValueLabel", "ValueLabel"),
    PropertyCode = c(NA, NA, NA, "2", "1"),
    PropertyText = c("Sex of the patient", "Age <in years>", "A & B",
                     "Female", "<Male>")
  ))
  lbl <- .lbl_table(found)

  d <- .browse_columns(cols, lbl)
  expect_named(d, c("#", "Column", "Description", "Type", "Notes"))
  expect_equal(d$Column, c("Sex", "Age&lt;", "id"))
  # Labels match columns in any case, as SQL Server matches names
  expect_equal(d$Description, c("Sex of the patient", "Age &lt;in years&gt;", ""))
  expect_equal(d$Type, c("int", "int", "varchar(5)"))
  expect_equal(d$Notes, c("", "A &amp; B", ""))

  v <- .browse_columns(cols, lbl, values = TRUE)
  expect_named(v, c("#", "Column", "Description", "Type", "Value labels",
                    "Notes"))
  expect_equal(v$`Value labels`[1], paste0(
    "<details><summary>2 labels</summary><div>1 = &lt;Male&gt;<br>",
    "2 = Female</div></details>"))
  expect_equal(v$`Value labels`[2:3], c("", ""))

  # No notes, no Notes column
  expect_named(.browse_columns(cols, .lbl_table(found[1:2, ])),
               c("#", "Column", "Description", "Type"))
  expect_equal(nrow(.browse_columns(cols[0, ], lbl)), 0)

})

test_that("the dataset label, notes and linked tables go above the columns", {

  lbl <- .lbl_table(.lbl_normalise(data.frame(
    ColumnName = c("", "", "id"),
    PropertyType = c("DatasetLabel", "DatasetNote", "Link"),
    PropertyCode = NA,
    PropertyText = c("Admitted patient care", "Extracted monthly.",
                     "APC_Extra")
  )))
  html <- as.character(.browse_about(lbl$dataset))
  expect_match(html, "<h5>Admitted patient care</h5>", fixed = TRUE)
  expect_match(html, "<p>Extracted monthly.</p>", fixed = TRUE)
  expect_match(html, "<p>Linked tables: APC_Extra (on id).</p>", fixed = TRUE)
  expect_null(.browse_about(.lbl_table(.lbl_normalise(NULL))$dataset))

})

test_that("the info bar says which label table the labels came from", {

  found <- data.frame(TABLE_SCHEMA = c("ref", "ref", "dbo"),
                      TABLE_NAME = c("DataProperties", "DataLabels",
                                     "DataLabels"),
                      Layout = c("properties", "labels", "labels"))
  note <- function(i, sources = found, auto = TRUE) {
    .browse_labels_note(if (i) sources[i, ], sources, auto)
  }
  expect_equal(note(1), "Labels from ref.DataProperties.")
  expect_equal(note(2), paste("Labels from ref.DataLabels, as",
                              "ref.DataProperties holds none for this table."))
  expect_equal(note(3), paste("Labels from dbo.DataLabels, as",
                              "ref.DataProperties and ref.DataLabels hold",
                              "none for this table."))
  expect_equal(note(0), paste("No label table holds labels for this table",
                              "(looked in ref.DataProperties, ref.DataLabels",
                              "and dbo.DataLabels)."))
  # Before ref.DataProperties exists
  expect_equal(note(1, found[2:3, ]), "Labels from ref.DataLabels.")
  # One label table chosen
  expect_equal(note(1, found[3, ], auto = FALSE), "Labels from dbo.DataLabels.")
  expect_equal(note(0, found[3, ], auto = FALSE),
               "dbo.DataLabels holds no labels for this table.")
  expect_equal(note(0, found[0, ]),
               "This database has no label table, so no labels are shown.")

})

test_that("the code reads the table as before, with the columns chosen", {

  expect_equal(
    .browse_code("Analysis", "dbo", "APC", c("Sex", "AgeYrs")),
    paste0("data <- actepir::episerver_quickconnect(\"APC\") |>\n",
           "  dplyr::select(Sex, AgeYrs) |>\n",
           "  actepir::collect_withlabels()\n")
  )
  expect_equal(
    .browse_code("Analysis", "ref", "StandardPops", mode = "connect"),
    paste0("library(actepir)\nlibrary(dplyr)\n\n",
           "conn <- episerver_connect()\n",
           "tb <- episerver_lazytable(conn, \"StandardPops\", schema = \"ref\")\n\n",
           "data <- tb |>\n",
           "  collect_withlabels()\n")
  )
  expect_equal(
    .browse_code("HospitalAPC", "dbo", "APC", "Sex", mode = "connect",
                 declare = FALSE, labels = FALSE),
    paste0("conn <- episerver_connect()\n",
           "tb <- episerver_lazytable(conn, \"APC\", schema = \"dbo\", ",
           "db = \"HospitalAPC\")\n\n",
           "data <- tb |>\n",
           "  select(Sex) |>\n",
           "  collect()\n")
  )
  expect_equal(
    .browse_code("Analysis", "dbo", "APC", "Sex", mode = "none",
                 declare = FALSE),
    "data <- tb |>\n  select(Sex) |>\n  collect_withlabels()\n"
  )

})

# ── Page ────────────────────────────────────────────────────────────────────

test_that("the page follows the app style: top bar, sidebar sections and tabs", {

  withr::local_options(sass.cache = FALSE)
  app  <- episerver_browse_app(query = fake_browse_episerver()$query)
  page <- app$httpHandler(list(PATH_INFO = "/", REQUEST_METHOD = "GET",
                               QUERY_STRING = ""))$content
  part <- function(pattern) {
    regmatches(page, regexpr(pattern, page, perl = TRUE))
  }
  has_id <- function(html, id) grepl(paste0("id=\"", id, "\""), html, fixed = TRUE)

  expect_match(page, "<title>Dataset Browser</title>", fixed = TRUE)

  # The name at the left, the tabs, and the logo at the right; no Done
  navbar <- part("(?s)<nav .*?</nav>")
  expect_length(navbar, 1)
  expect_match(navbar, "(?s)class=\"navbar-brand\"[^>]*>\\s*Dataset Browser\\s*<",
               perl = TRUE)
  in_bar <- function(x) regexpr(x, navbar, fixed = TRUE)
  for (tab in c("columns", "help")) {
    at <- in_bar(paste0("data-value=\"", tab, "\""))
    expect_gt(at, in_bar("Dataset Browser"))
    expect_lt(at, in_bar("class=\"epi-logo\""))
  }
  expect_false(has_id(page, "done"))
  expect_no_match(page, "gadget", fixed = TRUE)

  # The sidebar: three sections that start open, then the code and bug
  # buttons at its foot. It can be resized.
  sidebar <- part("(?s)<aside id=\"settings\" class=\"sidebar.*?</aside>")
  expect_length(sidebar, 1)
  expect_match(part("<aside id=\"settings\"[^>]*>"), "data-resizable")
  expect_match(page, "--_sidebar-width:340px", fixed = TRUE)
  headings <- regmatches(sidebar, gregexpr(
    "(?s)<button class=\"accordion-button[^>]*>.*?</button>", sidebar,
    perl = TRUE))[[1]]
  expect_equal(strip_html(headings), c("Dataset", "Labels", "Code"))
  expect_no_match(paste(headings, collapse = ""), "collapsed")
  for (id in c("db", "schema", "table", "labels_source", "show_values",
               "insert_mode", "declare_pkgs", "use_labels", "insert_code",
               "bug_report")) {
    expect_true(has_id(sidebar, id), label = id)
  }
  foot <- part("(?s)<div class=\"epi-actions\">.*?</div>")
  expect_match(foot, "id=\"insert_code\"[^>]*>(?s).*Copy Code", perl = TRUE)
  expect_match(foot, "id=\"bug_report\"", fixed = TRUE)
  # Analysis is chosen; system databases are not offered
  expect_match(page, "<option value=\"Analysis\" selected>Analysis</option>",
               fixed = TRUE)
  expect_no_match(page, "<option value=\"master\"", fixed = TRUE)
  # Each table's dataset label is shown muted, by the list's render function
  expect_match(page, "browse-label", fixed = TRUE)

  # The info bar is outside the sidebar; the Help tab uses the shared style
  expect_false(has_id(sidebar, "info_bar"))
  expect_true(has_id(page, "info_bar"))
  expect_match(page, "<div class=\"help\">", fixed = TRUE)
  expect_match(page, ".help h5 {", fixed = TRUE)
  # Once stopped, the page says how to open it again
  expect_match(page, "The Dataset Browser is no longer running.", fixed = TRUE)
  expect_match(page, "Addins &gt; Open Dataset Browser", fixed = TRUE)
  expect_match(page, "actepir::episerver_browse()", fixed = TRUE)
  # The columns' table fits the window
  expect_match(page, "--browse-table-height", fixed = TRUE)

})

# ── App ─────────────────────────────────────────────────────────────────────

test_that("the Table list holds each schema's tables with their dataset labels", {

  sent <- list()
  capture <- function(session, inputId, label = NULL, choices = NULL,
                      selected = NULL, ...) {
    sent[[inputId]] <<- list(choices = choices, selected = selected)
  }
  local_mocked_bindings(updateSelectInput = capture,
                        updateSelectizeInput = capture, .package = "shiny")
  fake <- fake_browse_episerver()
  app <- episerver_browse_app(query = fake$query)

  shiny::testServer(app, {
    session$setInputs(db = "Analysis")
    expect_equal(sent$schema, list(choices = c("dbo", "ref", "stage"),
                                   selected = "dbo"))
    expect_equal(sent$labels_source$choices, c(
      Automatic = "auto", "ref.DataProperties" = "ref.DataProperties",
      "ref.DataLabels" = "ref.DataLabels", "dbo.DataLabels" = "dbo.DataLabels"
    ))
    expect_equal(sent$labels_source$selected, "auto")

    # Label tables are not listed. Each table's dataset label comes from
    # the first label table that holds rows for it: Births has rows in
    # ref.DataProperties but no dataset label there.
    session$setInputs(schema = "dbo", labels_source = "auto")
    expect_equal(sent$table$choices, list(
      Tables = c("ACTGHSMYX\tGeneral health survey" = "ACTGHSMYX",
                 "APC\tAdmitted patient care" = "APC",
                 Births = "Births",
                 "Deaths\tRegistered deaths" = "Deaths",
                 "ED\tEmergency presentations" = "ED",
                 Plain = "Plain"),
      Views = c(vw_ED = "vw_ED")
    ))
    expect_equal(sent$table$selected, "ACTGHSMYX")

    # The table chosen stays chosen while it is in the list
    session$setInputs(table = "APC", schema = "stage")
    expect_equal(sent$table$choices,
                 list(Tables = c("APC\tAdmitted patient care, staged" = "APC")))
    expect_equal(sent$table$selected, "APC")
    session$setInputs(table = "ED", schema = "ref")
    expect_equal(sent$table$choices, list(Tables = c(StandardPops = "StandardPops")))
    expect_equal(sent$table$selected, "StandardPops")

    # A label table chosen gives the labels of the list
    session$setInputs(schema = "dbo", labels_source = "ref.DataLabels")
    expect_equal(sent$table$choices$Tables[1:3],
                 c(ACTGHSMYX = "ACTGHSMYX", "APC\tOld APC label" = "APC",
                   "Births\tBirths, old label" = "Births"))

    # A database without label tables: Automatic only, and no labels read
    n <- length(fake$calls())
    session$setInputs(db = "HospitalAPC", schema = "dbo", table = "APC")
    expect_equal(sent$labels_source$choices, c(Automatic = "auto"))
    expect_equal(sent$table$choices, list(Tables = c(APC = "APC")))
    expect_false(any(grepl("AS Source", fake$calls()[-seq_len(n)], fixed = TRUE)))
  })

})

test_that("each table takes its labels from the first label table holding rows", {

  fake <- fake_browse_episerver()
  app <- episerver_browse_app(query = fake$query)

  shiny::testServer(app, {
    session$setInputs(db = "Analysis", schema = "dbo", labels_source = "auto",
                      show_values = FALSE, table = "APC")
    info <- function() strip_html(output$info_bar$html)

    expect_equal(info(), paste("Analysis.dbo.APC (table), 43 columns",
                               "Labels from ref.DataProperties."))
    d <- .browse_columns(columns()$data, lbl())
    expect_equal(d$Description[1:4], c("", "Sex of the patient",
                                       "Age in years (&lt;1 is 0)", ""))
    expect_equal(d$Notes[3], "Second note.<br>Tenth note.<br>Uncoded note.")
    expect_false("Value labels" %in% names(d))
    about <- strip_html(output$about$html)
    expect_equal(about, paste("Admitted patient care Coded first. Coded",
                              "second. Not coded. Linked tables: APC_Extra",
                              "(on PatientID)."))

    session$setInputs(table = "ED")
    expect_equal(info(), paste("Analysis.dbo.ED (table), 2 columns",
                               "Labels from ref.DataLabels, as",
                               "ref.DataProperties holds none for this table."))
    expect_equal(lbl()$notes$triage, c("Noted first.", "Noted second."))
    expect_match(strip_html(output$about$html), "^Emergency presentations ")

    session$setInputs(table = "Deaths")
    expect_match(info(), paste("Labels from dbo.DataLabels, as",
                               "ref.DataProperties and ref.DataLabels hold",
                               "none for this table."), fixed = TRUE)
    expect_equal(unname(lbl()$label["cause"]), "Underlying cause of death")

    session$setInputs(table = "vw_ED")
    expect_match(info(), "^Analysis.dbo.vw_ED \\(view\\), 1 column Labels from ref.DataLabels")

    session$setInputs(table = "Plain")
    expect_match(info(), "No label table holds labels for this table", fixed = TRUE)
    expect_null(output$about$html)

    # Births takes its labels from ref.DataProperties, without a dataset
    # label, although ref.DataLabels holds one
    session$setInputs(table = "Births")
    expect_equal(unname(lbl()$label["weight"]), "Birth weight in grams")
    expect_true(is.na(lbl()$dataset$label))

    # Tables of the same name in other schemas have labels of their own
    session$setInputs(schema = "stage", table = "APC")
    expect_equal(unname(lbl()$label["sex"]), "Sex as staged")

    # A label table chosen is read alone
    session$setInputs(schema = "dbo", labels_source = "ref.DataLabels")
    expect_equal(info(), paste("Analysis.dbo.APC (table), 43 columns",
                               "Labels from ref.DataLabels."))
    expect_equal(unname(lbl()$label["sex"]), "Old sex label")
    session$setInputs(labels_source = "dbo.DataLabels")
    expect_match(info(), "dbo.DataLabels holds no labels for this table.",
                 fixed = TRUE)
  })

})

test_that("every table falls back to DataLabels before ref.DataProperties exists", {

  app <- episerver_browse_app(query = fake_browse_episerver(properties = FALSE)$query)

  shiny::testServer(app, {
    session$setInputs(db = "Analysis", schema = "dbo", labels_source = "auto",
                      table = "APC")
    expect_equal(strip_html(output$info_bar$html),
                 paste("Analysis.dbo.APC (table), 43 columns",
                       "Labels from ref.DataLabels."))
    expect_equal(unname(lbl()$label["sex"]), "Old sex label")
    session$setInputs(table = "Deaths")
    expect_match(strip_html(output$info_bar$html),
                 "Labels from dbo.DataLabels, as ref.DataLabels holds none for this table.",
                 fixed = TRUE)
  })

})

test_that("value labels are read only while shown", {

  fake <- fake_browse_episerver()
  app <- episerver_browse_app(query = fake$query)
  reads <- function() {
    sum(grepl("(?<!NOT )IN \\((N'ValueLabel'|'Opt', 'ValueLabel')\\)",
              fake$calls(), perl = TRUE))
  }

  shiny::testServer(app, {
    session$setInputs(db = "Analysis", schema = "dbo", labels_source = "auto",
                      show_values = FALSE, table = "APC")
    d <- .browse_columns(columns()$data, lbl(), values = FALSE)
    expect_equal(reads(), 0)

    session$setInputs(show_values = TRUE)
    expect_equal(lbl()$values$sex$Code, c("1", "2", "9", "10"))
    expect_equal(lbl()$values$sex$Text[1], "Male")
    expect_equal(reads(), 1)

    session$setInputs(table = "ED")
    expect_equal(lbl()$values$triage$Text, c("Resuscitation", "Emergency"))
    expect_equal(reads(), 2)
    # Plain has no label table, so nothing is read for it
    session$setInputs(table = "Plain")
    expect_null(values())
    expect_equal(reads(), 2)
  })

})

test_that("read errors are reported in the info bar", {

  run <- function(fail, table = "APC", show_values = FALSE) {
    app <- episerver_browse_app(query = fake_browse_episerver(fail = fail)$query)
    html <- NULL
    shiny::testServer(app, {
      session$setInputs(db = "Analysis", schema = "dbo",
                        labels_source = "auto", show_values = show_values,
                        table = table)
      html <<- output$info_bar$html
    })
    html
  }

  # The columns cannot be read: an error
  html <- run("AS ord")
  expect_match(html, "info-bar epi-error", fixed = TRUE)
  expect_match(strip_html(html), paste("The columns of Analysis.dbo.APC",
                                       "could not be read: Fake failure"),
               fixed = TRUE)
  # The labels cannot be read: warnings, and the columns are still shown
  html <- run("WHEN EXISTS")
  expect_match(html, "class=\"epi-warn\">The label tables could not be read: Fake failure",
               fixed = TRUE)
  expect_no_match(html, "Labels from", fixed = TRUE)
  html <- run("NOT IN \\(N'ValueLabel'\\)")
  expect_match(strip_html(html), paste("Labels from ref.DataProperties. The",
                                       "labels could not be read from",
                                       "ref.DataProperties: Fake failure"),
               fixed = TRUE)
  html <- run("(?<!NOT )IN \\(N'ValueLabel'\\)", show_values = TRUE)
  expect_match(html, "The value labels could not be read from ref.DataProperties",
               fixed = TRUE)
  html <- run("AS Layout")
  expect_match(html, "The label tables of Analysis could not be listed",
               fixed = TRUE)
  expect_no_match(html, "no label table", fixed = TRUE)
  html <- run("AS Source, ")
  expect_match(html, "The dataset labels for the Table list could not be read",
               fixed = TRUE)
  html <- run("SELECT TABLE_NAME, TABLE_TYPE")
  expect_match(strip_html(html), paste("The tables and views of Analysis.dbo",
                                       "could not be read"), fixed = TRUE)

})

test_that("the code button gives the code for the table and columns chosen", {

  sent <- character()
  local_mocked_bindings(.epi_send_code = function(session, code) {
    sent <<- c(sent, code)
  })
  app <- episerver_browse_app(query = fake_browse_episerver()$query)

  shiny::testServer(app, {
    # Nothing is chosen yet
    session$setInputs(insert_code = 1)
    expect_length(sent, 0)

    session$setInputs(db = "Analysis", schema = "dbo", labels_source = "auto",
                      table = "APC", insert_mode = "connect",
                      declare_pkgs = TRUE, use_labels = FALSE,
                      col_table_rows_selected = c(3, 2, 99))
    session$setInputs(insert_code = 2)
    session$setInputs(schema = "stage", insert_mode = "quickconnect",
                      use_labels = TRUE, col_table_rows_selected = NULL)
    session$setInputs(insert_code = 3)
  })

  expect_equal(sent, c(
    .browse_code("Analysis", "dbo", "APC", c("AgeYrs", "Sex"), "connect",
                 declare = TRUE, labels = FALSE),
    .browse_code("Analysis", "stage", "APC", character(), "quickconnect")
  ))

})

test_that("the bug button opens a report that holds the settings but no labels", {

  copied <- NULL
  local_mocked_bindings(
    .epi_copy_text = function(session, text, message) copied <<- text
  )
  app <- episerver_browse_app(query = fake_browse_episerver()$query)
  client <- list(agent = "Mozilla/5.0 (Windows NT 10.0) Edg/130.0",
                 width = 1920, height = 961, ratio = 1.25,
                 errors = list(), script_errors = list(), at = 1)

  shiny::testServer(app, {
    session$setInputs(db = "Analysis", schema = "dbo", labels_source = "auto",
                      show_values = TRUE, table = "APC",
                      insert_mode = "quickconnect", declare_pkgs = TRUE,
                      use_labels = TRUE, col_table_rows_selected = 2)
    session$setInputs(bug_client = client)

    details <- output$bug_details
    expect_match(details, "^Dataset Browser problem report")
    expect_match(details, "Databases listed: 2", fixed = TRUE)
    expect_match(details, paste("Label tables: ref.DataProperties,",
                                "ref.DataLabels, dbo.DataLabels"), fixed = TRUE)
    expect_match(details, "Labels: Labels from ref.DataProperties.", fixed = TRUE)
    expect_match(details, "Errors: none", fixed = TRUE)
    expect_match(details, "Code button: Copy Code", fixed = TRUE)
    expect_match(details, "table = \"APC\"", fixed = TRUE)
    expect_match(details, "col_table_rows_selected = 2", fixed = TRUE)
    expect_match(details, "actepir::episerver_quickconnect(\"APC\")", fixed = TRUE)
    expect_match(details, "dplyr::select(Sex)", fixed = TRUE)
    expect_match(details, "Window: 1920 x 961", fixed = TRUE)
    # No labels
    for (text in c("Admitted patient care", "Sex of the patient", "Female",
                   "Coded first")) {
      expect_no_match(details, text, fixed = TRUE)
    }

    session$setInputs(bug_what = "The labels looked old.", bug_copy = 1)
    expect_match(copied, "What happened\n-------------\nThe labels looked old.",
                 fixed = TRUE)
  })

})
