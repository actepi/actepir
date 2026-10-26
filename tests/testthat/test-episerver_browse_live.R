# tests/testthat/test-episerver_browse_live.R
#
# Live tests for the Dataset Browser and the labels lookup. Each needs
# EpiServer and is skipped when it cannot be reached. They run every query
# the browser builds against Analysis and check the lookup on its label
# tables. They hold whether or not ref.DataProperties exists yet.

library(testthat)
library(actepir)

# One connection per test, closed when the test ends
live_db <- function(env = parent.frame()) {
  db <- .epi_connection()
  withr::defer(db$disconnect(), envir = env)
  db
}

# The tables of Analysis.dbo whose labels are checked: ACTGHSMYX, whose
# labels test-labels.R also reads, and the first 30 others
live_sample <- function(tables) {
  others <- setdiff(tables$TABLE_NAME, "ACTGHSMYX")
  c("ACTGHSMYX", utils::head(others, 30))
}

test_that("the catalogue queries list databases, tables, views and columns", {

  skip_if_no_episerver()
  db <- live_db()

  dbs <- db$query(.browse_sql_databases())
  offered <- .browse_databases(dbs$name)
  expect_true("Analysis" %in% offered)
  expect_false(any(c("master", "model", "msdb", "tempdb") %in% offered))
  expect_false(any(startsWith(offered, "_")))

  schemas <- db$query(.browse_sql_schemas("Analysis"))
  expect_true(all(c("dbo", "ref") %in% schemas$TABLE_SCHEMA))

  tables <- db$query(.browse_sql_tables("Analysis", "dbo"))
  expect_named(tables, c("TABLE_NAME", "TABLE_TYPE"))
  expect_true(all(tables$TABLE_TYPE %in% c("BASE TABLE", "VIEW")))
  expect_true("ACTGHSMYX" %in% tables$TABLE_NAME)
  # Label tables are not listed, in dbo or ref
  ref <- db$query(.browse_sql_tables("Analysis", "ref"))
  listed <- c(tables$TABLE_NAME, ref$TABLE_NAME)
  expect_false(any(grepl("DataLabels", listed, ignore.case = TRUE)))
  expect_false(any(listed %in% c("DataProperties", "DataPropertyTables")))

  cols <- db$query(.browse_sql_columns("Analysis", "dbo", "ACTGHSMYX"))
  expect_named(cols, c("ord", "col", "dtype", "charlen", "numprec", "numscale"))
  expect_true("RSEX_Y20" %in% cols$col)
  expect_false(is.unsorted(cols$ord))
  expect_false(anyNA(.browse_type(cols$dtype, cols$charlen, cols$numprec,
                                  cols$numscale)))

  # A view's columns are read the same way, when Analysis has a view
  views <- rbind(tables, ref)
  views <- views$TABLE_NAME[views$TABLE_TYPE == "VIEW"]
  if (length(views)) {
    schema <- if (views[1] %in% tables$TABLE_NAME) "dbo" else "ref"
    v <- db$query(.browse_sql_columns("Analysis", schema, views[1]))
    expect_gt(nrow(v), 0)
  }

})

test_that("the label tables of Analysis are found in order, each in its layout", {

  skip_if_no_episerver()
  db <- live_db()

  s <- db$query(.lbl_sql_sources("Analysis"))
  expect_named(s, c("TABLE_SCHEMA", "TABLE_NAME", "Layout"))
  name <- paste0(s$TABLE_SCHEMA, ".", s$TABLE_NAME)
  expect_true("ref.DataLabels" %in% name)

  # ref.DataProperties, when it exists, comes first and alone in its layout
  expect_equal(s$Layout == "properties", name == "ref.DataProperties")
  if ("ref.DataProperties" %in% name) {
    expect_equal(name[1], "ref.DataProperties")
  }
  # Then DataLabels, DataLabelsNew and other names, ref first in each
  rank <- match(s$TABLE_NAME, c("DataProperties", "DataLabels",
                                "DataLabelsNew"), nomatch = 4L)
  expect_false(is.unsorted(rank))
  for (r in unique(rank)) {
    schemas <- s$TABLE_SCHEMA[rank == r]
    expect_false(is.unsorted(schemas != "ref"), label = paste("rank", r))
  }

  # Each has every column of its layout
  for (i in seq_len(nrow(s))) {
    cols <- db$query(.browse_sql_columns("Analysis", s$TABLE_SCHEMA[i],
                                         s$TABLE_NAME[i]))
    expect_true(all(tolower(.lbl_layouts[[s$Layout[i]]]) %in% tolower(cols$col)),
                label = name[i])
  }

})

test_that("each table takes its labels from the first label table holding rows", {

  skip_if_no_episerver()
  db <- live_db()

  s <- db$query(.lbl_sql_sources("Analysis"))
  tables <- db$query(.browse_sql_tables("Analysis", "dbo"))
  picked <- integer()
  for (t in live_sample(tables)) {
    # Whether each label table holds rows for the table, asked one by one
    held <- vapply(seq_len(nrow(s)), function(i) {
      db$query(.lbl_sql_pick("Analysis", s[i, ], "dbo", t))$Source == 1L
    }, logical(1))
    pick <- db$query(.lbl_sql_pick("Analysis", s, "dbo", t))$Source
    expect_equal(pick, if (any(held)) which(held)[1] else 0L, label = t)
    picked[t] <- pick
  }
  expect_gt(picked[["ACTGHSMYX"]], 0)

})

test_that("a table's rows normalise to the labels shown", {

  skip_if_no_episerver()
  db <- live_db()

  s <- db$query(.lbl_sql_sources("Analysis"))
  i <- db$query(.lbl_sql_pick("Analysis", s, "dbo", "ACTGHSMYX"))$Source
  expect_gt(i, 0)

  rows   <- db$query(.lbl_sql_rows("Analysis", s[i, ], "dbo", "ACTGHSMYX"))
  values <- db$query(.lbl_sql_rows("Analysis", s[i, ], "dbo", "ACTGHSMYX",
                                   values = TRUE))
  expect_named(rows, c("ColumnName", "PropertyType", "PropertyCode",
                       "PropertyText"))
  expect_named(values, names(rows))
  n <- .lbl_normalise(rows)
  v <- .lbl_normalise(values)
  expect_false("ValueLabel" %in% n$Type)
  expect_true(all(v$Type == "ValueLabel"))
  expect_true(all(c("Male", "Female") %in% .lbl_table(n, v)$values$rsex_y20$Text))

})

test_that("the Table list's dataset labels follow the label table of each table", {

  skip_if_no_episerver()
  db <- live_db()

  s <- db$query(.lbl_sql_sources("Analysis"))
  tables <- db$query(.browse_sql_tables("Analysis", "dbo"))
  found <- db$query(.lbl_sql_dataset_labels("Analysis", s, "dbo"))
  expect_named(found, c("Source", "TableName", "Label"))
  labels <- .lbl_dataset_labels(found)

  for (t in live_sample(tables)) {
    i <- db$query(.lbl_sql_pick("Analysis", s, "dbo", t))$Source
    expected <- character()
    if (i > 0) {
      # Any of the table's dataset labels, as SQL Server returns rows in no
      # set order
      rows <- db$query(.lbl_sql_rows("Analysis", s[i, ], "dbo", t))
      type <- .lbl_types[tolower(trimws(rows$PropertyType))]
      text <- trimws(rows$PropertyText)
      expected <- text[type %in% "DatasetLabel" & !is.na(text) & nzchar(text)]
    }
    if (length(expected)) {
      expect_true(unname(labels[tolower(t)]) %in% expected, label = t)
    } else {
      expect_false(tolower(t) %in% names(labels), label = t)
    }
  }

})

test_that("the code the browser copies reads the table in every form", {

  skip_if_no_episerver()
  # The code connects through ODBC itself, not through .epi_connection()
  skip_if_no_sqlserver_driver()
  withr::local_package("dplyr")

  columns <- c("SVYYEAR", "RSEX_Y20")
  forms <- expand.grid(mode = c("quickconnect", "connect", "none"),
                       declare = c(TRUE, FALSE), labels = c(TRUE, FALSE),
                       stringsAsFactors = FALSE)
  for (i in seq_len(nrow(forms))) {
    f <- forms[i, ]
    label <- paste(f$mode, if (f$declare) "declared" else "bare",
                   if (f$labels) "with labels" else "without labels")
    run <- new.env()
    if (f$mode == "none") run$tb <- episerver_quickconnect("ACTGHSMYX")
    eval(parse(text = .browse_code("Analysis", "dbo", "ACTGHSMYX", columns,
                                   f$mode, f$declare, f$labels)), run)
    if (!is.null(run$conn)) DBI::dbDisconnect(run$conn)

    expect_named(run$data, columns, label = label)
    expect_gt(nrow(run$data), 0)
    # With labels, the sex of respondents takes its value labels
    expect_equal(inherits(run$data$RSEX_Y20, "haven_labelled"), f$labels,
                 label = label)
  }

  # With a label table chosen, the code applies the labels of that table
  run <- new.env()
  eval(parse(text = .browse_code("Analysis", "dbo", "ACTGHSMYX", columns,
                                 label_table = c("ref", "DataLabels"))), run)
  labels <- episerver_getlabels(episerver_quickconnect("ACTGHSMYX"),
                                lbl_schema = "ref", lbl_table = "DataLabels")
  sex <- labels[toupper(labels$VarName) == "RSEX_Y20" &
                  labels$LabelType == "Opt", ]
  expect_setequal(names(attr(run$data$RSEX_Y20, "labels")), sex$LabelName)

})

test_that("the app lists and describes a table against EpiServer", {

  skip_if_no_episerver()

  app <- episerver_browse_app()

  shiny::testServer(app, {
    session$setInputs(db = "Analysis", schema = "dbo", labels_source = "auto",
                      show_values = TRUE, table = "ACTGHSMYX",
                      insert_mode = "quickconnect", declare_pkgs = TRUE,
                      use_labels = TRUE)

    expect_null(tables()$error)
    expect_null(dataset_labels()$error)
    expect_null(columns()$error)
    expect_null(labels()$error)
    expect_null(values()$error)
    expect_false(is.null(labels()$source))

    d <- .browse_columns(columns()$data, lbl(), values = TRUE)
    expect_equal(nrow(d), nrow(columns()$data))
    expect_match(d$`Value labels`[d$Column == "RSEX_Y20"], "Male", fixed = TRUE)
    expect_match(output$info_bar$html, "Labels from ref.", fixed = TRUE)
    expect_no_match(output$info_bar$html, "epi-warn", fixed = TRUE)

    # A view, when dbo has one
    views <- tables()$data$TABLE_NAME[tables()$data$TABLE_TYPE == "VIEW"]
    if (length(views)) {
      session$setInputs(table = views[1])
      expect_equal(chosen()$type, "view")
      expect_null(columns()$error)
      expect_null(labels()$error)
    }
  })

})
