# tests/testthat/test-episerver_getlabels_live.R
#
# Live tests for episerver_getlabels() and collect_withlabels(). Each needs
# EpiServer and is skipped when it cannot be reached. Those that read lazy
# tables also need a SQL Server ODBC driver, as a lazy table connects through
# ODBC itself. They hold whether or not ref.DataProperties exists yet.

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
live_sample <- function(db) {
  tables <- db$query(.browse_sql_tables("Analysis", "dbo"))
  c("ACTGHSMYX", utils::head(setdiff(tables$TABLE_NAME, "ACTGHSMYX"), 30))
}

test_that("scripts take each table's labels from the label table the browser shows", {

  skip_if_no_episerver()
  db <- live_db()

  s <- db$query(.lbl_sql_sources("Analysis"))
  for (t in live_sample(db)) {
    got <- suppressMessages(.lbl_get(db$query, "Analysis", "dbo", t))
    expect_named(got, c("Dataset", "VarName", "LabelType", "DataCode",
                        "LabelName"))
    i <- db$query(.lbl_sql_pick("Analysis", s, "dbo", t))$Source
    if (i == 0) {
      expect_equal(nrow(got), 0, label = t)
      next
    }
    # The variable and value labels the browser shows for the table
    rows <- .lbl_normalise(rbind(
      db$query(.lbl_sql_rows("Analysis", s[i, ], "dbo", t)),
      db$query(.lbl_sql_rows("Analysis", s[i, ], "dbo", t, values = TRUE))
    ))
    lbl <- .lbl_table(rows)
    vars <- got[got$LabelType == "Var", ]
    vals <- got[got$LabelType == "Opt", ]
    expect_true(all(got$Dataset == t), label = t)
    expect_equal(stats::setNames(vars$LabelName, tolower(vars$VarName)),
                 lbl$label, label = t)
    expect_equal(nrow(vals), sum(vapply(lbl$values, nrow, integer(1))),
                 label = t)
  }

})

test_that("scripts can name one label table, in either layout", {

  skip_if_no_episerver()
  db <- live_db()

  labels <- .lbl_get(db$query, "Analysis", "dbo", "ACTGHSMYX",
                     c("Analysis", "ref", "DataLabels"))
  expect_gt(nrow(labels), 0)
  expect_true(all(labels$LabelType %in% c("Var", "Opt")))
  sex <- labels[toupper(labels$VarName) == "RSEX_Y20" &
                  labels$LabelType == "Opt", ]
  expect_true(all(c("Male", "Female") %in% sex$LabelName))

  s <- db$query(.lbl_sql_sources("Analysis"))
  if ("properties" %in% s$Layout) {
    properties <- suppressMessages(.lbl_get(
      db$query, "Analysis", "dbo", "ACTGHSMYX",
      c("Analysis", "ref", "DataProperties")
    ))
    expect_named(properties, names(labels))
  }

  expect_error(.lbl_get(db$query, "Analysis", "dbo", "ACTGHSMYX",
                        c("Analysis", "dbo", "ACTGHSMYX")),
               "is not a label table", fixed = TRUE)

})

test_that("episerver_getlabels() with ref.DataLabels named gives the labels it gave before", {

  skip_if_no_episerver()
  # The lazy table connects through ODBC itself
  skip_if_no_sqlserver_driver()

  tb <- episerver_quickconnect("ACTGHSMYX")
  new <- episerver_getlabels(tb, lbl_table = "DataLabels")
  # The query episerver_getlabels() ran before the labels lookup
  old <- dplyr::tbl(tb$src$con, dbplyr::in_catalog("Analysis", "ref", "DataLabels")) |>
    dplyr::filter(Dataset == "ACTGHSMYX" & tolower(LabelType) %in% c("opt", "var")) |>
    dplyr::collect()

  # The same columns and codes labelled, each with one of the labels it had
  key <- function(d) {
    type <- tolower(d$LabelType)
    paste(toupper(trimws(d$VarName)), type,
          ifelse(type == "opt", trimws(d$DataCode), ""), sep = "|")
  }
  had <- !is.na(old$LabelName) & nzchar(trimws(old$LabelName))
  expect_setequal(key(new), unique(key(old)[had]))
  for (k in seq_len(nrow(new))) {
    expect_true(new$LabelName[k] %in% trimws(old$LabelName[key(old) == key(new)[k]]),
                label = key(new)[k])
  }

})

test_that("collect_withlabels() applies the labels the lookup finds", {

  skip_if_no_episerver()
  # The lazy table connects through ODBC itself
  skip_if_no_sqlserver_driver()
  db <- live_db()

  tb <- episerver_quickconnect("ACTGHSMYX")
  expect_equal(episerver_getlabels(tb),
               suppressMessages(.lbl_get(db$query, "Analysis", "dbo",
                                         "ACTGHSMYX")))
  # A table's name gives the labels of its lazy table in Analysis.dbo
  expect_equal(episerver_getlabels("ACTGHSMYX"), episerver_getlabels(tb))

  data <- tb |>
    dplyr::filter(SVYYEAR == 2023) |>
    dplyr::select(SVYYEAR, RSEX_Y20) |>
    collect_withlabels()
  expect_s3_class(data$RSEX_Y20, "haven_labelled")
  expect_true(all(c("Male", "Female") %in% names(attr(data$RSEX_Y20, "labels"))))

  # Tables labelled in ref.DataProperties, when it exists, get those labels
  s <- db$query(.lbl_sql_sources("Analysis"))
  if ("properties" %in% s$Layout) {
    from_properties <- Filter(function(t) {
      db$query(.lbl_sql_pick("Analysis", s, "dbo", t))$Source == 1
    }, live_sample(db))
    for (t in utils::head(from_properties, 3)) {
      expect_equal(episerver_getlabels(episerver_quickconnect(t)),
                   .lbl_get(db$query, "Analysis", "dbo", t), label = t)
    }
  }

})
