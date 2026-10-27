# tests/testthat/test-episerver_dictionary_live.R
#
# Live tests for the data dictionaries. Each needs EpiServer and is skipped
# when it cannot be reached. The test of episerver_dictionary() also needs a
# SQL Server ODBC driver, as it connects through episerver_connect(); its PDF
# is made only where Quarto is found.

library(testthat)
library(actepir)

test_that("a dictionary holds the labels that scripts read, for the table's columns", {

  skip_if_no_episerver()
  db <- .epi_connection()
  withr::defer(db$disconnect())

  tables <- db$query(.browse_sql_tables("Analysis", "dbo"))$TABLE_NAME
  for (t in c("ACTGHSMYX", utils::head(setdiff(tables, "ACTGHSMYX"), 10))) {
    d <- .dict_build(db$query, "Analysis", "dbo", t)
    cols <- db$query(.browse_sql_columns("Analysis", "dbo", t))
    expect_equal(d$columns$column, as.character(cols$col), label = t)

    # The labels of the table's columns, as episerver_getlabels() reads them
    labels <- suppressMessages(.lbl_get(db$query, "Analysis", "dbo", t))
    labels <- labels[tolower(labels$VarName) %in% tolower(d$columns$column), ]
    vars <- labels[labels$LabelType == "Var", ]
    described <- stats::setNames(d$columns$description, tolower(d$columns$column))
    expect_equal(unname(described[tolower(vars$VarName)]), vars$LabelName,
                 label = t)
    flat <- .dict_flat(d)
    # The dictionary for Excel: the headings and a line for each row, as
    # text and as the HTML table's rows
    excel <- .dict_excel(flat)
    expect_length(strsplit(excel$text, "\n")[[1]], nrow(flat) + 1)
    expect_equal(lengths(regmatches(excel$html, gregexpr("<tr>", excel$html,
                                                         fixed = TRUE))),
                 nrow(flat) + 1, label = t)
    flat <- flat[!is.na(flat$code), ]
    opts <- labels[labels$LabelType == "Opt", ]
    expect_setequal(paste(tolower(flat$column), flat$code, flat$value_label),
                    paste(tolower(opts$VarName), opts$DataCode, opts$LabelName))
  }

})

test_that("a database's tables are listed with whether a label table holds labels for them", {

  skip_if_no_episerver()
  db <- .epi_connection()
  withr::defer(db$disconnect())

  listed <- .dict_tables(db$query, "Analysis", "dbo")
  tables <- db$query(.browse_sql_tables("Analysis", "dbo"))
  expect_equal(listed$table, as.character(tables$TABLE_NAME))
  expect_true(listed$labelled[listed$table == "ACTGHSMYX"])
  # Up to 10 of each, against the lookup that reads their labels
  check <- c(utils::head(which(listed$labelled), 10),
             utils::head(which(!listed$labelled), 10))
  for (i in check) {
    r <- .lbl_read(db$query, "Analysis", "dbo", listed$table[i])
    expect_equal(is.null(r$none), listed$labelled[i], label = listed$table[i])
  }

})

test_that("episerver_dictionary() writes a table's files", {

  skip_if_no_episerver()
  skip_if_no_sqlserver_driver()
  withr::local_envvar(R_USER_CONFIG_DIR = withr::local_tempdir())
  dir <- withr::local_tempdir()
  formats <- c("html", if (!is.null(.dict_quarto())) "pdf")

  out <- suppressMessages(episerver_dictionary("ACTGHSMYX", formats = formats,
                                               dir = dir))
  expect_gt(nrow(out), 0)
  html <- paste(readLines(file.path(dir, "ACTGHSMYX_dictionary.html"),
                          encoding = "UTF-8"), collapse = "\n")
  expect_match(html, "RSEX_Y20", fixed = TRUE)
  expect_match(html, "Female", fixed = TRUE)
  if ("pdf" %in% formats) {
    pdf <- file.path(dir, "ACTGHSMYX_dictionary.pdf")
    expect_equal(readBin(pdf, "raw", 4), charToRaw("%PDF"))
  }

})
