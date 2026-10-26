# tests/testthat/test-episerver_getlabels.R
#
# Offline tests for episerver_getlabels() and collect_withlabels(): where the
# table is, which label table the lbl_* arguments name, and the labels read
# through the labels lookup. The connection's queries go to
# fake_browse_episerver() (helper-browse.R), so nothing here needs EpiServer.
# The live tests are in test-episerver_getlabels_live.R.

library(testthat)
library(actepir)

# A lazy table of Analysis.dbo.<table>, as dbplyr makes it for SQL Server,
# whose connection is never used: the queries are mocked
fake_lazy <- function(table, schema = "dbo", db = "Analysis") {
  path <- structure(sprintf("[%s].[%s].[%s]", db, schema, table),
                    class = c("dbplyr_table_path", "character"))
  structure(list(lazy_query = list(x = list(x = path)),
                 src = list(con = "lazy connection")),
            class = c("tbl_sql", "tbl_lazy", "tbl"))
}

test_that("a lazy table's database, schema and name come from its query", {

  expect_equal(.lbl_table_path(fake_lazy("ACTGHSMYX")),
               c("Analysis", "dbo", "ACTGHSMYX"))
  expect_equal(.lbl_table_path(fake_lazy("APC", "stage", "HospitalAPC")),
               c("HospitalAPC", "stage", "APC"))
  expect_error(.lbl_table_path(list(lazy_query = list(x = data.frame()))),
               "does not match expected format", fixed = TRUE)

})

test_that("the lbl_* arguments name one label table, with the old defaults", {

  # None given: the lookup chooses
  expect_equal(.lbl_target(c("Analysis", "dbo", "APC")),
               list(db = "Analysis", schema = "dbo", table = "APC",
                    label = NULL))
  # A table's name alone is in Analysis.dbo
  expect_equal(.lbl_target("APC")[c("db", "schema", "table")],
               list(db = "Analysis", schema = "dbo", table = "APC"))
  # Any given: the others default to the table's database, ref and DataLabels
  expect_equal(.lbl_target(c("HospitalAPC", "dbo", "APC"),
                           lbl_table = "DataLabelsNew")$label,
               c(db = "HospitalAPC", schema = "ref", table = "DataLabelsNew"))
  expect_equal(.lbl_target(c("Analysis", "dbo", "APC"), lbl_schema = "dbo")$label,
               c(db = "Analysis", schema = "dbo", table = "DataLabels"))
  expect_equal(.lbl_target("APC", lbl_db = "HospitalAPC", lbl_schema = "dbo",
                           lbl_table = "DataLabelsNew")$label,
               c(db = "HospitalAPC", schema = "dbo", table = "DataLabelsNew"))
  expect_null(.lbl_target("APC", lbl_db = NA, lbl_table = NULL)$label)

})

test_that("episerver_getlabels() reads a lazy table's labels through its connection", {

  fake <- fake_browse_episerver()
  cons <- character()
  local_mocked_bindings(dbGetQuery = function(conn, statement, ...) {
    cons <<- c(cons, conn)
    fake$query(statement)
  }, .package = "DBI")

  labels <- episerver_getlabels(fake_lazy("APC"))
  expect_equal(labels, .lbl_get(fake$query, "Analysis", "dbo", "APC"))
  expect_true(all(cons == "lazy connection"))

  # One label table named, with the default schema ref
  expect_equal(episerver_getlabels(fake_lazy("APC"), lbl_table = "DataLabels")$LabelName,
               c("Old sex label", "M"))
  expect_message(none <- episerver_getlabels(fake_lazy("Plain")),
                 "No label table of Analysis holds labels for dbo.Plain.",
                 fixed = TRUE)
  expect_equal(nrow(none), 0)

})

test_that("episerver_getlabels() connects for a table's name, and disconnects", {

  fake <- fake_browse_episerver()
  connected <- list()
  disconnected <- 0
  local_mocked_bindings(episerver_connect = function(...) {
    connected[[length(connected) + 1]] <<- list(...)
    "named connection"
  })
  local_mocked_bindings(
    dbGetQuery = function(conn, statement, ...) fake$query(statement),
    dbDisconnect = function(conn, ...) disconnected <<- disconnected + 1,
    .package = "DBI"
  )

  # Its name alone: the lookup, in Analysis.dbo
  expect_equal(episerver_getlabels("ED")$LabelName,
               c("Triage category", "Resuscitation", "Emergency"))
  expect_equal(disconnected, 1)
  # With one label table named, as before the lookup
  labels <- episerver_getlabels("APC", lbl_table = "DataLabels",
                                lbl_schema = "ref", lbl_db = "Analysis",
                                driver = "ODBC Driver 17 for SQL Server",
                                encrypt = "No")
  expect_equal(labels$LabelName, c("Old sex label", "M"))
  expect_equal(disconnected, 2)
  expect_equal(connected[[2]], list(driver = "ODBC Driver 17 for SQL Server",
                                    max_attempts = NULL, encrypt = "No"))

  expect_error(episerver_getlabels(), "requires argument 'dataset'")
  expect_error(episerver_getlabels(data.frame()), "do not resolve")

})

test_that("collect_withlabels() applies the labels of the table collected", {

  fake <- fake_browse_episerver()
  local_mocked_bindings(dbGetQuery = function(conn, statement, ...) {
    fake$query(statement)
  }, .package = "DBI")
  local_mocked_bindings(collect = function(x, ...) {
    data.frame(Sex = c(1, 2, 9), AgeYrs = c(30, 40, 50))
  }, .package = "dplyr")

  data <- collect_withlabels(fake_lazy("APC"))
  expect_s3_class(data$Sex, "haven_labelled")
  expect_equal(attr(data$Sex, "labels"),
               c(Male = 1, Female = 2, "Not stated" = 9, Other = 10))
  expect_equal(attr(data$Sex, "label"), "Sex of the patient")
  expect_equal(attr(data$AgeYrs, "label"), "Age in years (<1 is 0)")
  expect_false(inherits(data$AgeYrs, "haven_labelled"))

})
