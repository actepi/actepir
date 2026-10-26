# tests/testthat/test-episerver_labels.R
#
# Offline tests for the labels lookup (R/episerver_labels.R): the SQL that
# finds the label tables and reads a table's rows, normalising both layouts,
# the order of notes and codes, and the choice of label table. The queries
# are run against EpiServer in test-episerver_browse_live.R.

library(testthat)
library(actepir)

sources <- data.frame(
  TABLE_SCHEMA = c("ref", "ref", "dbo"),
  TABLE_NAME   = c("DataProperties", "DataLabels", "DataLabels"),
  Layout       = c("properties", "labels", "labels"),
  stringsAsFactors = FALSE
)

# The rows of a label table as .lbl_sql_rows() returns them
rows <- function(column, type, code, text) {
  data.frame(ColumnName = column, PropertyType = type, PropertyCode = code,
             PropertyText = text, stringsAsFactors = FALSE)
}

test_that("identifiers and literals are quoted for SQL Server", {

  expect_equal(.sql_ident("Analysis"), "[Analysis]")
  expect_equal(.sql_ident("Analysis", "ref", "Data]Labels"),
               "[Analysis].[ref].[Data]]Labels]")
  expect_equal(.sql_quote("O'Brien"), "'O''Brien'")
  expect_equal(.sql_quote("APC", unicode = TRUE), "N'APC'")

})

test_that("label tables are DataProperties in ref and DataLabels tables, in order", {

  sql <- .lbl_sql_sources("My]DB")
  expect_match(sql, "FROM [My]]DB].INFORMATION_SCHEMA.TABLES t", fixed = TRUE)
  expect_match(sql, "FROM [My]]DB].INFORMATION_SCHEMA.COLUMNS c", fixed = TRUE)
  # DataProperties is looked for in ref only; DataLabels tables anywhere
  expect_match(sql, "t.TABLE_SCHEMA = 'ref' AND t.TABLE_NAME = 'DataProperties'",
               fixed = TRUE)
  expect_match(sql, "t.TABLE_NAME LIKE '%DataLabels%'", fixed = TRUE)
  # Tables or views: no table type is required
  expect_no_match(sql, "TABLE_TYPE", fixed = TRUE)
  # Each must have every column of its layout; DataLabels still needs DataType
  expect_match(sql, paste0("IN ('TableSchema', 'TableName', 'ColumnName', ",
                           "'PropertyType', 'PropertyCode', 'PropertyText')) = 6"),
               fixed = TRUE)
  expect_match(sql, paste0("IN ('Dataset', 'VarName', 'LabelType', ",
                           "'LabelName', 'DataCode', 'DataType')) = 6"),
               fixed = TRUE)
  # DataProperties, DataLabels, DataLabelsNew, then others; ref first in each
  expect_match(sql, paste0(
    "(?s)ORDER BY\\s+CASE WHEN t.TABLE_NAME = 'DataProperties' THEN 0\\s+",
    "WHEN t.TABLE_NAME = 'DataLabels'\\s+THEN 1\\s+",
    "WHEN t.TABLE_NAME = 'DataLabelsNew'\\s+THEN 2\\s+ELSE 3 END,\\s+",
    "CASE WHEN t.TABLE_SCHEMA = 'ref' THEN 0 ELSE 1 END,\\s+",
    "t.TABLE_SCHEMA, t.TABLE_NAME"), perl = TRUE)

})

test_that("rows are matched by schema and name, or by name alone", {

  expect_equal(.lbl_where("properties", "dbo", "O'Neil"),
               "TableSchema = N'dbo' AND TableName = N'O''Neil'")
  expect_equal(.lbl_where("labels", "dbo", "APC"), "Dataset = 'APC'")

})

test_that("one query picks the first label table that holds rows for a table", {

  sql <- .lbl_sql_pick("Analysis", sources, "dbo", "APC")
  whens <- regmatches(sql, gregexpr("WHEN EXISTS [^\n]*", sql))[[1]]
  expect_equal(whens, c(
    paste("WHEN EXISTS (SELECT 1 FROM [Analysis].[ref].[DataProperties]",
          "WHERE TableSchema = N'dbo' AND TableName = N'APC') THEN 1"),
    paste("WHEN EXISTS (SELECT 1 FROM [Analysis].[ref].[DataLabels]",
          "WHERE Dataset = 'APC') THEN 2"),
    paste("WHEN EXISTS (SELECT 1 FROM [Analysis].[dbo].[DataLabels]",
          "WHERE Dataset = 'APC') THEN 3")
  ))
  expect_match(sql, "^SELECT CASE\n")
  expect_match(sql, "ELSE 0 END AS Source$")

})

test_that("rows are read in the shape of DataProperties, value labels apart", {

  props <- .lbl_sql_rows("Analysis", sources[1, ], "dbo", "APC")
  expect_match(props, "SELECT ColumnName, PropertyType, PropertyCode, PropertyText",
               fixed = TRUE)
  expect_match(props, "FROM [Analysis].[ref].[DataProperties]", fixed = TRUE)
  expect_match(props, "WHERE TableSchema = N'dbo' AND TableName = N'APC'",
               fixed = TRUE)
  expect_match(props, "AND PropertyType NOT IN (N'ValueLabel')", fixed = TRUE)
  expect_match(.lbl_sql_rows("Analysis", sources[1, ], "dbo", "APC", TRUE),
               "AND PropertyType IN (N'ValueLabel')", fixed = TRUE)

  labels <- .lbl_sql_rows("Analysis", sources[3, ], "dbo", "APC")
  expect_match(labels, paste("SELECT VarName AS ColumnName, LabelType AS",
                             "PropertyType, DataCode AS PropertyCode,",
                             "LabelName AS PropertyText"), fixed = TRUE)
  expect_match(labels, "FROM [Analysis].[dbo].[DataLabels]", fixed = TRUE)
  expect_match(labels, "WHERE Dataset = 'APC'", fixed = TRUE)
  expect_match(labels, "AND LabelType NOT IN ('Opt', 'ValueLabel')",
               fixed = TRUE)
  expect_match(.lbl_sql_rows("Analysis", sources[3, ], "dbo", "APC", TRUE),
               "AND LabelType IN ('Opt', 'ValueLabel')", fixed = TRUE)

})

test_that("dataset labels come with the tables each earlier label table holds", {

  sql <- .lbl_sql_dataset_labels("Analysis", sources, "dbo")
  parts <- strsplit(sql, "\nUNION ALL\n", fixed = TRUE)[[1]]
  expect_length(parts, 5)
  # DataProperties: the schema's dataset labels, and the tables it holds
  expect_match(parts[1], "^SELECT 1 AS Source, TableName COLLATE DATABASE_DEFAULT AS TableName")
  expect_match(parts[1], "WHERE TableSchema = N'dbo'", fixed = TRUE)
  expect_match(parts[1], "PropertyType IN (N'DatasetLabel', N'DataLabel')",
               fixed = TRUE)
  expect_match(parts[2], "^SELECT DISTINCT 1, TableName COLLATE DATABASE_DEFAULT")
  expect_match(parts[2], "CAST(NULL AS nvarchar(1))", fixed = TRUE)
  expect_match(parts[2], "WHERE TableSchema = N'dbo'", fixed = TRUE)
  # DataLabels tables file rows by name alone, in any schema
  expect_match(parts[3], "^SELECT 2 AS Source, Dataset COLLATE DATABASE_DEFAULT")
  expect_match(parts[3], "LabelType IN ('DtaLabel', 'DatasetLabel', 'DataLabel')",
               fixed = TRUE)
  expect_match(parts[4], "^SELECT DISTINCT 2, Dataset")
  expect_no_match(parts[4], "WHERE", fixed = TRUE)
  # The last label table needs no list of the tables it holds
  expect_match(parts[5], "^SELECT 3 AS Source")
  expect_no_match(paste(parts[5], collapse = ""), "DISTINCT", fixed = TRUE)

  one <- .lbl_sql_dataset_labels("Analysis", sources[2, ], "dbo")
  expect_no_match(one, "UNION ALL", fixed = TRUE)

})

test_that("a table's dataset label comes from the first label table holding it", {

  found <- data.frame(
    Source    = c(1L, 1L, 2L, 2L, 2L, 3L, 2L, 1L),
    TableName = c("APC", "Births", "apc", "Births", "ED", "Deaths", "ED ",
                  "Plain"),
    Label     = c("Admitted patient care", NA, "Old APC label",
                  "Births, old label", " Emergency ", "Registered deaths", NA,
                  ""),
    stringsAsFactors = FALSE
  )
  labels <- .lbl_dataset_labels(found)
  # Births has rows in the first label table but no dataset label there
  expect_equal(labels, c(apc = "Admitted patient care",
                         ed = "Emergency", deaths = "Registered deaths"))
  expect_length(.lbl_dataset_labels(NULL), 0)
  expect_length(.lbl_dataset_labels(found[0, ]), 0)

})

test_that("both layouts normalise to one shape, with types in any case", {

  found <- rows(
    column = c(" Sex ", "Sex", "sex", "Sex", "", "", "Age", "", "id", "Age",
               "Age", "Age", "Age", "Sex"),
    type   = c("VariableLabel", "Var", "VAR", "opt", "DtaLabel", "DataLabel",
               "NoteVar", "notedta", "Link", "VariableFormat", "Format",
               "Unknown", NA, "ValueLabel"),
    code   = c(NA, NA, NA, " 1 ", NA, NA, "2", NA, NA, NA, NA, NA, NA, "1"),
    text   = c("Sex of the patient ", "Second label", "Third label", "Male",
               "Admissions", "Second dataset label", "A note", "Data note",
               "APC_Extra", "%9.0f", "%9.0f", "x", "y", "Second label of 1")
  )
  n <- .lbl_normalise(found)
  expect_named(n, c("Column", "Key", "Type", "Code", "Text"))
  # Formats and unknown types are not read; the first label of a column, a
  # code or a dataset is kept; text and codes are trimmed
  expect_equal(n$Type, c("VariableLabel", "ValueLabel", "DatasetLabel",
                         "VariableNote", "DatasetNote", "Link"))
  expect_equal(n$Column, c("Sex", "Sex", "", "Age", "", "id"))
  expect_equal(n$Key, c("sex", "sex", "", "age", "", "id"))
  expect_equal(n$Code[2], "1")
  expect_equal(n$Text[1:3], c("Sex of the patient", "Male", "Admissions"))

  # Rows without text are dropped, and nothing gives an empty frame
  expect_equal(nrow(.lbl_normalise(rows("Sex", "Var", NA, c("", NA)))), 0)
  expect_named(.lbl_normalise(NULL), c("Column", "Key", "Type", "Code", "Text"))

  # The type names of DataProperties, sqldta 1.0.0 and the DataLabels layout
  for (type in names(.lbl_types)) {
    expect_equal(.lbl_normalise(rows("x", toupper(type), "1", "t"))$Type,
                 unname(.lbl_types[[type]]), label = type)
  }

})

test_that("codes sort as numbers first, then as text", {

  code <- c("10", "b", "2", "-1", "a", "1.5", "B")
  expect_equal(code[.lbl_code_order(code)],
               c("-1", "1.5", "2", "10", "B", "a", "b"))

})

test_that("a table's labels are gathered by column, with notes in sqldta's order", {

  found <- .lbl_normalise(rows(
    column = c("Sex", "Age", "Age", "Age", "Age", "Triage", "Triage", "", "",
               "", "", "id"),
    type   = c("Var", "Var", "NoteVar", "NoteVar", "NoteVar", "NoteVar",
               "NoteVar", "DtaLabel", "NoteDta", "NoteDta", "NoteDta", "Link"),
    code   = c(NA, NA, "10", NA, "2", "b", "a", NA, NA, "1", "B", NA),
    text   = c("Sex", "Age in years", "Tenth", "Uncoded", "Second",
               "Noted second", "Noted first", "Admissions", "Plain note",
               "Note 1", "Note B", "APC_Extra")
  ))
  values <- .lbl_normalise(rows(
    column = "Sex", type = "Opt", code = c("9", "2", "10", "1", "x"),
    text = c("Not stated", "Female", "Other", "Male", "Unknown")
  ))
  lbl <- .lbl_table(found, values)

  expect_equal(lbl$label, c(sex = "Sex", age = "Age in years"))
  # Coded notes first, numerically when every code is a number, otherwise
  # as text; then the others in the order of the rows
  expect_equal(lbl$notes$age, c("Second", "Tenth", "Uncoded"))
  expect_equal(lbl$notes$triage, c("Noted first", "Noted second"))
  expect_equal(lbl$values$sex$Code, c("1", "2", "9", "10", "x"))
  expect_equal(lbl$values$sex$Text[1], "Male")
  expect_equal(lbl$dataset$label, "Admissions")
  expect_equal(lbl$dataset$notes, c("Note 1", "Note B", "Plain note"))
  expect_equal(lbl$dataset$links, data.frame(Key = "id", Table = "APC_Extra"))

  # Without value labels read, a table has none; without rows, nothing
  expect_length(.lbl_table(found)$values, 0)
  empty <- .lbl_table(.lbl_normalise(NULL))
  expect_length(empty$label, 0)
  expect_true(is.na(empty$dataset$label))
  expect_length(empty$dataset$notes, 0)
  expect_equal(nrow(empty$dataset$links), 0)

})
