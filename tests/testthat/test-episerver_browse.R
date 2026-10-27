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

test_that("the Tables tab's queries read the catalogue and the label tables", {

  details <- .browse_sql_details("My]DB", "O'Brien")
  expect_match(details, "FROM [My]]DB].sys.objects o", fixed = TRUE)
  expect_match(details, "JOIN [My]]DB].sys.schemas s", fixed = TRUE)
  expect_match(details, "WHERE s.name = N'O''Brien'", fixed = TRUE)
  expect_match(details, "o.type IN ('U', 'V')", fixed = TRUE)
  # Rows of the heap or clustered index, as a float
  expect_match(details, "CAST(SUM(p.rows) AS float)", fixed = TRUE)
  expect_match(details, "p.index_id IN (0, 1)", fixed = TRUE)
  expect_match(.browse_sql_schema_columns("Analysis", "dbo"),
               "SELECT TABLE_NAME, COLUMN_NAME\\s+FROM \\[Analysis\\]\\.INFORMATION_SCHEMA\\.COLUMNS\\s+WHERE TABLE_SCHEMA = N'dbo'")

  sources <- data.frame(TABLE_SCHEMA = c("ref", "ref"),
                        TABLE_NAME = c("DataProperties", "DataLabels"),
                        Layout = c("properties", "labels"))
  coverage <- .browse_sql_coverage("Analysis", sources, "dbo")
  parts <- strsplit(coverage, "\nUNION ALL\n", fixed = TRUE)[[1]]
  expect_length(parts, 2)
  expect_match(parts[1], "SELECT DISTINCT 1 AS Source", fixed = TRUE)
  expect_match(parts[1], "FROM [Analysis].[ref].[DataProperties]\n       WHERE TableSchema = N'dbo'",
               fixed = TRUE)
  expect_match(parts[1], "LOWER(LTRIM(RTRIM(ColumnName))) COLLATE DATABASE_DEFAULT",
               fixed = TRUE)
  # DataLabels rows are filed by name alone, so they are not filtered
  expect_match(parts[2], "SELECT DISTINCT 2 AS Source,\n         Dataset COLLATE DATABASE_DEFAULT",
               fixed = TRUE)
  expect_match(parts[2], "CASE WHEN LabelType IN ('VariableLabel', 'Var')", fixed = TRUE)
  expect_no_match(parts[2], "WHERE", fixed = TRUE)

})

test_that("the search's queries escape the text and leave out label tables", {

  expect_equal(.sql_like("50%_[a]\\b'c"), "N'%50\\%\\_\\[a]\\\\b''c%'")

  names <- .browse_sql_search_columns("Analysis", "sex_")
  expect_match(names, "SELECT TOP 1000 c.TABLE_SCHEMA, c.TABLE_NAME, c.COLUMN_NAME",
               fixed = TRUE)
  expect_match(names, "LOWER(c.COLUMN_NAME) LIKE LOWER(N'%sex\\_%') ESCAPE '\\'",
               fixed = TRUE)
  expect_match(names, "t.TABLE_NAME NOT LIKE '%DataLabels%'", fixed = TRUE)
  expect_match(names, "t.TABLE_TYPE IN ('BASE TABLE', 'VIEW')", fixed = TRUE)

  properties <- data.frame(TABLE_SCHEMA = "ref", TABLE_NAME = "DataProperties",
                           Layout = "properties")
  labels <- data.frame(TABLE_SCHEMA = "ref", TABLE_NAME = "DataLabels",
                       Layout = "labels")
  both <- .browse_sql_search_labels("Analysis", properties, "female")
  expect_match(both, "FROM [Analysis].[ref].[DataProperties]", fixed = TRUE)
  expect_match(both, "PropertyType IN ('VariableLabel', 'Var', 'ValueLabel', 'Opt')",
               fixed = TRUE)
  expect_match(both, "LOWER(PropertyText) LIKE LOWER(N'%female%')", fixed = TRUE)
  values <- .browse_sql_search_labels("Analysis", labels, "female", "values")
  expect_match(values, "CAST(NULL AS nvarchar(128)) AS TableSchema, Dataset AS TableName",
               fixed = TRUE)
  expect_match(values, "LabelType IN ('ValueLabel', 'Opt')", fixed = TRUE)
  expect_match(values, "LOWER(LabelName) LIKE", fixed = TRUE)

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

test_that("the Tables tab gives each table's rows, dates, columns and labels", {

  fake <- fake_browse_episerver()
  q <- fake$query
  sources <- q(.lbl_sql_sources("Analysis"))
  tables <- q(.browse_sql_tables("Analysis", "dbo"))
  details <- q(.browse_sql_details("Analysis", "dbo"))
  columns <- q(.browse_sql_schema_columns("Analysis", "dbo"))
  labels <- .lbl_dataset_labels(q(.lbl_sql_dataset_labels("Analysis", sources, "dbo")))

  d <- .browse_tables_overview(tables, labels, details, columns,
                               q(.browse_sql_coverage("Analysis", sources, "dbo")),
                               sources)
  expect_named(d, c("Table", "Description", "Type", "Rows", "Columns",
                    "Without var labels", "Labels from", "Created",
                    "Structure changed"))
  expect_equal(d$Table, c("ACTGHSMYX", "APC", "Births", "Deaths", "ED", "Plain",
                          "vw_ED"))
  expect_equal(d$Description[1:2], c("General health survey", "Admitted patient care"))
  expect_equal(d$Type, c(rep("Table", 6), "View"))
  expect_equal(d$Rows, c(45210, 1234567, 5300, 2104, 890000, 0, NA))
  expect_equal(d$Columns, c(2L, 43L, 1L, 1L, 2L, 1L, 1L))
  # Each table's labels come from the first label table holding rows for
  # it; APC's two labelled columns of 43 are from ref.DataProperties, not
  # ref.DataLabels
  expect_equal(d$`Labels from`, c("ref.DataProperties", "ref.DataProperties",
                                  "ref.DataProperties", "dbo.DataLabels",
                                  "ref.DataLabels", "", "ref.DataLabels"))
  expect_equal(d$`Without var labels`, c(1L, 41L, 0L, 0L, 1L, 1L, 0L))
  expect_equal(d$Created[1], "2020-01-15")
  expect_equal(d$`Structure changed`[1], "2020-04-24")

  # A label table chosen: its rows alone
  chosen <- sources[sources$TABLE_NAME == "DataLabels" & sources$TABLE_SCHEMA == "ref", ]
  d <- .browse_tables_overview(tables, labels, details, columns,
                               q(.browse_sql_coverage("Analysis", chosen, "dbo")),
                               chosen)
  expect_equal(d$`Labels from`, c("ref.DataLabels", "ref.DataLabels", "ref.DataLabels",
                                  "", "ref.DataLabels", "", "ref.DataLabels"))
  expect_equal(d$`Without var labels`, c(1L, 42L, 0L, 1L, 1L, 1L, 0L))

  # Without label tables or details: no labels, no rows or dates
  none <- details[0, ]
  d <- .browse_tables_overview(tables, character(), none, columns)
  expect_equal(d$`Labels from`, rep("", 7))
  expect_equal(d$`Without var labels`, d$Columns)
  expect_true(all(is.na(d$Rows)))
  expect_equal(d$Created, rep("", 7))
  expect_equal(.browse_date(c("2021-03-04 10:00:00", NA)), c("2021-03-04", ""))

})

test_that("the search finds columns by name and by the labels the browser reads", {

  fake <- fake_browse_episerver()
  sources <- fake$query(.lbl_sql_sources("Analysis"))
  search <- function(term, look_in = c("names", "labels", "values"),
                     src = sources, auto = TRUE) {
    .browse_search(fake$query, "Analysis", term, look_in, src, auto)
  }

  # Names and variable labels, in any case, in every schema of the database
  r <- search("SEX")
  expect_false(r$capped)
  expect_equal(r$results$Table, c("ACTGHSMYX", "APC", "APC"))
  expect_equal(r$results$Schema, c("dbo", "dbo", "stage"))
  expect_equal(r$results$Column, c("RSEX_Y20", "Sex", "Sex"))
  expect_equal(r$results$`Variable label`,
               c("Sex of respondent", "Sex of the patient", "Sex as staged"))
  expect_named(r$results, c("Schema", "Table", "Column", "Variable label",
                            "Value labels"))
  # Names alone
  expect_equal(search("sex", "names")$results$`Variable label`, rep("", 3))

  # Value labels, in code order
  r <- search("e", "values")
  expect_equal(r$results$`Value labels`[r$results$Table == "APC"],
               "1 = Male; 2 = Female; 9 = Not stated; and 1 more")
  expect_equal(search("female", "values")$results$Table, c("ACTGHSMYX", "APC"))

  # A label counts only in the label table the table's labels come from:
  # APC's come from ref.DataProperties, so ref.DataLabels' are not found
  expect_equal(nrow(search("Old sex label")$results), 0)
  # With that label table chosen they are, for every table of the name, as
  # DataLabels rows are filed by name alone
  chosen <- sources[sources$TABLE_SCHEMA == "ref" & sources$TABLE_NAME == "DataLabels", ]
  r <- search("Old sex label", src = chosen, auto = FALSE)
  expect_equal(paste(r$results$Schema, r$results$Table, r$results$Column),
               c("dbo APC Sex", "stage APC Sex"))
  # Label tables themselves are not searched as tables
  expect_false(any(c("DataLabels", "DataProperties") %in% search("Label")$results$Table))
  # LIKE's wildcards are taken as text
  expect_equal(nrow(search("%")$results), 0)

  # A query that stops at 1,000 rows is reported
  capped <- function(sql) {
    if (grepl("SELECT TOP 1000 c.TABLE_SCHEMA", sql, fixed = TRUE)) {
      return(data.frame(TABLE_SCHEMA = rep("dbo", 1000), TABLE_NAME = "APC",
                        COLUMN_NAME = paste0("x", 1:1000)))
    }
    fake$query(sql)
  }
  expect_true(.browse_search(capped, "Analysis", "x", "names", sources)$capped)

})

test_that("the Profiler reads each type as R can, and finds the keys", {

  expect_equal(.browse_sql_value("Sex", "int"), "[Sex]")
  expect_equal(.browse_sql_value("Flag", "BIT"), "CAST([Flag] AS tinyint)")
  expect_equal(.browse_sql_value("Id", "varbinary"),
               "CONVERT(varchar(max), CAST([Id] AS varbinary(max)), 1)")
  expect_equal(.browse_sql_value("Area", "geography"), "[Area].ToString()")
  expect_equal(.browse_sql_value("Node", "hierarchyid"), "[Node].ToString()")
  expect_equal(.browse_sql_value("Any", "sql_variant"), "CAST([Any] AS nvarchar(4000))")
  expect_equal(.browse_sql_value("Doc", "xml"), "CAST([Doc] AS nvarchar(max))")

  profile <- .browse_sql_profile("Analysis", "dbo", "APC", "Flag", "bit")
  expect_match(profile, "CAST(COUNT_BIG(*) AS float) AS TotalRows", fixed = TRUE)
  expect_match(profile, "CASE WHEN [Flag] IS NULL THEN 0 ELSE 1 END", fixed = TRUE)
  expect_match(profile, "COUNT_BIG(DISTINCT CAST([Flag] AS tinyint))", fixed = TRUE)
  expect_match(profile, "MIN(CAST([Flag] AS tinyint)) AS MinValue", fixed = TRUE)
  expect_match(profile, "CAST(NULL AS float) AS Blank", fixed = TRUE)
  # Blank text for text types; nothing compared for types that cannot be
  text <- .browse_sql_profile("Analysis", "dbo", "Deaths", "Cause", "varchar")
  expect_match(text, "LTRIM(RTRIM([Cause])) = ''", fixed = TRUE)
  xml <- .browse_sql_profile("Analysis", "dbo", "T", "Doc", "xml")
  expect_match(xml, "CAST(NULL AS float) AS DistinctValues", fixed = TRUE)
  expect_match(xml, "NULL AS MinValue", fixed = TRUE)
  expect_no_match(xml, "DISTINCT", fixed = TRUE)

  counts <- .browse_sql_profile_counts("Analysis", "dbo", "APC", "Flag", "bit")
  expect_match(counts, "SELECT TOP 1000 CAST([Flag] AS tinyint) AS ProfileValue",
               fixed = TRUE)
  expect_match(counts, "GROUP BY CAST([Flag] AS tinyint)", fixed = TRUE)
  expect_match(counts, "ORDER BY COUNT_BIG(*) DESC, CAST([Flag] AS tinyint)",
               fixed = TRUE)

  cross <- .browse_sql_crosstab("Analysis", "dbo", "APC", "Sex", "int", "Flag", "bit")
  expect_match(cross, "SELECT TOP 50001 [Sex] AS ProfileValue, CAST([Flag] AS tinyint) AS ProfileBy",
               fixed = TRUE)
  expect_match(cross, "GROUP BY [Sex], CAST([Flag] AS tinyint)", fixed = TRUE)

  keys <- .browse_sql_keys("My]DB", "dbo", "O'Brien")
  expect_match(keys, "FROM [My]]DB].sys.indexes i", fixed = TRUE)
  expect_match(keys, "WHERE s.name = N'dbo' AND o.name = N'O''Brien'", fixed = TRUE)
  expect_match(keys, "(i.is_primary_key = 1 OR i.is_unique = 1)", fixed = TRUE)
  # Only keys of one column
  expect_match(keys, "AND k.is_included_column = 0) = 1", fixed = TRUE)

})

test_that("columns are left out by key or by a pattern kept between sessions", {

  withr::local_envvar(R_USER_CONFIG_DIR = withr::local_tempdir())
  names <- c("PatientID", "patient_id", "RecordUID", "Valid", "Opioid", "MRN",
             "URNumber", "RETURN", "PatientId", "Sex", "Episode")
  why <- .browse_excluded(names, .browse_exclude_default, keys = "episode")
  expect_equal(why, c("ID$", "_id$", "ID$", NA, NA, "MRN", "^URN", NA, "Id$", NA,
                      "key"))
  # Case is followed unless a pattern says otherwise
  expect_true(is.na(.browse_excluded("Valid", "ID$")))
  expect_equal(.browse_excluded("Valid", "(?i)id$"), "(?i)id$")

  # Typed lines: blanks dropped, patterns that are not regular expressions
  expect_no_warning(
    typed <- .browse_exclude_parse("ID$\n\n  ^MRN  \n[unclosed\n")
  )
  expect_equal(typed$patterns, c("ID$", "^MRN", "[unclosed"))
  expect_equal(typed$invalid, "[unclosed")
  expect_equal(.browse_exclude_parse(NULL)$patterns, character())

  # Kept between sessions; NULL goes back to the defaults
  expect_equal(.browse_exclude_read(), .browse_exclude_default)
  .browse_exclude_save(c("^Key", "Code$"))
  expect_true(file.exists(.browse_exclude_file()))
  expect_equal(.browse_exclude_read(), c("^Key", "Code$"))
  .browse_exclude_save(NULL)
  expect_false(file.exists(.browse_exclude_file()))
  expect_equal(.browse_exclude_read(), .browse_exclude_default)

})

test_that("a cross tab gives the rows of each pair of values, with totals and labels", {

  pairs <- data.frame(
    ProfileValue = c(1, 1, 2, 2, NA, 4, 2),
    ProfileBy    = c("b", "a", "a", "c", "a", "d", NA),
    ProfileCount = c(10, 20, 30, 5, 2, 3, 1)
  )
  vals <- data.frame(Code = c("1", "2"), Text = c("Male", "Female"))
  by_vals <- data.frame(Code = c("a", "b"), Text = c("Alpha", "Beta"))
  x <- .browse_crosstab(pairs, vals, by_vals)
  expect_equal(x$table$Value, c("1", "2", "4", "NA", "Total"))
  expect_equal(x$table$Label, c("Male", "Female", "(no label)", "", ""))
  expect_named(x$table, c("Value", "Label", "Alpha", "Beta", "c", "d", "NA", "Total"))
  expect_equal(x$titles, c("a: Alpha", "b: Beta", "c", "d", "NA", ""))
  expect_equal(x$table$Alpha, c(20, 30, 0, 2, 52))
  expect_equal(x$table$Total, c(30, 36, 3, 2, 71))
  expect_false(x$rows_capped || x$cols_capped)

  # Per cents of rows and of columns
  row <- .browse_crosstab(pairs, vals, by_vals, show = "row")$table
  expect_equal(row$Total, rep(100, 5))
  expect_equal(round(row$Alpha[1], 2), round(100 * 20 / 30, 2))
  col <- .browse_crosstab(pairs, vals, by_vals, show = "column")$table
  expect_equal(col$Alpha[5], 100)
  expect_equal(round(col$Total[2], 2), round(100 * 36 / 71, 2))

  # The most common kept, the rest put together, still in code order
  x <- .browse_crosstab(pairs, NULL, NULL, max_rows = 2, max_cols = 2)
  expect_equal(x$table$Value, c("1", "2", "Other values", "Total"))
  expect_named(x$table, c("Value", "a", "b", "Other", "Total"))
  expect_equal(x$table$Other, c(0, 6, 3, 9))
  expect_true(x$rows_capped && x$cols_capped)
  expect_equal(x$table$Total[4], 71)

  # Headings that would repeat take their codes
  same <- data.frame(Code = c("a", "b"), Text = c("Same", "Same"))
  expect_equal(names(.browse_crosstab(pairs, NULL, same)$table)[2:3],
               c("Same (a)", "Same (b)"))

})

test_that("values show as text, and value labels are matched as text or numbers", {

  expect_equal(.browse_text(c(1, 2.5, 1e6, NA)), c("1", "2.5", "1000000", NA))
  expect_equal(.browse_text(c(3L, NA)), c("3", NA))
  expect_equal(.browse_text(as.Date("2024-03-05")), "2024-03-05")
  expect_equal(.browse_text(as.POSIXct("2024-03-05 08:30:00", tz = "UTC")),
               "2024-03-05 08:30:00")
  expect_equal(.browse_text(c("a", NA)), c("a", NA))
  expect_equal(.browse_cut(c(strrep("x", 201), "short", NA)),
               c(paste0(strrep("x", 200), "\u2026"), "short", NA))

  vals <- data.frame(Code = c("01", "2", "X"), Text = c("One", "Two", "Ex"))
  expect_equal(.browse_value_labels(c("1", "2", "X", "3", NA, "2.0"), vals),
               c("One", "Two", "Ex", NA, NA, "Two"))
  expect_equal(.browse_value_labels(c("1", "2"), NULL), c(NA_character_, NA))

})

test_that("a profile gives the counts with their labels and the codes without labels", {

  summary <- data.frame(TotalRows = 150, Present = 135, DistinctValues = 5,
                        MinValue = 1, MaxValue = 10, Blank = NA)
  counts <- data.frame(ProfileValue = c(1, 2, NA, 4, 9, 10),
                       ProfileCount = c(45, 45, 15, 15, 15, 15))
  vals <- data.frame(Code = c("1", "2", "9", "10"),
                     Text = c("Male", "Female", "Not stated", "Other"))
  p <- .browse_profile(summary, counts, vals)
  expect_equal(p$summary$Measure, c("Rows", "Missing (NULL)", "Distinct values",
                                    "Minimum", "Maximum", "Value labels",
                                    "Codes without labels"))
  expect_equal(p$summary$Value, c("150", "15 (10.0%)", "5", "1 (Male)", "10 (Other)",
                                  "4", "1 code: 4"))
  expect_named(p$counts, c("Value", "Label", "Rows", "Per cent"))
  expect_equal(p$counts$Value, c("1", "2", "NA", "4", "9", "10"))
  expect_equal(p$counts$Label, c("Male", "Female", "", "(no label)", "Not stated",
                                 "Other"))
  expect_equal(p$counts$`Per cent`, c(30, 30, 10, 10, 10, 10))
  expect_equal(p$unlabelled, "4")
  expect_true(p$complete)

  # Counts that stop at 1,000; text with blanks; no value labels
  many <- data.frame(ProfileValue = sprintf("v%04d", 1:1000), ProfileCount = 1)
  p <- .browse_profile(data.frame(TotalRows = 2000, Present = 1990,
                                  DistinctValues = 1500, MinValue = "  ",
                                  MaxValue = "v1500", Blank = 12), many,
                       data.frame(Code = "v0001", Text = "First"))
  expect_false(p$complete)
  expect_equal(p$summary$Value[p$summary$Measure == "Blank text"], "12 (0.6%)")
  expect_match(p$summary$Value[p$summary$Measure == "Codes without labels"],
               "^999 codes: v0002, .* and 989 more among the 1,000 most common values$")
  expect_false("Label" %in% names(.browse_profile(summary, counts)$counts))

  # A type that cannot be grouped; a table without rows
  p <- .browse_profile(data.frame(TotalRows = 10, Present = 4, DistinctValues = NA,
                                  MinValue = NA, MaxValue = NA, Blank = NA))
  expect_null(p$counts)
  expect_equal(p$summary$Measure, c("Rows", "Missing (NULL)"))
  p <- .browse_profile(data.frame(TotalRows = 0, Present = NA, DistinctValues = 0,
                                  MinValue = NA, MaxValue = NA, Blank = NA),
                       counts[0, ])
  expect_equal(p$summary$Value[1:2], c("0", "0"))

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

test_that("the dataset label, notes and linked tables go on their own tab", {

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
  expect_equal(strip_html(as.character(
    .browse_about(.lbl_table(.lbl_normalise(NULL))$dataset))),
    "This table has no description, notes or linked tables.")

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

test_that("the code collects with dplyr and backquotes names that need it", {

  # Without labels, collect() is dplyr's: actepir does not export it
  expect_equal(
    .browse_code("Analysis", "dbo", "APC", labels = FALSE),
    paste0("data <- actepir::episerver_quickconnect(\"APC\") |>\n",
           "  dplyr::collect()\n")
  )
  expect_no_match(
    paste(vapply(c("quickconnect", "connect", "none"), function(mode) {
      .browse_code("Analysis", "dbo", "APC", mode = mode, labels = FALSE)
    }, character(1)), collapse = ""),
    "actepir::collect(", fixed = TRUE
  )

  # Names that are not syntactic are backquoted, with backquotes escaped
  columns <- c("Sex", "Age group", "if", "2nd", "a`b", "x.y")
  expect_match(.browse_code("Analysis", "dbo", "APC", columns),
               "dplyr::select(Sex, `Age group`, `if`, `2nd`, `a\\`b`, x.y)",
               fixed = TRUE)
  expect_match(.browse_code("Analysis", "dbo", "APC", columns, "connect"),
               "  select(Sex, `Age group`, `if`, `2nd`, `a\\`b`, x.y) |>",
               fixed = TRUE)

  # The none form runs on a lazy table tb; a data frame stands in for it
  tb <- data.frame(Sex = 1, "Age group" = 2, "if" = 3, "2nd" = 4, "a`b" = 5,
                   x.y = 6, Other = 7, check.names = FALSE)
  run <- new.env()
  run$tb <- tb
  eval(parse(text = .browse_code("Analysis", "dbo", "APC", columns, "none",
                                 labels = FALSE)), run)
  expect_equal(run$data, tb[columns])

})

test_that("the code names the label table chosen", {

  expect_equal(
    .browse_code("Analysis", "dbo", "APC", "Sex",
                 label_table = c("ref", "DataLabels")),
    paste0("data <- actepir::episerver_quickconnect(\"APC\") |>\n",
           "  dplyr::select(Sex) |>\n",
           "  actepir::collect_withlabels(lbl_schema = \"ref\", ",
           "lbl_table = \"DataLabels\")\n")
  )
  for (mode in c("connect", "none")) {
    expect_match(
      .browse_code("Analysis", "dbo", "APC", mode = mode, declare = FALSE,
                   label_table = c("dbo", "DataLabels")),
      "\n  collect_withlabels(lbl_schema = \"dbo\", lbl_table = \"DataLabels\")\n",
      fixed = TRUE
    )
  }
  # Without labels, collect() names no label table
  expect_equal(.browse_code("Analysis", "dbo", "APC", labels = FALSE,
                            label_table = c("ref", "DataLabels")),
               .browse_code("Analysis", "dbo", "APC", labels = FALSE))

})

# ── Page ────────────────────────────────────────────────────────────────────

test_that("the page follows the app style: top bar, sidebar sections and tabs", {

  withr::local_options(sass.cache = FALSE)
  # As in a background process, whether or not the tests run in RStudio
  local_mocked_bindings(isAvailable = function(...) FALSE,
                        .package = "rstudioapi")
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
  tabs <- vapply(c("tables", "columns", "profile", "collection", "search", "help"),
                 function(tab) {
    in_bar(paste0("data-value=\"", tab, "\""))
  }, integer(1))
  expect_false(is.unsorted(c(in_bar("Dataset Browser"), tabs,
                             in_bar("class=\"epi-logo\""))))
  expect_true(all(tabs > 0))
  # The Collection tab's title is its name alone; the search is Var Finder
  expect_match(navbar, "(?s)data-value=\"collection\"[^>]*>\\s*Collection\\s*</a>",
               perl = TRUE)
  expect_match(navbar, "(?s)data-value=\"search\"[^>]*>\\s*Var Finder\\s*</a>",
               perl = TRUE)
  # The browser opens on the Tables tab, the first
  expect_equal(sub(".*data-value=\"([a-z]+)\".*", "\\1",
                   part("<div class=\"tab-pane active\"[^>]*>")), "tables")
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
  foot <- part("(?s)<div class=\"epi-actions\">.*?id=\"bug_report\"")
  expect_match(foot, "id=\"insert_code\"[^>]*>(?s).*Copy Code", perl = TRUE)
  # Download, between them: a menu of the dictionary as PDF and HTML
  expect_match(foot, paste0("(?s)id=\"insert_code\".*<div class=\"dropup\">.*",
                            "id=\"dict_download\".*Download.*",
                            "Dictionary as PDF.*",
                            "id=\"dict_html\".*Dictionary as HTML"), perl = TRUE)
  # Value labels are shown by default
  expect_match(sidebar, "<input id=\"show_values\"[^>]* checked")
  # Above the columns' table: buttons that expand and collapse the value
  # labels and clear the rows chosen, which starts disabled, and a line on
  # the columns chosen
  columns <- part("(?s)<div class=\"tab-pane[^>]*data-value=\"columns\".*?id=\"col_table\"")
  expect_length(columns, 1)
  for (id in c("values_open", "values_close", "clear_rows", "chosen_note")) {
    expect_true(has_id(columns, id), label = id)
  }
  expect_match(columns, "<button id=\"clear_rows\" type=\"button\" disabled",
               fixed = TRUE)
  # The dataset's description and notes are on the Collection tab
  notes <- part("(?s)<div class=\"tab-pane[^>]*data-value=\"collection\".*?</div>")
  expect_true(has_id(notes, "about"))
  expect_false(has_id(columns, "about"))
  # The Profiler tab, its settings in a panel at its left, its lists drawn
  # over the page
  expect_match(navbar, "(?s)data-value=\"profile\"[^>]*>\\s*Profiler\\s*</a>",
               perl = TRUE)
  profile <- part("(?s)<div class=\"tab-pane[^>]*data-value=\"profile\".*?data-value=\"collection\"")
  expect_match(profile, "epi-tab-layout", fixed = TRUE)
  for (id in c("profile_settings", "profile_column", "profile_by", "profile_show",
               "profile_go", "profile_summary", "profile_exclude",
               "profile_exclude_save", "profile_exclude_reset", "profile_note",
               "profile_table")) {
    expect_true(has_id(profile, id), label = id)
  }
  expect_match(profile, "\"dropdownParent\":\"body\"", fixed = TRUE)
  # The Tables tab; the Var Finder tab, its settings in a panel at its left
  tables <- part("(?s)<div class=\"tab-pane[^>]*data-value=\"tables\".*?id=\"tables_table\"")
  expect_true(has_id(tables, "tables_note"))
  search <- part("(?s)<div class=\"tab-pane[^>]*data-value=\"search\".*?data-value=\"help\"")
  expect_match(search, "epi-tab-layout", fixed = TRUE)
  for (id in c("search_settings", "search_text", "search_in", "search_go",
               "search_note", "search_table")) {
    expect_true(has_id(search, id), label = id)
  }
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
    expect_match(output$about$html, "This table has no description", fixed = TRUE)

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

test_that("the line above the table counts the columns chosen, and Clear selection clears them", {

  cleared <- list()
  local_mocked_bindings(selectRows = function(proxy, selected, ...) {
    cleared[[length(cleared) + 1]] <<- list(id = proxy$id, selected = selected)
  }, .package = "DT")
  app <- episerver_browse_app(query = fake_browse_episerver()$query)

  shiny::testServer(app, {
    session$setInputs(db = "Analysis", schema = "dbo", labels_source = "auto",
                      table = "APC")
    expect_equal(output$chosen_note,
                 "No columns chosen: the code reads them all.")
    session$setInputs(col_table_rows_selected = 2)
    expect_equal(output$chosen_note, "1 column chosen for the code.")
    session$setInputs(col_table_rows_selected = c(2, 3))
    expect_equal(output$chosen_note, "2 columns chosen for the code.")

    session$setInputs(clear_rows = 1)
    expect_length(cleared, 1)
    expect_match(cleared[[1]]$id, "col_table$")
    expect_null(cleared[[1]]$selected)
  })

})

test_that("the Tables tab lists the schema's tables, and a click chooses one", {

  sent <- list()
  capture <- function(session, inputId, label = NULL, choices = NULL,
                      selected = NULL, ...) {
    sent[[inputId]] <<- list(choices = choices, selected = selected)
  }
  local_mocked_bindings(updateSelectInput = capture,
                        updateSelectizeInput = capture, .package = "shiny")
  marked <- NULL
  local_mocked_bindings(selectRows = function(proxy, selected, ...) {
    marked <<- selected
  }, .package = "DT")
  app <- episerver_browse_app(query = fake_browse_episerver()$query)

  # testServer() computes every output, shown or not, so that the tab's
  # queries wait until it is shown rests on Shiny suspending the outputs of
  # a hidden tab; only the marking of the table chosen is held back here
  shiny::testServer(app, {
    session$setInputs(db = "Analysis", schema = "dbo", labels_source = "auto",
                      show_values = FALSE, table = "APC", view = "columns")
    expect_null(marked)
    session$setInputs(view = "tables")
    expect_equal(output$tables_note,
                 "7 tables and views in Analysis.dbo. Click one to choose it.")
    expect_equal(overview()$table$`Labels from`[2], "ref.DataProperties")
    expect_match(output$tables_table, "Structure changed", fixed = TRUE)
    # The table chosen is marked; a click on another chooses it
    expect_equal(marked, 2L)
    session$setInputs(tables_table_rows_selected = 5L)
    expect_equal(sent$table$selected, "ED")
    session$setInputs(table = "ED")
    expect_equal(marked, 2L)
    # A table chosen in the sidebar is marked
    session$setInputs(table = "Deaths")
    expect_equal(marked, 4L)
  })

  # A query that fails is reported, and the rest is shown
  app <- episerver_browse_app(query = fake_browse_episerver(fail = "AS ColumnKey")$query)
  shiny::testServer(app, {
    session$setInputs(db = "Analysis", schema = "dbo", labels_source = "auto",
                      show_values = FALSE, table = "APC", view = "tables")
    expect_match(output$tables_note, "The label tables could not be read: Fake failure",
                 fixed = TRUE)
    expect_equal(overview()$table$Rows[2], 1234567)
  })

})

test_that("the Var Finder tab finds columns, and a click opens the column's table", {

  sent <- list()
  capture <- function(session, inputId, label = NULL, choices = NULL,
                      selected = NULL, ...) {
    sent[[inputId]] <<- list(choices = choices, selected = selected)
  }
  local_mocked_bindings(updateSelectInput = capture,
                        updateSelectizeInput = capture, .package = "shiny")
  tab <- NULL
  local_mocked_bindings(nav_select = function(id, selected, ...) {
    tab <<- c(id, selected)
  }, .package = "bslib")
  warned <- NULL
  local_mocked_bindings(showNotification = function(ui, ...) warned <<- ui,
                        .package = "shiny")
  app <- episerver_browse_app(query = fake_browse_episerver()$query)

  shiny::testServer(app, {
    session$setInputs(db = "Analysis", schema = "dbo", labels_source = "auto",
                      show_values = FALSE, table = "APC",
                      search_in = c("names", "labels", "values"))
    expect_match(output$search_note, "Finds the columns of Analysis, in every schema",
                 fixed = TRUE)
    session$setInputs(search_run = "  ")
    expect_match(warned, "Type a name", fixed = TRUE)
    expect_null(found())

    session$setInputs(search_run = "sex")
    expect_equal(output$search_note, paste(
      "3 columns of Analysis match \"sex\". Click a column to open its table."
    ))
    expect_match(output$search_table, "Variable label", fixed = TRUE)
    expect_equal(found()$results$`Variable label`[2], "Sex of the patient")
    session$setInputs(search_run = "female")
    expect_equal(found()$term, "female")
    expect_equal(found()$results$Table, c("ACTGHSMYX", "APC"))

    # A column in another schema: the schema, then the table once its list
    # arrives, on the Columns tab
    session$setInputs(search_run = "staged")
    session$setInputs(search_table_rows_selected = 1L)
    expect_equal(sent$schema$selected, "stage")
    expect_equal(tab, c("view", "columns"))
    session$setInputs(schema = "stage")
    expect_equal(sent$table$selected, "APC")
    expect_null(wanted())

    # Results belong to one database
    session$setInputs(db = "HospitalAPC")
    expect_null(found())
    session$setInputs(search_in = character(), search_run = "sex")
    expect_match(warned, "Choose where to look.", fixed = TRUE)
  })

})

test_that("the Profiler profiles the column chosen, only when asked, and leaves out identifiers", {

  withr::local_envvar(R_USER_CONFIG_DIR = withr::local_tempdir())
  sent <- list()
  capture <- function(session, inputId, label = NULL, choices = NULL,
                      selected = NULL, value = NULL, ...) {
    sent[[inputId]] <<- list(choices = choices, selected = selected, value = value)
  }
  local_mocked_bindings(updateSelectizeInput = capture,
                        updateTextAreaInput = capture, .package = "shiny")
  warned <- NULL
  local_mocked_bindings(showNotification = function(ui, ...) warned <<- ui,
                        .package = "shiny")
  fake <- fake_browse_episerver()
  reads <- function(what) sum(grepl(what, fake$calls(), fixed = TRUE))
  app <- episerver_browse_app(query = fake$query)

  shiny::testServer(app, {
    # The list kept is shown in its box
    expect_equal(sent$profile_exclude$value,
                 paste(.browse_exclude_default, collapse = "\n"))
    session$setInputs(db = "Analysis", schema = "dbo", labels_source = "auto",
                      show_values = FALSE, table = "APC", view = "columns")
    # Nothing is read for the tab before it is shown
    expect_equal(reads("sys.indexes"), 0)
    session$setInputs(view = "profile")
    expect_equal(reads("sys.indexes"), 1)
    # PatientID, the primary key, is left out
    expect_equal(sent$profile_column$choices[1:2], c("Sex", "AgeYrs"))
    expect_equal(sent$profile_column$selected, "Sex")
    expect_equal(sent$profile_by$choices[1:3], c(None = "", Sex = "Sex", AgeYrs = "AgeYrs"))
    expect_equal(output$profile_left_out, "Columns left out (1)")
    expect_match(output$profile_exclude_note$html, "Left out: PatientID (key).", fixed = TRUE)
    expect_match(output$profile_note, "Choose a column", fixed = TRUE)
    expect_equal(reads("AS TotalRows"), 0)

    # The first column chosen in the Columns tab is offered
    session$setInputs(col_table_rows_selected = 3L)
    expect_equal(sent$profile_column$selected, "AgeYrs")
    session$setInputs(profile_column = "Sex", profile_by = "", profile_go = 1)
    expect_equal(reads("AS TotalRows"), 1)
    expect_equal(output$profile_note,
                 "Profile of Analysis.dbo.APC.Sex (Sex of the patient).")
    expect_match(output$profile_summary$html, "Codes without labels", fixed = TRUE)
    expect_equal(profile()$counts$Label[4], "(no label)")
    expect_match(output$profile_table, "Per cent", fixed = TRUE)

    # By: a cross tab, shown as rows or per cents with no new query
    session$setInputs(profile_by = "AgeYrs", profile_show = "rows", profile_go = 2)
    expect_equal(reads("AS ProfileBy"), 1)
    expect_match(output$profile_note, "by AgeYrs (Age in years (<1 is 0)).", fixed = TRUE)
    expect_match(output$profile_note, "The 20 most common values of AgeYrs are columns",
                 fixed = TRUE)
    expect_equal(crosstab()$table$Total[nrow(crosstab()$table)], 150)
    session$setInputs(profile_show = "row")
    expect_equal(unique(crosstab()$table$Total), 100)
    expect_equal(reads("AS ProfileBy"), 1)
    # By cannot be the column profiled
    session$setInputs(profile_by = "Sex", profile_go = 3)
    expect_match(warned, "Choose a By column other than", fixed = TRUE)

    # The list: a pattern that is not a regular expression is not saved; a
    # good one is used and kept; the defaults come back
    session$setInputs(profile_exclude = "ID$\n[unclosed", profile_exclude_save = 1)
    expect_match(output$profile_exclude_note$html, "[unclosed", fixed = TRUE)
    expect_false(file.exists(.browse_exclude_file()))
    session$setInputs(profile_exclude = "^Age", profile_exclude_save = 2)
    expect_equal(readLines(.browse_exclude_file()), "^Age")
    expect_equal(output$profile_left_out, "Columns left out (2)")
    expect_false("AgeYrs" %in% sent$profile_column$choices)
    session$setInputs(profile_exclude_reset = 1)
    expect_false(file.exists(.browse_exclude_file()))
    expect_equal(sent$profile_exclude$value,
                 paste(.browse_exclude_default, collapse = "\n"))
    expect_equal(output$profile_left_out, "Columns left out (1)")

    # Text: blank values; a profile of another table's column is not shown
    session$setInputs(table = "Deaths", profile_by = "")
    expect_null(profile())
    session$setInputs(profile_column = "Cause", profile_go = 4)
    expect_match(output$profile_summary$html, "Blank text", fixed = TRUE)
    expect_true(profile()$complete)
    expect_equal(profile()$counts$Rows, c(8, 8, 7, 7))
  })

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
    # A label table chosen is named in the code
    session$setInputs(labels_source = "ref.DataLabels")
    session$setInputs(insert_code = 4)
  })

  expect_equal(sent, c(
    .browse_code("Analysis", "dbo", "APC", c("AgeYrs", "Sex"), "connect",
                 declare = TRUE, labels = FALSE),
    .browse_code("Analysis", "stage", "APC", character(), "quickconnect"),
    .browse_code("Analysis", "stage", "APC", character(), "quickconnect",
                 label_table = c("ref", "DataLabels"))
  ))

})

test_that("the code button inserts the code when the app runs in RStudio", {

  withr::local_options(sass.cache = FALSE)
  local_mocked_bindings(isAvailable = function(...) TRUE,
                        .package = "rstudioapi")
  app  <- episerver_browse_app(query = fake_browse_episerver()$query)
  page <- app$httpHandler(list(PATH_INFO = "/", REQUEST_METHOD = "GET",
                               QUERY_STRING = ""))$content
  foot <- regmatches(page, regexpr("(?s)<div class=\"epi-actions\">.*?</div>",
                                   page, perl = TRUE))
  expect_match(foot, "id=\"insert_code\"[^>]*>(?s).*Insert Code", perl = TRUE)
  expect_no_match(foot, "Copy Code", fixed = TRUE)

})

test_that("the bug button opens a report that holds the settings but no labels", {

  # As in a background process, whether or not the tests run in RStudio
  local_mocked_bindings(isAvailable = function(...) FALSE,
                        .package = "rstudioapi")
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
