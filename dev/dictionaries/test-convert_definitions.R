# dev/dictionaries/test-convert_definitions.R
#
# Offline tests for convert_definitions.R, on a made-up workbook. Run from the
# root of the repository with
#   Rscript -e 'testthat::test_file("dev/dictionaries/test-convert_definitions.R")'
# testthat::test_local() does not run them, as they are not in tests/.

library(testthat)
here <- if (file.exists("convert_definitions.R")) "." else "dev/dictionaries"
source(file.path(here, "convert_definitions.R"))

# A workbook in the layout of definitions.xlsx, and a run script's tables
make_workbook <- function(dir, vars = NULL, notes = NULL, limits = NULL) {
  if (is.null(vars)) {
    vars <- data.frame(
      Dataset = c("APC", "APC", "APC", "APC", "APC", "APC", "Retired"),
      Theme = c("Record identifiers", " Demography ", "Diagnoses/Procedures",
                "Diagnoses/Procedures", "Diagnoses/Procedures",
                "Diagnoses/Procedures", NA),
      Column = c("APCEpisodeNumber", "Sex", "Diagnosis1", "Diagnosis2",
                 "Diagnosis3", "Diagnosis4", "Old"),
      ColumnAlias = c(NA, NA, "Diagnosis1", "Diagnosis2-4", "Diagnosis2-4",
                      "Diagnosis2-4", NA),
      DescriptionAlias = c(NA, NA, "Primary diagnosis", "Secondary diagnoses",
                           "Secondary diagnoses", "Secondary diagnoses", NA),
      ColumnAction = c("INCLUDE", NA, "include", "INCLUDE", "INCLUDE",
                       "INCLUDE", "EXCLUDE"),
      ValueAction = c(NA, "suppress", NA, NA, NA, NA, NA),
      Limits = c(NA, "ATSI", NA, NA, NA, NA, NA),
      Comments = c(NA, "It's internal", NA, NA, NA, NA, NA),
      Other = c(NA, "Sex \u2013 as recorded", NA, NA, NA, NA, NA),
      stringsAsFactors = FALSE
    )
  }
  if (is.null(notes)) {
    notes <- data.frame(
      Dataset = c("APC", "APC", "Retired"),
      Aspect = c("Dataset Description", "Links & Resources", "Dataset Description"),
      Issue = c(NA, "METeOR \u2013 AIHW", NA),
      Comment = c("Line one\nLine two ", "https://meteor.aihw.gov.au/", "Old"),
      stringsAsFactors = FALSE
    )
  }
  if (is.null(limits)) {
    limits <- data.frame(code = "atsi", hex = "#f36c23", symbol = "\u25c6",
                         text = "Availability may need ethics approval",
                         stringsAsFactors = FALSE)
  }
  path <- file.path(dir, "definitions.xlsx")
  openxlsx::write.xlsx(list(vars = vars, notes = notes, limits = limits), path)
  path
}
tables <- data.frame(
  active = 1, fullname = c("Admitted Patient Care", "Emergency Department"),
  abbrname = c("APC", "ED"), doi = c("10.9999/exam/ple999", "10.1000/xyz"),
  version = c(5, 2), db = "Analysis", dat.sc = "dbo", dat.tb = c("APC", "ED"),
  stringsAsFactors = FALSE
)

test_that("the workbook's rows become the tables' rows, spellings standardised", {

  dir <- withr::local_tempdir()
  x <- definition_rows(read_definitions(make_workbook(dir)), tables,
                       theme_order = c("Record identifiers", "Demography",
                                       "Unused"))
  r <- x$rows

  expect_equal(r$DictionaryProfiles$Title, "Data Linkage Dataset Metadata")
  expect_equal(r$DictionarySections$Heading,
               c("General Enquiries", "Acknowledgement of Country",
                 "Accessibility"))
  expect_equal(r$DictionaryCaveats$Colour, "#F36C23")
  expect_equal(r$DictionaryCaveats$Symbol, "\u25c6")
  expect_equal(r$DictionaryThemes$ThemeOrder, c(10L, 20L, 30L))

  # The placeholder DOI is not written
  expect_equal(r$DictionaryDatasets$DatasetCode, c("APC", "ED"))
  expect_equal(r$DictionaryDatasets$DOI, c(NA, "10.1000/xyz"))
  expect_identical(r$DictionaryDatasets$Version, c(5L, 2L))

  cols <- r$DictionaryColumns
  expect_equal(cols$ColumnName, c("APCEpisodeNumber", "Sex", "Diagnosis1",
                                  "Diagnosis2", "Diagnosis3", "Diagnosis4"))
  sex <- cols[cols$ColumnName == "Sex", ]
  expect_equal(sex$Theme, "Demography")
  expect_equal(sex$ColumnAction, "INCLUDE")
  expect_equal(sex$ValueAction, "SUPPRESS")
  expect_equal(sex$CaveatCode, "atsi")
  expect_equal(unique(cols$ColumnAction), "INCLUDE")
  # The workbook's internal columns are not carried over
  expect_named(cols, c("Profile", "DatasetCode", "ColumnName", "Theme",
                       "ColumnAlias", "DescriptionAlias", "ColumnAction",
                       "ValueAction", "CaveatCode"))

  notes <- r$DictionaryNotes
  expect_equal(notes$NoteOrder, 1:2)
  expect_equal(notes$Comment[1], "Line one\nLine two")

  # Rows of a dataset not in tables are left out, and reported
  p <- x$problems
  expect_false(any(p$kind == "error"))
  expect_match(p$message[p$kind == "left out"],
               "Dataset Retired|ED \\(ED\\) has no rows|not in the order",
               all = FALSE)
  expect_equal(sum(grepl("Retired", p$message)), 2)
  expect_true(any(grepl("ColumnAction (blank) written as INCLUDE in 1 rows",
                        p$message, fixed = TRUE)))
  expect_true(any(grepl("ValueAction suppress written as SUPPRESS",
                        p$message, fixed = TRUE)))
  expect_true(any(grepl("themes in the order that no column has: Unused",
                        p$message, fixed = TRUE)))

})

test_that("problems that would give wrong rows stop the conversion", {

  dir <- withr::local_tempdir()
  vars <- data.frame(
    Dataset = "APC",
    Theme = c("A", "A", "A", "B", "A", "A"),
    Column = c("Sex", "sex", "Diagnosis2", "Diagnosis3", "Diagnosis4", "Age"),
    ColumnAlias = c(NA, NA, "Diagnosis2-5", "Diagnosis2-5", "Diagnosis2-5", NA),
    DescriptionAlias = c(strrep("x", 501), NA, NA, NA, NA, NA),
    ColumnAction = c("MAYBE", NA, NA, NA, NA, NA),
    ValueAction = c(NA, NA, NA, NA, NA, "hide"),
    Limits = c(NA, NA, NA, NA, NA, "xyz"),
    Comments = NA, Other = NA, stringsAsFactors = FALSE
  )
  path <- make_workbook(dir, vars = vars)
  p <- definition_rows(read_definitions(path), tables)$problems
  errors <- p$message[p$kind == "error"]

  expect_true(any(grepl("ColumnAction MAYBE is not INCLUDE or EXCLUDE", errors,
                        fixed = TRUE)))
  expect_true(any(grepl("ValueAction hide is not Suppress", errors, fixed = TRUE)))
  expect_true(any(grepl("Limits xyz is not a code", errors, fixed = TRUE)))
  expect_true(any(grepl("Column sex of APC appears twice", errors, fixed = TRUE)))
  expect_true(any(grepl("DescriptionAlias is longer than 500", errors, fixed = TRUE)))
  expect_true(any(grepl("its columns differ in Theme", errors, fixed = TRUE)))
  expect_true(any(grepl(paste("the alias numbers 2 to 5, but its columns are",
                              "numbered 2 to 4"), errors, fixed = TRUE)))
  # The duplicate is reported at its row of the workbook
  expect_equal(p$rows[grepl("appears twice", p$message)], "3")

  out <- file.path(dir, "out.sql")
  expect_error(suppressMessages(convert_definitions(path, tables, out)),
               "Nothing was written")
  expect_false(file.exists(out))

  # A workbook without a sheet or a column is refused
  openxlsx::write.xlsx(list(vars = vars), file.path(dir, "short.xlsx"))
  expect_error(read_definitions(file.path(dir, "short.xlsx")),
               "has no sheet notes, limits")
  openxlsx::write.xlsx(list(vars = vars[-1], notes = data.frame(a = 1),
                            limits = data.frame(a = 1)),
                       file.path(dir, "narrow.xlsx"))
  expect_error(read_definitions(file.path(dir, "narrow.xlsx")),
               "Sheet vars has no column Dataset")

})

test_that("text is written as plain ASCII literals", {

  expect_equal(sql_value(c("It's", NA, "a\nb", "\u2013 x", "")),
               c("N'It''s'", "NULL", "N'a' + NCHAR(10) + N'b'",
                 "NCHAR(8211) + N' x'", "N''"))
  expect_equal(sql_value(c(5L, NA)), c("5", "NULL"))
  expect_equal(sql_value(c(TRUE, FALSE)), c("1", "0"))
  # Joined pieces longer than 4,000 characters are kept whole
  expect_match(sql_value(paste0(strrep("a", 4001), "\n")),
               "^CAST\\(N'' AS nvarchar\\(max\\)\\) \\+ N'a")
  expect_error(sql_value("\U0001F600"), "NCHAR\\(\\) cannot write")

  rows <- data.frame(Profile = "Linkage", N = seq_len(1001))
  inserts <- sql_insert("DictionaryThemes", rows)
  expect_length(inserts, 3)
  expect_match(inserts[1], "^INSERT INTO ref.DictionaryThemes \\(Profile, N\\)\nVALUES\n")
  expect_equal(lengths(regmatches(inserts, gregexpr("\\(N'Linkage'", inserts))),
               c(500, 500, 1))

})

test_that("the script loads the profile in one transaction, and only once", {

  dir <- withr::local_tempdir()
  out <- file.path(dir, "linkage.sql")
  suppressMessages(convert_definitions(make_workbook(dir), tables, out))
  script <- readLines(out)

  expect_false(any(grepl("[^ -~]", script)))
  text <- paste(script, collapse = "\n")
  expect_match(text, paste0("IF EXISTS (SELECT 1 FROM ref.DictionaryProfiles ",
                            "WHERE Profile = N'Linkage')\n    THROW 50000"),
               fixed = TRUE)
  expect_match(text, "BEGIN TRANSACTION;\n\nINSERT INTO ref.DictionaryProfiles",
               fixed = TRUE)
  expect_match(text, "COMMIT TRANSACTION;", fixed = TRUE)
  # The order of inserts follows the foreign keys; deletes go the other way
  tables_in <- regmatches(text, gregexpr("INSERT INTO ref\\.[A-Za-z]+", text))[[1]]
  expect_equal(unique(sub("INSERT INTO ref.", "", tables_in, fixed = TRUE)),
               c("DictionaryProfiles", "DictionarySections", "DictionaryCaveats",
                 "DictionaryThemes", "DictionaryDatasets", "DictionaryColumns",
                 "DictionaryNotes"))
  expect_match(text, paste0("--   DELETE FROM ref.DictionaryNotes WHERE Profile = ",
                            "N'Linkage';\n--   DELETE FROM ref.DictionaryColumns"),
               fixed = TRUE)
  expect_match(text, "(N'DictionaryColumns', 6)", fixed = TRUE)
  expect_match(text, "N'METeOR ' + NCHAR(8211) + N' AIHW'", fixed = TRUE)
  expect_no_match(text, "It''s internal", fixed = TRUE)

})
