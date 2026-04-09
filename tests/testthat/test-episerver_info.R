# tests/testthat/test-episerver_info.R

library(testthat)
library(actepir)

# ── Table listing mode ──────────────────────────────────────────────────────

test_that("episerver_info returns table list for default namespace", {

  result <- episerver_info()

  expect_s3_class(result, "data.frame")
  expect_true(all(c("table_catalog", "table_schema", "table_name", "table_type") %in% colnames(result)))
  expect_true(nrow(result) > 0)
  expect_true(all(result$table_schema == "dbo"))
  expect_true(all(result$table_catalog == "Analysis"))

})

test_that("episerver_info returns table list for ref schema", {

  result <- episerver_info(schema = "ref")

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true(all(result$table_schema == "ref"))

  # DataLabels should exist in the ref schema
  expect_true("DataLabels" %in% result$table_name)

})

# ── Column metadata mode ────────────────────────────────────────────────────

test_that("episerver_info returns column metadata for known table", {

  # Use a table known to exist from the labels test
  result <- episerver_info(dataset = "ACTGHSMYX")

  expect_true(nrow(result) > 0)
  expect_true(all(result$table_name == "ACTGHSMYX"))

  # ordinal_position should be sequential from 1
  expect_equal(result$Position, seq_len(nrow(result)))

})

test_that("episerver_info returns column metadata for DataLabels in ref schema", {

  result <- episerver_info(schema = "ref", dataset = "DataLabels")

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)

  # DataLabels should contain the columns used elsewhere in actepir
  expected_cols <- c("Dataset", "VarName", "LabelType", "LabelName", "DataCode", "LabelName")
  actual_cols <- result$VarName
  expect_true(all(expected_cols %in% actual_cols))

})

# ── Edge cases ──────────────────────────────────────────────────────────────

test_that("episerver_info warns on nonexistent table", {

  expect_warning(
    result <- episerver_info(dataset = "ThisTableDoesNotExist_XYZ999"),
    regexp = "No columns found"
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)

})

test_that("episerver_info warns on nonexistent schema", {

  expect_warning(
    result <- episerver_info(schema = "nonexistent_schema_xyz"),
    regexp = "No tables found"
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)

})
