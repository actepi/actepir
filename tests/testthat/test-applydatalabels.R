# tests/testthat/test-applydatalabels.R
#
# Offline tests for applydatalabels(), on labels in the layout that
# episerver_getlabels() returns.

library(testthat)
library(actepir)

labels <- tibble::tibble(
  Dataset   = "T",
  VarName   = c("Sex", "Sex", "Sex", "Sex", "Region", "Region", "Code",
                "Code", "Age", "Missing"),
  LabelType = c("Var", "Opt", "opt", "Opt", "OPT", "Opt", "Var", "Opt",
                "VAR", "Var"),
  DataCode  = c(NA, "1", "2", "1", "1", "2", NA, "A", NA, NA),
  LabelName = c("Sex of patient", "Male", "Female", "A second label of 1",
                "North", "South", "A code", "Alpha", "Age in years",
                "Not in the data")
)

test_that("variable and value labels are applied, the first of each kept", {

  data <- data.frame(Sex = c(1, 2), Code = c("A", "B"), Age = c(30, 40),
                     Other = 1:2)
  out <- applydatalabels(data, labels)

  expect_s3_class(out$Sex, "haven_labelled")
  expect_equal(attr(out$Sex, "labels"), c(Male = 1, Female = 2))
  expect_equal(attr(out$Sex, "label"), "Sex of patient")
  # A character column takes labels on its codes
  expect_equal(attr(out$Code, "labels"), c(Alpha = "A"))
  expect_equal(attr(out$Code, "label"), "A code")
  # A variable label alone; a column without labels is untouched
  expect_equal(attr(out$Age, "label"), "Age in years")
  expect_false(inherits(out$Age, "haven_labelled"))
  expect_identical(out$Other, data$Other)
  expect_false("Missing" %in% names(out))

})

test_that("value labels are applied to a column without a variable label", {

  out <- applydatalabels(data.frame(Region = c(1, 2)), labels)
  expect_s3_class(out$Region, "haven_labelled")
  expect_equal(attr(out$Region, "labels"), c(North = 1, South = 2))
  expect_null(attr(out$Region, "label", exact = TRUE))

})

test_that("variable names match the data's column names in any case", {

  data <- data.frame(SEX = c(1, 2), code = c("A", "B"))
  out <- applydatalabels(data, labels)
  expect_named(out, c("SEX", "code"))
  expect_equal(attr(out$SEX, "labels"), c(Male = 1, Female = 2))
  expect_equal(attr(out$SEX, "label"), "Sex of patient")
  expect_equal(attr(out$code, "labels"), c(Alpha = "A"))

  # Rows for one column and code in different cases count as one
  mixed <- tibble::tibble(VarName   = c("Sex", "SEX", "sex"),
                          LabelType = c("Var", "Opt", "opt"),
                          DataCode  = c(NA, "1", "1"),
                          LabelName = c("Sex", "Male", "A second label of 1"))
  out <- applydatalabels(data.frame(Sex = 1), mixed)
  expect_equal(attr(out$Sex, "labels"), c(Male = 1))
  expect_equal(attr(out$Sex, "label"), "Sex")

})

test_that("no labels leave the data as they were", {

  data <- data.frame(Sex = c(1, 2))
  expect_identical(applydatalabels(data, labels[0, ]), data)

})
