# tests/testthat/test-episerver_details.R

library(testthat)
library(actepir)

test_that("episerver details can be found", {
  
  result = episerver_serverdetails("server")
  expect_type(result, "character")
  expect_true(nchar(result) > 0)
  
  result = episerver_serverdetails("driver")
  expect_type(result, "character")
  expect_true(nchar(result) > 0)
  
  result = episerver_serverdetails("port")
  expect_type(result, "character")
  expect_true(nchar(result) > 0)
  
})