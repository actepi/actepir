# tests/testthat/test-episerver_details.R

library(testthat)
library(actepir)

test_that("episerver details can be found", {

  result = episerver_serverdetails("server")
  expect_type(result, "character")
  expect_true(nchar(result) > 0)

  result = episerver_serverdetails("port")
  expect_type(result, "character")
  expect_true(nchar(result) > 0)

})

# The driver is picked from odbcListDrivers(), so it is a property of the
# machine rather than of the package and is NA where no SQL Server driver is
# installed.
test_that("episerver driver can be found", {

  skip_if_no_sqlserver_driver()

  result = episerver_serverdetails("driver")
  expect_type(result, "character")
  expect_true(nchar(result) > 0)

})