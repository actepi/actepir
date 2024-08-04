# tests/testthat/test-proxy.R

library(testthat)
library(actepir)

test_that("proxy toggle switches on", {
  
  proxy_actgov_toggle("on")
  result = Sys.getenv(c("http_proxy","https_proxy","http_proxy_user","https_proxy_user"))
  for (i in 1:length(result)) { 
    expect_type(result[i], "character")
    expect_true(substr(result[i], nchar(result[i])-4+1, nchar(result[i])) == "9090")
    expect_true(grepl("act.gov.au", result[i], fixed = TRUE))
    expect_true(nchar(result[i]) > 0)
  }
  
})

test_that("proxy toggle switches off", {
  
  proxy_actgov_toggle("off")
  result = Sys.getenv(c("http_proxy","https_proxy","http_proxy_user","https_proxy_user"))
  for (i in 1:length(result)) { 
    expect_type(result[i], "character")
    expect_true(nchar(result[i]) == 0)
  }
  
})