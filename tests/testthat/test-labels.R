# tests/testthat/test-labels.R

library(testthat)
library(actepir)

test_that("load episerver data with labels", {
  
  # multistep connection process
  con = episerver_connect()
  table = episerver_quickconnect("ACTGHSMYX")
  result = table |>
    dplyr::filter(SVYYEAR==2023) |>
    dplyr::group_by(SVYYEAR,RSEX_Y20) |>
      dplyr::summarise(count=n()) |>
    dplyr::ungroup() |>
    collect_withlabels() 
  
  expect_equal(nrow(result), 4)
  expect_equal(ncol(result), 3)
  expect_equal(sum(result$count), 2200)
  
  expect_true(all(c("Male", "Female") %in% sjlabelled::as_label(result$RSEX_Y20) ))
  
  
})
