# tests/testthat/test-labels.R

library(testthat)
library(actepir)

df <- data.frame(
  ID = 1:5,
  gender = c(1, 2, 1, 2, 1),
  status = c(1, 1, 2, 2, 1),
  anothr = c(3, 4, 2, 4, 3),
  nolabs = c(3, 1, 2, 4, 2)
  ) %>% 
  
  # Applying value/variable labels to multiple columns using the labelled package
  labelled::set_value_labels(
    status = c(Single = 1, Married = 2),
    gender = c(Male = 1, Female = 2),
    anothr = c(Thing = 2, `An other` = 3, Bit = 4)
  ) %>% 
  labelled::set_variable_labels(
    gender = "Gender of the respondent",
    status = "Marital status",
    anothr = "Another column"
  ) %>% 
  
  # Mutate a recode and set variable and value labels to a single column in same mutation
  dplyr::mutate(
    single = dplyr::case_when(status==1~1,TRUE~0) %>% 
      labelled::set_value_labels(c(Single=1,`Not single`=0)) %>% 
      labelled::set_variable_labels("Single Status")
  )


test_that("full data dictionary creation", {
  
  expect_error(create_dictionary(df), regexp = NA)
  result = create_dictionary(df)
  
  expect_s3_class(result, "data.frame")
  expect_true(all(c("variable", "variable_label", "value_label", "value") %in% colnames(result)))
  expect_equal(nrow(result), 11)
  expect_equal(ncol(result), 4)
  
})

test_that("partial data dictionary creation", {
  
  expect_error(create_dictionary(df,c("gender","status")), regexp = NA)
  result = create_dictionary(df,c("gender","status"))
  
  expect_s3_class(result, "data.frame")
  expect_true(all(c("variable", "variable_label", "value_label", "value") %in% colnames(result)))
  expect_equal(nrow(result), 4)
  expect_equal(ncol(result), 4)
  
})