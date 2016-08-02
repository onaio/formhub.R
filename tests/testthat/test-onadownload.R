context('')
library(testthat)

#test_dir("~/onaio/ona.R/tests/")

test_that("downloading public data (with public form) works", {
  good_eats <- onaDownload("good_eats",account="mberg", uname="mberg")
  expect_true("risk_factor" %in% names(good_eats))
  expect_true("food_type" %in% names(good_eats))
  expect_true(nrow(good_eats) > 1)
  
  # public data with a password also works, no matter what the password
  good_eats <- onaDownload("good_eats", account="mberg", uname="mberg", pass="boguspassword")
  expect_true("risk_factor" %in% names(good_eats))
  expect_true(nrow(good_eats) > 1)
  
})

test_that("downloading private data works", {
  data <- onaDownload("good_eats", account="ona_r", uname="ona_r", 
                        pass="ona_r")
  expect_true("submit_date" %in% names(data))
  expect_true(nrow(data) > 0)
  expect_true(is.character(data$description))
  expect_true(is.factor(data$risk_factor))
})


# test_that("downloading public data (with private form) falls back gracefully", {
#   data <- onaDownload("Public_Data_Private_Schema",account="ona_r", uname="ona_r")
#   expect_true(nrow(data) > 0)
#   expect_true(is.instant(data$submit_date))
#   expect_true(is.factor(data$functional))
#   expect_true(is.character(data$wp_id))
# })
