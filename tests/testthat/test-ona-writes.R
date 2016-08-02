library(testthat)
library(stringr)
library(ona)

test_dir = "fixtures/"
#test_dir("~/onaio/ona.R/tests/")


edu_datafile <- str_c(test_dir, "edu1.csv")
edu_formfile <- str_c(test_dir, "edu1.json")


good_eats_datafile <- str_c(test_dir, "good_eats.csv")
good_eats_formfile <- str_c(test_dir, "good_eats.json")


# Spatial Point Data Frame Object
test_that("SpatialPointsDataFrame Object can be produced", {
  edu_onaObj <- onaRead(edu_datafile, edu_formfile)
  good_eats <- onaRead(good_eats_datafile, good_eats_formfile)
  
  edu_spdf <- as.SpatialPointsDataFrame(edu_onaObj)
  expect_is(edu_spdf, "SpatialPointsDataFrame")
  expect_equal(nrow(edu_spdf), nrow(as.data.frame(edu_onaObj)))
  
  # good eats: WARNINGS should be emitted; we have NAs in the gps field; we'll drop 3 rows
  expect_warning(good_eats_spdf <- as.SpatialPointsDataFrame(good_eats))
  expect_equal(nrow(as.data.frame(good_eats_spdf)), length(which(!is.na(good_eats$gps))))
  expect_is(good_eats_spdf, "SpatialPointsDataFrame")
})

 test_that("SpatialPointsDataFrame Object can be produced even when _lat _long columns missing", {
  edu_onaObj <- onaRead(edu_datafile, edu_formfile, dropCols="X_gps*")
          
  edu_spdf <- as.SpatialPointsDataFrame(edu_onaObj)
  expect_is(edu_spdf, "SpatialPointsDataFrame")
  expect_equal(nrow(edu_spdf), nrow(as.data.frame(edu_onaObj)))
  
  })


test_that("SpatialPointDataFrame Objec conversion returns NA when gps field missing", {
  edu_onaObj <- onaRead(edu_datafile, edu_formfile, dropCols="gps")
  good_eats <- onaRead(good_eats_datafile, good_eats_formfile, dropCols="gps")
  
  expect_true(is.na(as.SpatialPointsDataFrame(edu_onaObj)))
  expect_true(is.na(as.SpatialPointsDataFrame(good_eats)))
})
