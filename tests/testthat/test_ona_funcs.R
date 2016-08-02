require(testthat)
require(stringr)
require(ona)

test_dir = "fixtures/"
#test_dir("~/onaio/ona.R/tests/")

edu_datafile <- str_c(test_dir, "edu1.csv")
edu_formfile <- str_c(test_dir, "edu1.json")
good_eats_datafile <- str_c(test_dir, "good_eats.csv")
good_eats_formfile <- str_c(test_dir, "good_eats.json")
pde_datafile <- str_c(test_dir, "pde.csv")
pde_formfile <- str_c(test_dir, "pde.json")


# Header Names with Labels works
test_that("replaceHeaderNamesWithLabels basically works", {
  edu_onaObj <- onaRead(edu_datafile, edu_formfile)
  good_eats <- onaRead(good_eats_datafile, good_eats_formfile)
  
  edu_onaObj_replaced <- replaceHeaderNamesWithLabels(edu_onaObj)
  good_eats_replaced <- replaceHeaderNamesWithLabels(good_eats)
  
  expect_equal(class(edu_onaObj_replaced), "data.frame")
  expect_equal(class(good_eats_replaced), "data.frame")
  
  expect_false(any(names(edu_onaObj_replaced) == "NA"))
  expect_false(any(names(good_eats_replaced) == "NA"))
  
  expect_true(all(c("submit_date", "imei", "X_gps_longitude","Rating","Type of Eat", "Food Pic")
              %in% names(good_eats_replaced)))
  expect_true(all(c("subscriberid", "uuid", "2.1 Respondent\031s Name", "X_gps_latitude",
                    "LGA", "mylga", "num_ss_total_calc", "TOTAL students in Senior Secondary 1 thugh 3",
                    "23. What type(s) of power sources are available at this school? >> Generator",
                    "23. What type(s) of power sources are available at this school? >> None")
              %in% names(edu_onaObj_replaced)))
})
 
test_that("replaceAllNamesWithLabels works on good_eats", {
  good_eats <- onaRead(good_eats_datafile, good_eats_formfile)
  good_eats_replaced <- replaceAllNamesWithLabels(good_eats)
  
  expect_equal(class(good_eats_replaced), "data.frame")
  expect_false(any(names(good_eats_replaced) == "NA"))
  expect_true(all(c("submit_date", "imei", "X_gps_longitude","Rating","Type of Eat", "Food Pic")
                  %in% names(good_eats_replaced)))
  
  expect_false("bad" %in% good_eats_replaced$Rating)
  expect_true("What was I thinking" %in% good_eats_replaced$Rating)
  expect_equivalent(table(good_eats_replaced$Rating)["What was I thinking"], 25)
  
  expect_true("Low Risk" %in% good_eats_replaced[,"Risk Factor"])
})

test_that("replace Functions work with dot-replaced names", {
  good_eats <- onaRead(good_eats_datafile, good_eats_formfile)
  names(good_eats)[3] <- 'food.type' # R replaces characters with dot sometimes
                                     # doesn't replace _, but we don't have better
                                     # test data at the moment
  good_eats_replaced <- replaceAllNamesWithLabels(good_eats)
  good_eats_names_replaced <- replaceHeaderNamesWithLabels(good_eats)
  
  expect_equal(class(good_eats_replaced), "data.frame")
  expect_equal(class(good_eats_names_replaced), "data.frame")
  
  expect_false(any(names(good_eats_replaced) == "NA"))
  expect_true("Type of Eat" %in% names(good_eats_replaced))
  expect_true("Dinner" %in% good_eats_replaced[,"Type of Eat"])
  
  expect_false(any(names(good_eats_names_replaced) == "NA"))
  expect_true("Type of Eat" %in% names(good_eats_names_replaced))
})

# Replace works with multi-lingual forms
test_that("replace Functions work with multi-lingual forms", {
    pde <- onaRead(pde_datafile, pde_formfile)
    pde_names_replaced <- replaceHeaderNamesWithLabels(pde)
    pde_replaced_en <- replaceAllNamesWithLabels(pde, "English")
    pde_replaced_fr <- replaceAllNamesWithLabels(pde, "French")
    
    expect_equal(class(pde_replaced_en), "data.frame")
    expect_equal(class(pde_replaced_fr), "data.frame")
    expect_equal(class(pde_names_replaced), "data.frame")
    
    expect_false(any(names(pde_replaced_en) == "NA"))
    expect_false(any(names(pde_replaced_fr) == "NA"))
    expect_true("A-2.4 Commune" %in% names(pde_replaced_en))
    expect_true("A-2.4 Commune" %in% names(pde_replaced_fr))
    expect_true("Grid is further than 500m" %in% 
        pde_replaced_en[,"A-3.1 A quelle distance estimez-vous le reseau electrique national EDH de cet etablissment."])
    expect_true("Information non disponible/Ne sais pas" %in% 
        pde_replaced_fr[,"A-3.1 A quelle distance estimez-vous le reseau electrique national EDH de cet etablissment."])
    
    expect_false(any(names(pde_names_replaced) == "NA"))
    expect_true("A-2.4 Commune" %in% names(pde_names_replaced))
})

# Adding photo URLs work
test_that("adding photo urls works", {
  good_eats <- onaRead(good_eats_datafile, good_eats_formfile)
  good_eats_with_photo_urls <- addPhotoURLs(good_eats, 'mberg',type='url')
  # check that new columns were added
  expect_true(all(c("food_photo_URL_original", "food_photo_URL_medium", "food_photo_URL_small",
                "location_photo_URL_original", "location_photo_URL_medium",
                "location_photo_URL_small") %in% names(good_eats_with_photo_urls)))
  # check that if original is blank, so is medium and small
  expect_true(all(which(good_eats_with_photo_urls$food_photo_URL_original == "") ==
              intersect(which(good_eats_with_photo_urls$food_photo_URL_small == ""),
                        which(good_eats_with_photo_urls$food_photo_URL_medium == ""))))
  # check one of the URLs
  expect_equal(subset(good_eats_with_photo_urls, description == "Fistikli")$location_photo_URL_original,
               "https://api.ona.io/attachment/?media_file=mberg/attachments/1460708481327.jpg")
  expect_equal(subset(good_eats_with_photo_urls, description == "Fistikli")$location_photo_URL_medium,
               "https://api.ona.io/attachment/medium?media_file=mberg/attachments/1460708481327.jpg")
  expect_equal(subset(good_eats_with_photo_urls, description == "Fistikli")$location_photo_URL_small,
               "https://api.ona.io/attachment/small?media_file=mberg/attachments/1460708481327.jpg")
})

