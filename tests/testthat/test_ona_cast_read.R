library(testthat)
library(stringr)
library(ona)


test_dir = "fixtures/"
#test_dir("~/onaio/ona.R/tests/")



edu_datafile <- str_c(test_dir, "edu1.csv")
edu_formfile <- str_c(test_dir, "edu1.json")
hlt_datafile <- str_c(test_dir, "health1.csv")
hlt_formfile <- str_c(test_dir, "health1.json")
good_eats_datafile <- str_c(test_dir, "good_eats.csv")
good_eats_formfile <- str_c(test_dir, "good_eats.json")

edu_rawdf <- read.csv(edu_datafile, na.strings="n/a", stringsAsFactors=FALSE, header=TRUE)
hlt_form_df <- form_to_df(fromJSON(hlt_formfile))

edu_onaObj <- onaRead(edu_datafile, edu_formfile)
edu_df <- edu_onaObj
edu_form_df <- edu_onaObj@form

good_eats <- onaRead(good_eats_datafile, good_eats_formfile)

test_that("convert.dates works", {
  ef <- onaRead(edu_datafile, edu_formfile)
  expect_true(is.instant(ef$start))
  ef <- onaRead(edu_datafile, edu_formfile, convert.dates=F)
  expect_false(is.instant(ef$start))
})

test_that("formdf is read properly", {
  edu_typeofname <- function(nom) { subset(edu_form_df, name==nom)$type }
  expect_true(edu_typeofname("start") == "start")
  expect_true(edu_typeofname("km_to_catchment_area") == "integer")
  expect_true(edu_typeofname("num_toilet.num_toilet_boy") == "integer")
  expect_true(edu_typeofname("generator_funct_yn") == "select one")
  expect_true(edu_typeofname("uuid") == "calculate")
  expect_true(edu_typeofname("respondent_name") == "text")
  expect_true(edu_typeofname("respondent_contact") == "string")
  expect_true(edu_typeofname("power_sources.generator") == "boolean")
  
  expect_true(is.character(edu_form_df$type))
  expect_true(is.character(edu_form_df$name))
  expect_true(is.character(edu_form_df$label))
  
  hlt_typeofname <- function(nom) { subset(hlt_form_df, name==nom)$type }
  expect_true(hlt_typeofname("not_for_private_1.toilets_available.num_flush_or_pour_flush_piped")=="integer")
  expect_true(hlt_typeofname("not_for_private_1.power_sources.generator")=="boolean")
})

test_that("formdf is read properly when keepGroupNames is FALSE", {
  hlt_form_df_without_groups <- form_to_df(fromJSON(hlt_formfile), keepGroupNames=F)
  expect_false(any(str_detect(hlt_form_df_without_groups$name, 'not_for_private')))
  expect_true(all(c("num_flush_or_pour_flush_piped", "power_sources.generator") %in%
                    hlt_form_df_without_groups$name))
  hlt_typeofname <- function(nom) { subset(hlt_form_df_without_groups, name==nom)$type }
  expect_true(hlt_typeofname("num_flush_or_pour_flush_piped")=="integer")
  expect_true(hlt_typeofname("power_sources.generator")=="boolean")
  
  hlt_form_df_with_groups <- form_to_df(fromJSON(hlt_formfile), keepGroupNames=T)
  expect_true(all(hlt_form_df_without_groups$type == hlt_form_df_with_groups$type))
  expect_true(all(hlt_form_df_without_groups$label == hlt_form_df_with_groups$label))
  expect_true(all(hlt_form_df_without_groups$options == hlt_form_df_with_groups$options, na.rm=T))
})

# test_that("reCastingRVectors Works as expected", {
#   expect_true(is.character(edu_rawdf$mylga))
#   
#   expect_true(is.factor(recastRVectorBasedOnonaType(edu_rawdf$mylga, "select one")))
#   expect_true(is.character(recastRVectorBasedOnonaType(edu_rawdf$ward, "text")))
# 
#   expect_true(is.numeric(recastRVectorBasedOnonaType(
#       edu_rawdf$num_students_total_gender.num_students_female, "integer")))
#   expect_true(is.numeric(recastRVectorBasedOnonaType(
#     edu_rawdf$num_students_total_gender.num_students_female, "integer")))
# })


test_that("pre-conversion expectations are correct (if failing, fix dataset)", {
  expect_true(is.character(edu_rawdf$mylga_zone))
  expect_true(is.logical(edu_rawdf$mylga_lga_in_benue)) # all NAs
  expect_true(is.numeric(edu_rawdf$respondent_contact)) # phone numbers
})

test_that("groups and select multiples convert correctly", {
  edu_rawdf2 <- read.csv(edu_datafile) #no na.strings, no stringsAsFactors=FALSE
  #again, just to make sure input is perpared
  expect_true(is.factor(edu_rawdf2$num_pry_total_gender.num_pry_female))
  expect_true(is.factor(edu_rawdf2$num_pry_total_gender.num_pry_male))
  expect_true(is.factor(edu_rawdf2$power_sources.generator))
  expect_true(is.factor(edu_rawdf2$power_sources.grid))
  
  edu_df2 <- recastDataFrameBasedOnFormDF(edu_rawdf2, edu_form_df)
  expect_true(is.numeric(edu_df2$num_pry_total_gender.num_pry_female))
  expect_true(is.numeric(edu_df2$num_pry_total_gender.num_pry_male))
  
  expect_true(is.logical(edu_df2$power_sources.generator))
  expect_true(is.logical(edu_df2$power_sources.grid))
})

test_that("onaRead converted types properly", {
  expect_true(is.factor(edu_df$mylga_zone)) # type: select one
  expect_true(is.factor(edu_df$mylga_lga_in_benue))

  expect_true(is.numeric(edu_df$num_students_total_gender.num_students_female))
  expect_true(is.numeric(edu_df$num_students_total_gender.num_students_male))

  expect_true(is.character(edu_df$ward)) #type : string / text
  expect_true(is.character(edu_df$community))
  # XXX : what should be the behavior?
  # expect_true(is.character(edu_df$respondent_contact)) # type: phone number
  expect_true(is.character(edu_df$photo)) # type: attachment
  
  
  expect_true(is.instant(edu_df$start)) # type: start
  expect_true(is.instant(edu_df$end)) # type: end 
  
  # need to test type: datetime
  expect_true(is.instant(good_eats$submit_date))
  expect_true(is.instant(good_eats$submit_data))
  
  # miscellaneous values that are converted to factors
  expect_true(is.factor(good_eats$imei))
  expect_true(is.factor(edu_df$deviceid))
  expect_true(is.factor(edu_df$subscriberid))
  expect_true(is.factor(edu_df$simserial))
  expect_true(is.factor(edu_df$phonenumber))
  
})  
  
test_that("onaRead parses dates in wild ona data properly", {
  expect_true(is.instant(recastDataFrameBasedOnFormDF(
      read.csv(textConnection("date\n2012-08-15T00:00:00.000000\n2012-08-15T00:00:00.000000")),
      data.frame(name=c("date"), type=c("today"), label=c("Today's date:"), stringsAsFactors=F))
    $date))
  
  
})

test_that("passing nice extraFormDF in works", {
  extraFormDF = data.frame(name=c("mylga",      "mylga_state", "deviceid"),
                           type=c("select one", "select one", "integer"),
                           label=c("LGA",       "STATE",      "Device ID"))
  
  edu_df_with_extra <- onaRead(edu_datafile, edu_formfile, extraFormDF=extraFormDF)
  expect_true(is.factor(edu_df_with_extra$mylga))
  expect_true(is.factor(edu_df_with_extra$mylga_state))
  expect_true(is.numeric(edu_df_with_extra$deviceid))
  expect_true(all(edu_df_with_extra@form$type %in% 
    c("boolean", "calculate", "end", "gps", "integer", "note", "phonenumber", "photo",
      "select one", "simserial", "start", "string", "subscriberid", "text", "today")))
})

test_that("passing bad extraFormDF in works", {
  extraFormDF = setNames(data.frame(rbind(
    c("mylga", "select one", "LGA"), 
    c("mylga_state", "select one", "State"))),
                         c("name", "type", "label"))
  
  edu_df_with_extra <- onaRead(edu_datafile, edu_formfile, extraFormDF=extraFormDF)
  expect_true(is.factor(edu_df_with_extra$mylga))
  expect_true(is.factor(edu_df_with_extra$mylga_state))
})

test_that("passing na.strings works", {
  na.strings = c("southeast")
  edu_df_wo_SE <- onaRead(edu_datafile, edu_formfile, na.strings=na.strings)
  expect_equal(levels(edu_df_wo_SE$mylga_zone), c("northwest"))
})

test_that("onaRead works when keepGroupNames is FALSE", {
  edu <- onaRead(edu_datafile, edu_formfile, keepGroupNames=F)
  expect_true(all(c("num_ss_total", "num_students_female") %in% names(edu)))
  
  edu_with_groups <- onaRead(edu_datafile, edu_formfile)
  expect_false(all(names(edu_with_groups) == names(edu)))
  expect_equivalent(setNames(edu, names(edu_with_groups)), edu_with_groups)
  expect_equivalent(setNames(edu_with_groups, names(edu)), edu)
  
  hlt <- onaRead(hlt_datafile, hlt_formfile, keepGroupNames=F)
  expect_false(any(str_detect(names(hlt), 'not_for_private')))
  expect_true(all(c("num_flush_or_pour_flush_piped", "power_sources.generator") %in%
                    names(hlt)))
  expect_true(is.numeric(hlt$num_flush_or_pour_flush_piped))
  expect_true(is.logical(hlt$power_sources.generator))
  
  hlt_with_groups <- onaRead(hlt_datafile, hlt_formfile)
  expect_false(all(names(hlt_with_groups) == names(hlt)))
  expect_equivalent(setNames(hlt, names(hlt_with_groups)), hlt_with_groups)
  expect_equivalent(setNames(hlt_with_groups, names(hlt)), hlt)
})

test_that("column deletion works", {
  edu_df_dropped <- removeColumns(edu_df, "")
  edu_df_dropped <- removeColumns(edu_df_dropped, NA)
  expect_equal(names(edu_df), names(edu_df_dropped))
  
  edu_df_dropped <- removeColumns(edu_df, "mylga_.*_in_.*")
  expect_equal(names(edu_df_dropped)[which(str_detect(names(edu_df_dropped), "mylga"))],
               c("mylga_zone", "mylga_state", "mylga"))

  edu_df_dropped <- removeColumns(edu_df, "^num.*")
  expect_false(any(str_detect("num", names(edu_df_dropped))))
  expect_false("num_pry_total_gender.num_pry_female" %in% names(edu_df_dropped))
  expect_true("mylga_lga_in_benue" %in% names(edu_df_dropped))

  edu_df_dropped <- removeColumns(edu_df, c("num.*", "mylga_.*"))
  #expect_false("mylga_lga_in_benue" %in% names(edu_df_dropped))
  expect_false("num_pry_total_gender.num_pry_male" %in% names(edu_df_dropped))
  expect_false("mylga_lga_in_benue" %in% names(edu_df_dropped))
  expect_false("mylga_zone" %in% names(edu_df_dropped))
})
