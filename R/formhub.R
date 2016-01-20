library(RJSONIO)
library(stringr)
library(plyr)
library(RCurl)
library(lubridate)
library(sp)
library(doBy)


setClass("formhubData", representation("data.frame", form="data.frame"), contains="data.frame")

#' Produce a data.frame out of a formhubDataObj
#'
#' @param the formhub object which will be possibly co-erced to a dataframe.
#' @export
#' @return A data.frame represntation of this formhub oject
#' @examples
#' #' Produce a SpatialPointsDataFrame if data has a column of type `gps` or `geopoint`.
#' Otherwise, return NA.
#'
#' @param the formhub object which will be possibly co-erced to a SpatialPointsDataFrame object.
#' @export
#' @return A SpatialPointsDataFrame representation of this formhub Object
#' @examples
#' good_eats_data <- as.data.frame(formhubDownload("good_eats", "mberg"))
#' class(ge_spdf) # "data.frame"
as.data.frame.formhubData <- function(fhD, ...) {
   data.frame(setNames(fhD@.Data, names(fhD)))
}

#' Produce a SpatialPointsDataFrame if data has a column of type `gps` or `geopoint`.
#' Otherwise, return NA.
#'
#' @param the formhub object which will be possibly co-erced to a SpatialPointsDataFrame object.
#' @export
#' @return A SpatialPointsDataFrame representation of this formhub Object
#' @examples
#' good_eats_data <- as.data.frame(formhubDownload("good_eats", "mberg"))
#' ge_spdf <- as.SpatialPointsDataFrame(good_eats_data)
#' class(ge_spdf) # "SpatialPointsDataFrame"
as.SpatialPointsDataFrame <- function(formhubObj) {
  gpsfields = subset(formhubObj@form, type %in% c("gps", "geopoint"))$name
  gpsfields = gpsfields[which(gpsfields %in% names(data.frame(formhubObj)))]
  if(length(gpsfields) == 0) NA
  else {
    #TODO: deal with multiple gps questions?
    gpses <- data.frame(formhubObj)[,gpsfields[1]]
    dropRows <- gpses=="NA" | is.na(gpses)
    if(any(dropRows)) {    
      warning(paste("formhub.R: Dropping",table(dropRows)['TRUE'],"rows because GPS not present."))
      gpses <- gpses[!dropRows]
    }
    gpses_split <- apply(str_split_fixed(gpses, " ", 3)[,c(2,1)], 2, FUN=as.numeric)
    SpatialPointsDataFrame(gpses_split, data.frame(formhubObj)[!dropRows,],
                           proj4string=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84"))
  }
}

#' Get a new dataframe, where the header contains the full questions as opposed to slugs.
#'
#' formhub Objects have some data, as well as the form, which documents how
#' the data was obtained through a survey. The data, by default, is represented by
#' slugs, ie, items in the `name` column in the original xfrom. This function
#' replaces slugs in the header with the actual question text.
#'
#' @param formhubDataObj is the formhub data object whose data slot will be renamed
#' @export
#' @return a new data frame with the column names renamed from `name`s (slugs) to `label`s(full questions)
#' @examples
#' good_eats <- formhubDownload("good_eats", "mberg")
#' names(good_eats) # still slugged names
#' summary(good_eats$rating)
#' full_header_good_eats <- replaceHeaderNamesWithLabels(good_eats)
#' names(full_header_good_eats) # not slugged anymore
#' summary(full_header_good_eats$Rating) # but data is the same
replaceHeaderNamesWithLabels <- function(formhubDataObj) {
  newNames <- lapply(names(formhubDataObj), function(nm) {
    index <- which(formhubDataObj@form$name == nm)
    if (length(index) == 0) {
        # 2nd pass: deals with data frame names where character are replaced by dots
        index <- which(str_detect(formhubDataObj@form$name, paste0('^',nm,'$'))) 
    } 
    # replace the name if exactly one other label matches
    if(length(index) == 1 && !is.na(formhubDataObj@form$label[[index]])) {
       formhubDataObj@form$label[[index]]
    } else {
      nm
    }
  })
  setNames(data.frame(formhubDataObj), newNames)
}

#' Get a new dataframe, where all 'name's are replaced with full labels.
#'
#' formhub Objects have some data, as well as the form, which documents how
#' the data was obtained through a survey. The data, by default, is represented by
#' slugs, ie, items in the `name` column in the original xfrom. This function
#' replaces slugs in the header with actual question text, and replaces slugs in
#' select one options with the actual resposne text.
#'
#' @param formhubDataObj is the formhub data object whose data slot will be renamed
#' @param language if this is a multi-lingual form, the language of choice for
#'        the labels should be passed in. For single-language forms language=NULL.
#' @export
#' @return a new data frames with the column names, as well as factor values, renamed from `name`s (slugs) to `label`s(full questions)
#' @examples
#' good_eats <- formhubDownload("good_eats", "mberg")
#' names(good_eats) # still slugged names
#' summary(good_eats$rating)
#' good_eats_readable <- replaceAllNamesWithLabels(good_eats)
#' names(good_eats_readable) # not slugged anymore
#' summary(good_eats_readable$`Risk Factor`) # not slugged anymore.
replaceAllNamesWithLabels <- function(formhubDataObj, language=NULL) {
  data <- data.frame(formhubDataObj)
  form <- formhubDataObj@form
  row.names(form) <- form$name
  
  l_ply(form[form$type == 'select one',]$name, function(field_name) {
    ol <- RJSONIO::fromJSON(form[field_name,'options'])
    old <- if (is.null(language)) {
        tryCatch({
            ldply(ol, rbind)
        }, error=function(e) {
            stop("If you have a multi-language form, please specify the language.") 
        })
    } else {
        tryCatch({
            ldply(ol, function(option) {
                c(name = option[['name']], label=option[['label']][[language]])
            })
        }, error=function(e) {
            stop("Language argument should be null for single-language forms.") 
        })
    }
    if (! field_name %in% names(data)) {
      col_name <- names(data)[str_detect(field_name, paste0('^', names(data), '$'))] # sometimes characters are
        # replaced by dot; we take advantage of fact that . is an all-character
        # match for regular expressions, and reassign field_name to a name that matches
    } else {
      col_name <- field_name
    }
    stopifnot(length(col_name) == 1) #form and data don't match
    levels(data[,col_name]) <<- recodeVar(levels(data[,col_name]), 
                as.character(old$name), as.character(old$label), default=NA, keep.na=T)
  })
  replaceHeaderNamesWithLabels(new("formhubData", data, form=form))
}

#' Download data from formhub.
#'
#' This function downloads a dataset for the given form and username, and produces a 
#' formhubData Object.
#'
#' @param formName formname on formhub.org for which we download the data
#' @param uname formhub.org username
#' @param pass formhub.org password, if the data and/or form is private
#' @param ... other parameters to pass onto formhubRead
#' @export
#' @return formhubDataObj a formhubData Object, with "data" and "form" slots
#' @examples
#' good_eats <- formhubDownload("good_eats", "mberg")
#' good_eats # is a data frame of all the data
#' good_eats@form # is the form for that data, encoded as a dataframe
#' privateData <- formhubDownload("Private_Data_For_Testing", uname="formhub_r", pass="t3st~p4ss")
formhubDownload = function(formName, uname, pass=NA, ...) {
  fUrl <- function(formName, uname, form=F) {
    str_c('http://classic.ona.io/', uname, '/forms/', formName,
          ifelse(form,'/form.json', '/data.csv'))
  }
  dataUrl = fUrl(formName, uname)
  formUrl = fUrl(formName, uname, form=T)
  
  #TODO -- pre-flight check? below doesn't work; expects 200+ status
  #if(!url.exists(datUrl)) { stop("could not find ", dataUrl)}
  #if(!url.exists(formUrl)) { stop("could not find ", formUrl)}
  
  # get the data, depending on public or not
  dataCSVstr <- ifelse(is.na(pass),
                 getURI(dataUrl),
                 getURI(dataUrl, userpwd=str_c(uname,pass,sep=":"), httpauth = 1L))
  
  # get the form, depending on public or not
  # TODO: situations where data is public, form is not
  formJSON <- ifelse(is.na(pass),
                 getURI(formUrl),
                 getURI(formUrl, userpwd=str_c(uname,pass,sep=":"), httpauth = 1L))
  formhubRead(textConnection(dataCSVstr), formJSON, ...)
}

#' Reads data from a passed csv filename and json filename into a formhubData object.
#'
#' This function creates a formhubData object from two files: a csv data file, and a 
#' json form file. These should both be downloaded from formhub.org for the same form.
#'
#' @param csvfilename filename (or a connection object) that has the formhub data
#' @param jsonfilename filename of a json file (or a connection object) that has the form.json form
#' @param extraFormDF override the form (such as by providing a type for a calculate, a new label, etc.)
#' @param dropCols a regular expression, any column name that matches that regexp will be dropped
#' @param na.strings list of na.strings to be passed onto read.csv (default: "n/a")
#' @param keepGroupNames for a question with name foo in group bar, keepGroupName=T will generate
#'        a name foo.bar, while keepGroupName=F will generate a name bar
#' @export
#' @return formhubDataObj a formhubData Object, with "data" and "form" slots
#' @examples
#' # will need to download data.csv and form.json for a specific form on formhub, for below, download
#' http://formhub.org/mberg/forms/good_eats/data.csv http://formhub.org/mberg/forms/good_eats/form.json
#' good_eats <- formhubRead("~/Downloads/good_eats_2013_05_05.csv", "~/Downloads/good_eats.json")
#' head(good_eats) # is a data frame of all the data
#' good_eatsX <- formhubRead("~/Downloads/good_eats_2013_05_05.csv", "~/Downloads/good_eats.json",
#'              extraFormDF=data.frame(name="imei", type="integer", label="IMEI"))
#' good_eatsX@form # note that imei is now slated as type "integer" instead of type "imei"
#' str(good_eatsX$imei) # also notice that it is numeric instead of a factor
#' good_eatsWO <- formhubRead("~/Downloads/good_eats_2013_05_05.csv", "~/Downloads/good_eats.json",
#'              dropCols="submit*")
#' names(good_eatsWO) # notice how submit_date and submit_date are no longer there
#' good_eatsNA <- formhubRead("~/Downloads/good_eats_2013_05_05.csv", "~/Downloads/good_eats.json",
#'              na.strings=c("999"))
#' good_eatsNA$amount # notice that the value that was 999 is now missing. This is helpful when using values such
#'                    # as 999 to indicate no data
formhubRead  = function(csvfilename, jsonfilename, extraFormDF=data.frame(), dropCols="", na.strings=c("n/a"),
                        convert.dates=TRUE, keepGroupNames=TRUE) {
  dataframe <- read.csv(csvfilename, stringsAsFactors=FALSE, header=TRUE, na.strings=na.strings)
  formDF <- form_to_df(RJSONIO::fromJSON(jsonfilename, encoding='utf-8'), keepGroupNames=keepGroupNames)
  
  # drop group names from data frame names
  dataframe <- setNames(dataframe, llply(names(dataframe), function(name) {
    if (name %in% formDF$name) {
      name
    } else { # try to deal with the fact that R munges non-alphanumeric characters into a .
      split_by_dots <- unlist(str_split(name, '\\.'))
      sbd_length <- length(split_by_dots)
      if(sbd_length == 1) { name } else {
        # first try and see if we find a match as a simple question
        searchstring <- paste0('^',split_by_dots[sbd_length],'$')
        possible_name <- formDF$name[str_detect(formDF$name, searchstring)]
        if(length(possible_name) == 1) { # we're home free
          possible_name
        } else {
          # next try to see if we match the last dot-phrase (ie, an option for multi-select)
          searchstring <- paste0('^',split_by_dots[sbd_length-1], '\\.',
                                 split_by_dots[sbd_length],'$')
          possible_name <- formDF$name[str_detect(formDF$name, searchstring)]
          if(length(possible_name) == 1) possible_name else name
        }
      }
    }
  }))

  formhubCast(dataframe, formDF, extraFormDF=extraFormDF, dropCols=dropCols,
              convert.dates=convert.dates)
}

#' Casts a dataframe to the right types based on a form-dataframe.
#'
#' This function creates a formhubData object based on a pair of dataframes: the data
#' and the form that describes the data. The column names of the data match with the "name" column of 
#' the form, and the "type" column in the form provide information for type conversion.
#'
#' @param dataDF data
#' @param formDF form data frame. See format above.
#' @param extraFormDF override the form (such as by providing a type for a calculate, a new label, etc.)
#' @param dropCols a regular expression, any column name that matches that regexp will be dropped
#' @return formhubDataObj a formhubData Object, with "data" and "form" slots
#' @examples
#' 
#' #See examples under formhubRead; this should be used through formhubRead in almost all cases
formhubCast  = function(dataDF, formDF, extraFormDF=data.frame(), dropCols="", convert.dates=TRUE) {
  dataDF <- removeColumns(dataDF, dropCols)

  extraFormDF <- colwise(as.character)(extraFormDF)
  formDF <- rbind.fill(extraFormDF, formDF)
  formDF <- formDF[!duplicated(formDF$name),]
  
  new("formhubData", recastDataFrameBasedOnFormDF(dataDF, formDF, convert.dates=convert.dates),
                     form=formDF)
}

#' Converts formhub form.json format to dataframe format. Dataframe has name, type, label columns.
#'
#' @param formJSON formJSON that has been freshly read from JSON using JSONIO's fromJSON function.
#' @param keepGroupNames for a question with name foo in group bar, keepGroupName=T will generate
#'        a name foo.bar, while keepGroupName=F will generate a name bar
#' @return formDF
#' @examples
#' good_eats_form_df <- form_to_df(fromJSON("~/Downloads/good_eats.json"))
form_to_df = function(formJSON, keepGroupNames=TRUE) {
  form_to_df_internal = function(thisJSON, prefix="") {
    ldply(thisJSON[["children"]], function(child) {
      nom <- if (prefix == "") { child[["name"]] } else { paste(prefix, child[["name"]], sep=".") }
      if (child[["type"]] == "group") {
        if(keepGroupNames) {
          form_to_df_internal(child, prefix=nom)
        } else {
          form_to_df_internal(child)
        }
      } else if (child[["type"]] == "select all that apply") {

        options <- child[["children"]]        
        nameprefix <- ifelse(prefix=="", child[["name"]], str_c(prefix, child[["name"]], sep="."))
        names <- paste(nameprefix, sapply(options, function(o) o['name']), sep=".")
        
        labels <- sapply(options, function(o) { paste( child[["label"]], o['label'], sep=" >> ")})
        #TODO: fix properly; this is a hack for multi-lingual labels
        labels <- if(class(labels) == 'matrix') { labels[1,] } else { labels }
        data.frame(name=names, label=labels, type="boolean", options=NA, stringsAsFactors=F)
      } else if (child[["type"]] == "select one") {
        if("children" %in% names(child)) {
            data.frame(name=nom, type=child[["type"]], options=toJSON(child$children),
                   label=if("label" %in% names(child)) {child[["label"]]} else {child[["name"]]},
                   stringsAsFactors=F)
        } else if ("itemset" %in% names(child)) {
            data.frame(name=nom, type=child[["type"]],
                     options=toJSON(formJSON$choices[[child[['itemset']]]]), # options are more complex with itemset
                     label=if("label" %in% names(child)) {child[["label"]]} else {child[["name"]]},
                     stringsAsFactors=F)
        }
      } else {
        data.frame(name=nom, type=child[["type"]], options=NA,
                   label=if("label" %in% names(child)) {child[["label"]]} else {child[["name"]]},
                   stringsAsFactors=F)
      }
    })
  }
  df <- form_to_df_internal(formJSON)
  df
}

#' Casts a dataframe to the right types based on a form-dataframe. Used by formhubCast
#'
#' @param df data
#' @param formdf form data frame. See format on formhubCast.
#' @return df re-casted data frame.
#' @examples
#' 
#' #See examples under formhubRead; this should be used through formhubRead in almost all cases
recastDataFrameBasedOnFormDF = function(df, formdf, convert.dates=TRUE) {
  # do this by type
  #TODO: refactor
  stopifnot(is.character(formdf$name))
  # re-type everything in df of type in types with reTypeFunc
  reTypeColumns <- function(types, reTypeFunc) { 
    cols <- c(subset(formdf, type %in% types)$name)
    colsToReType <- unique(cols[cols %in% names(df)])
    suppressWarnings(suppressMessages(
      df[colsToReType] <<- colwise(reTypeFunc)(df[colsToReType])
    ))
  }
  # lubridate doesn't handle ISO 8601 datetimes yet, so we just chuck the timezone info
  iso8601DateTimeConvert <- function(x) { ymd_hms(str_extract(x, '^[^+Z]*(T| )[^+Z-]*')) }
  
  # some formhub dates come in the format 2011-04-24T00:20:00.000000
  iso8601DateConvert <- function(x) { ymd(str_extract(x, '^[^T]*')) }
  
  reTypeColumns(c("integer", "decimal"), as.numeric)
  reTypeColumns(c("boolean"), as.logical)
  reTypeColumns(c("select one", "imei", "subscriberid", "simserial", "deviceid", "phonenumber"), as.factor)
  if(convert.dates) {
    reTypeColumns(c("date", "today"), iso8601DateConvert)
    reTypeColumns(c("start", "end", "datetime"), iso8601DateTimeConvert)
  }
  df
}

#' Add columns corresponding to the original, as well as medium and small thumbnails of images
#' as stored on the formhub server.
#'
#' @param the formhubData object which to create URLs from.
#' @param the formhub username of the person who owns this form.
#' @param type: "url" or "img". URL only puts image url in, img puts image tag in.
#'
#' @export
#' @return a formhubData object, with a few additional URL columns
#' @examples
#' good_eats <- as.data.frame(formhubDownload("good_eats", "mberg"))
#' good_eats_with_photos <- addPhotoURLs(good_eats, "mberg")
#' grep("URL", names(good_eats_with_photo), value=T) # the new columns
addPhotoURLs = function(formhubDataObj, formhubUsername, type="url") {
  photos <- c(subset(formhubDataObj@form, type %in% "photo")$name)
  htmlFromCol <- function(photoCol, size, type) {
    stopifnot(size %in% c("", "medium", "small"))
    if (type == "url") { 
      ifelse(is.na(photoCol), "",
           sprintf("https://formhub.org/attachment/%s?media_file=%s/attachments/%s",
                   size, formhubUsername, photoCol))
    } else if (type == "img") {
      ifelse(is.na(photoCol), "",
             sprintf('<img src="https://formhub.org/attachment/%s?media_file=%s/attachments/%s" />',
                     size, formhubUsername, photoCol))
    } else { 
      stop("Type must be either 'url' or 'img'.")
    }
  }
  tmp <- llply(photos, function(photoColName) {
    photoCol <- formhubDataObj[[photoColName]]
    setNames(data.frame(
      htmlFromCol(photoCol, "", type),
      htmlFromCol(photoCol, "medium", type),
      htmlFromCol(photoCol, "small", type),
      stringsAsFactors=FALSE
    ), paste0(photoColName, c("_URL_original", "_URL_medium", "_URL_small")))
  })
  tmp <- cbind(formhubDataObj, do.call(cbind, tmp))
  new("formhubData", tmp, form=formhubDataObj@form)
}

#' Helper function to remove columns from data based on reg-exp matching. Also takes list of strings.
#'
#' @param df data
#' @param columnNameRegExpMatcher pattern(s) to match to columns; matched columns are dropped.
#' @return a smaller data frame.
#' @examples
#' good_eats_df <- formhubDownload("good_eats", "mberg")
#' names(good_eats_form_df) # note it includes submit_date and submit_data both
#' names(removeColumns(good_eats_form_df, "submit*")) # both of which are gone now
#' names(removeColumns(good_eats_form_df, c("submit*", "_gps*")) # you can pass a list of regular expressions
removeColumns <- function(df, columnNameRegExpMatcher) {
  if (columnNameRegExpMatcher=="" || is.na(columnNameRegExpMatcher)) { 
    df 
  } else {
    orMatcher <- paste(columnNameRegExpMatcher, collapse="|")
    df[,-which(str_detect(names(df), orMatcher))]
  }
}


