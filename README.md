[![Build Status](https://travis-ci.org/onaio/ona.R.svg?branch=master)](https://travis-ci.org/onaio/ona.R)

ona.R
=====

ona.R is a library for making reading data form [ona.io](https://ona.io) into R easier.

This project is fork of [formhub.r](https://github.com/prabhasp/formhub.R) by Prabhas Pokharel.  Future developments and improvements of the library will be done here.

Examples
--------

 * Getting started -- [Basics of ona.R](https://onaio.github.com/ona.R/demo/Basics_of_ona.R.html)
 * Quick example -- [Charting good_eats submission over time](https://onaio.github.com/ona.R/demo/Good_Eats_Example.html)
 * Making maps -- [Making maps with North Ghana data](https://onaio.github.com/ona.R/demo/Water_Points_Example.html)
 * Quality control -- [How long did it take to process a survey](https://onaio.github.com/ona.R/demo/How_Long_Example.html)
 * Quality control II -- [A basic tutorial on removing outliers from your data](https://onaio.github.com/ona.R/demo/RemoveOutliers.html)
 * Making Readable Data -- [Replacing "slugs" in data output with survey text](https://onaio.github.com/ona.R/demo/ReadableData.html)

For most of the examples, I use the [ggplot2](http://ggplot2.org) library, which is an amazing data visualization library worth every minute of your time spent learning it.

Installing ona.R
--------------------
The package for ona.R in R is simply called `ona`. For now, it cannot be installed from CRAN, but using Hadley Wickam's excellent `devtools` package, you can install it directly from github. To install (or upgrade), run the following commands from your R terminal:

    install.packages('devtools') 
    library(devtools)
    install_github('onaio/ona.R')
    library(ona)

And voila! The `ona` package is installed in your R system! You can check by running:

    help(onaDownload)
 
Features
--------

At the moment, it has the following features:

 * One command download of [ona.io](https://ona.io/) data -- both public and private
 * (Automatic) casting of data to the right type, based on the type of the input field in XLSform
   * `select one` fields are converted to factors
   * `integer`/`decimal` converted to numerics
   * `date` fields (including `today`), and `datetime` fields (including `start` and `end`) are converted to [lubridate](http://cran.r-project.org/package=lubridate) instants [timezone information is discarded at the moment]
   * TRUE / FALSE fields created out of `select multiple` options are converted into booleans
 * removeColumns convenience function that removes columns based on matching the title with a regular expression
 * 'extra-schema' over-ride. 

For planned features, go to the [issues](https://github.com/onaio/ona.R/issues) page.


