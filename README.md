<link href="http://kevinburke.bitbucket.org/markdowncss/markdown.css" rel="stylesheet"></link>
ona.R
=========

ona.R is a library for making reading data form [ona.io](http://ona.io) into R easier.

Examples
--------

 * Quick example -- [Charting good_eats submission over time](http://onaio.github.com/ona.R/examples/Good_Eats_Example.html)
 * Making maps -- [Making maps with North Ghana data](http://onaio.github.com/ona.R/examples/Water_Points_Example.html)
 * Quality control -- [How long did it take to process a survey](http://onaio.github.com/ona.R/examples/How_Long_Example.html)

For most of the examples, I use the wonderful [ggplot2](http://ggplot2.org) library, which is an amazing data visualization worth every minute of your time spent learning it.
 
Features
--------

At the moment, it has the following features:

 * One command download of [ona.io](https://ona.io) data -- both public and private
 * Casting of data to the right type, based on the type of the input field in XLSform
   * `select one` fields are converted to factors
   * `integer`/`decimal` converted to numerics
   * `date` fields (including `today`), and `datetime` fields (including `start` and `end`) are converted to [lubridate](http://cran.r-project.org/package=lubridate) instants [timezone information is discarded at the moment]
   * TRUE / FALSE fields created out of `select multiple` options are converted into booleans
 * removeColumns convenience function that removes columns based on matching the title with a regular expression
 * 'extra-schema' over-ride. This helps provide a type to `calculate` fields if that is desired; automagically calculating the type of a calculate field is not in the short-term feature list

For planned features, go to the [issues](https://github.com/onaio/ona.R/issues) page.


