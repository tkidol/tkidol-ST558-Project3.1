README
================
Todd Idol
11/18/2020

-   [Description](#description)
-   [Project Repo](#project-repo)
-   [Packages](#packages)
    -   [Tidyverse](#tidyverse)
    -   [Rmarkdown](#rmarkdown)
    -   [Caret](#caret)
    -   [GBM](#gbm)
    -   [Data Table](#data-table)
-   [Code](#code)

Description
===========

Project 3

Project Repo
============

Find the project repo
[here](https://github.com/tkidol/tkidol-ST558-Project3.1).

Packages
========

Tidyverse
---------

The workhorse of this project for 3 main core packages: DPLYR -
manipulated all of my data (selects, joins, filter, new variables… ),
Tibble for rendering Data Frames more effectively, & () ggplot2 for
discrete & continuous data plots

Rmarkdown
---------

Key to the project for the Rmd file itself including the knitr::
functions for consumable object output (kable) & README.md github doc
output through render()

Caret
-----

Creates standardized train/test data, create controls and tuning
parameters and execute the regression/boosted regression tree analysis

GBM
---

Used by caret package to execute the generalized boosted regression
model for prediction

Data Table
----------

Used for setattr function to change row names in prediction output

Code
====

install packages run app from empty rstudio

\` library(dplyr) library(rmarkdown)

charDay &lt;- c(“Sunday”, “Monday”, “Tuesday”, “Wednesday”, “Thursday”,
“Friday”, “Saturday”)

numDay &lt;- c(0, 1, 2, 3, 4, 5, 6)

output\_file &lt;- paste0(charDay,“Analysis.md”)

params &lt;- lapply(numDay, FUN = function(x) (list(day = x)))

reports &lt;- tibble(output\_file, params)

apply(reports, MARGIN = 1, FUN = function(x) { render(“P2\_TKIdol.Rmd”,
output\_file = x\[\[1\]\], params = x\[\[2\]\]) })

\`
