library(testthat)

source("C:/Users/User/Documents/R/packages/FarsPackage/R/fars_functions.R")

test_that("fars_functions.R", {

  file <- make_filename(2013)
  expect_that(file, matches("accident_2013.csv.bz2"))

})



