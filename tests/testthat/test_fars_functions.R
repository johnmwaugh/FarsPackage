library(testthat)

test_that("fars_functions.R", {

    sum13 <- fars_summarize_years(2013)
    nov13 <- sum13[11,2]
    nov13 <- as.numeric(as.vector(nov13))
    expect_that(nov13, equals(2615))

})




