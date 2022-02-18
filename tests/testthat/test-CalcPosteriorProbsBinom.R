################################################################################ .
#   This file provides an example of how to create a test.                  ####
#   For an R package, if you are in the package directory you can execute
#   the following command usethis::use_test("name") to create a new test
#   file.  Typically, if you have a file named FileXX.R in the R directory
#   of the package then you would execute usethis::use_test("FileXX") a
#   new text file will be added to tests/testthat/test-FileXX.R.
################################################################################ .


context( "Test - CalcPosteriorProbsBinom.R")

test_that("Test Name", {
    dA1     <- 1
    dB1     <- 1
    dA2     <- 1
    dB2     <- 1
    dDelta1 <- 0
    dDelta2 <- 0

    dExp1   <- 0.5
    dExp2   <- 0.5

    dCalc   <- CalcPosteriorProbsBinom( dA1, dB1, dA2, dB2, dDelta1, dDelta2 )
    expect_equal( dCalc$dPPGrtDelta1, dExp1,  tolerance = 0.0001, label = "Pr( > Delta1 )" )
    expect_equal( dCalc$dPPGrtDelta2, dExp2,  tolerance = 0.0001, label = "Pr( > Delta2 )" )
})
