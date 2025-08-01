library(testthat)
library(moveVis)
library(move2)
library(sf)
library(terra)

Sys.setenv("R_TESTS" = "") ## needed to pass R CMD check: https://github.com/hadley/testthat/issues/144

test_check("moveVis")
