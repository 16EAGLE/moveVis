skip_on_cran()
context("internal")

#if("df2move" %in% which_tests){
test_that("internal (labels)", {
  expect_equal(moveVis:::.x_labels(c(NA, -10,0,10,20,30)), c("", "10°W", "0°", "10°E", "20°E", "30°E"))
  expect_equal(moveVis:::.y_labels(c(NA, -10,0,10,20,30)), c("", "10°S", "0°", "10°N", "20°N", "30°N"))
})
