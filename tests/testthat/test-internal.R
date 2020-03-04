skip_on_cran()
context("internal")

#if("df2move" %in% which_tests){
test_that("internal (labels)", {
  expect_length(expect_is(moveVis:::.x_labels(c(NA, -10,0,10,20,30)), "expression"), 5)
  expect_length(expect_is(moveVis:::.y_labels(c(NA, -10,0,10,20,30)), "expression"), 5)
})
