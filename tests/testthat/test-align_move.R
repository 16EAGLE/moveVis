skip_on_cran()
context("align_move")

#if("align_move" %in% which_tests){
test_that("align_move (default)", {
  x <- expect_is(align_move(m), "MoveStack")
  expect_length(unique(unlist(timeLag(x, units = "secs"))), 1)
})
#}

# add tests for all arguments