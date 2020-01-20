skip_on_cran()
context("align_move")

#if("align_move" %in% which_tests){
test_that("align_move (default)", {
  # correct calls
  x <- expect_is(align_move(m), "MoveStack")
  expect_length(unique(unlist(move::timeLag(x, units = "secs"))), 1)
  
  x <- expect_is(align_move(m, res = "max"), "MoveStack")
  expect_length(unique(unlist(move::timeLag(x, units = "secs"))), 1)
  
  x <- expect_is(align_move(m, res = "mean"), "MoveStack")
  expect_length(unique(unlist(move::timeLag(x, units = "secs"))), 1)
  
  x <- expect_is(align_move(m, digit = "max"), "MoveStack")
  expect_length(unique(unlist(move::timeLag(x, units = "secs"))), 1)
  
  x <- expect_is(align_move(m, digit = "mean"), "MoveStack")
  expect_length(unique(unlist(move::timeLag(x, units = "secs"))), 1)
  
  # false calls
  expect_error(align_move(NA)) # wrong class
  expect_error(align_move(m, res = FALSE))
  expect_error(align_move(m, digit = FALSE))
  expect_error(align_move(m, unit = "abc"))
})
#}
