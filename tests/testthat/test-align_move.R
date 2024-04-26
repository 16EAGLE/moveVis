skip_on_cran()
context("align_move")

# if("align_move" %in% which_tests){
test_that("align_move (default)", {
  # correct calls
  x <- expect_is(align_move(m, verbose = F), "MoveStack")
  expect_length(unique(unlist(move::timeLag(x, units = "secs"))), 1)

  x <- expect_is(align_move(m, verbose = F, res = "max"), "MoveStack")
  expect_length(unique(unlist(move::timeLag(x, units = "secs"))), 1)

  x <- expect_is(align_move(m, res = "mean", verbose = F), "MoveStack")
  expect_length(unique(unlist(move::timeLag(x, units = "secs"))), 1)

  # false calls
  expect_error(align_move(NA, verbose = F)) # wrong class
  expect_error(align_move(m, res = FALSE, verbose = F))
  expect_error(align_move(m, unit = "abc", verbose = F))
  expect_error(align_move(move_data, res = 1, unit = "days", verbose = F))

  # warnings
  expect_warning(align_move(m, digit = "max", verbose = F))
  expect_warning(align_move(move_data, res = 13, unit = "hours", verbose = F))
})
# }
