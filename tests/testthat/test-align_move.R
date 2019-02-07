context("align_move")

test_that("align_move (default)", {
  x <- expect_is(align_move(m), "MoveStack")
  expect_length(unique(unlist(timeLag(x, units = "secs"))), 1)
})

# add tests for all arguments