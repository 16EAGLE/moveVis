skip_on_cran()
context("subset_move")

# if("df2move" %in% which_tests){
test_that("subset_move", {
  # correct calls
  m.subset <- expect_is(subset_move(m.aligned, from = min(move::timestamps(m.aligned)), to = "2018-05-15 18:00:00"), "MoveStack")
  expect_length(move::timestamps(m.subset), 424)

  # false calls
  expect_error(subset_move(m.aligned, from = "2018-05-15 07:00:00", to = "2018-05-15 18:00:00"))
  expect_error(subset_move(m.aligned, from = "2018-05-15 08:00:00", to = "2019-05-15 18:00:00"))
})
# }
