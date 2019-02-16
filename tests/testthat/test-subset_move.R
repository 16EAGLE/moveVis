skip_on_cran()
context("subset_move")

#if("df2move" %in% which_tests){
test_that("subset_move", {
  m.subset <- expect_is(subset_move(m.aligned, from = "2018-05-15 07:00:00", to = "2018-05-15 18:00:00"), "MoveStack")
  expect_length(move::timestamps(m.subset), 424)
  
  m.subset <- expect_is(subset_move(m.aligned, from = as.POSIXct("2018-05-15 07:00:00", tz = "UTC"), to = as.POSIXct("2018-05-15 18:00:00", tz = "UTC")), "MoveStack")
  expect_length(move::timestamps(m.subset), 424)
})
#}