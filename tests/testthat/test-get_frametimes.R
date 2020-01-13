skip_on_cran()
context("get_frametimes")

#if("get_maptypes" %in% which_tests){
test_that("get_frametimes", {
  frames <- frames_spatial(m.aligned, r_grad, r_times, r_type = "gradient", verbose = F)
  expect_is(get_frametimes(frames), "POSIXct")
  expect_equal(length(get_frametimes(frames)), length(unique(move::timestamps(m.aligned))))
})
#}