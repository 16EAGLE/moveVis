skip_on_cran()
context("get_frametimes")

test_that("get_frametimes", {
  frames <- frames_spatial(m.aligned, r_grad, r_type = "gradient", verbose = F)
  expect_is(get_frametimes(frames), "POSIXct")
  expect_equal(length(get_frametimes(frames)), length(unique(mt_time(m.aligned))))
})
