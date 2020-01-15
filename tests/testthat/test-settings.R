skip_on_cran()
context("settings")

test_that("use_multicore", {
  expect_output(use_multicore(n_cores = 2, verbose = T))
  expect_equal(getOption("moveVis.n_cores"), 2)
  use_multicore(n_cores = 1, verbose = F)
})

test_that("use_disk", {
  expect_output(use_disk(frames_to_disk = TRUE, n_memory_frames = NULL))
  expect_is(frames_spatial(m = m.aligned[move::timestamps(m.aligned) > as.POSIXct("2018-05-15 07:00:00") & move::timestamps(m.aligned) < as.POSIXct("2018-05-15 10:00:00"),],
                           r_list = r_grad, r_times = r_times, r_type = "gradient", verbose = F, fade_raster = T), "list")
  use_disk(frames_to_disk = FALSE, verbose = F)
})