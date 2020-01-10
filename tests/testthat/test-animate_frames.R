skip_on_cran()
context("animate_frames")

test_that("animate_frames", {
  frames <- frames_spatial(m.aligned, r_grad, r_times, r_type = "gradient", verbose = F)[1:10]
  file.gif <- tempfile(tmpdir = test_dir, fileext = ".gif")
  expect_null(animate_frames(frames, out_file = file.gif, verbose = F, overwrite = T, display = F))
  
  file.mov <- tempfile(tmpdir = test_dir, fileext = ".mov")
  expect_null(animate_frames(frames, out_file = file.mov, verbose = F, overwrite = T, display = F))
})