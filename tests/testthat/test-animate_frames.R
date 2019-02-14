context("animate_frames")

test_that("animate_frames", {
  frames <- frames_spatial(m.aligned, r_grad, r_times, r_type = "gradient", verbose = F)[1:10]
  file.gif <- tempfile(tmpdir = test_dir, fileext = ".gif")
  expect_null(animate_frames(frames, file.gif, verbose = F, overwrite = T, display = F))
  
  file.mov <- tempfile(tmpdir = test_dir, fileext = ".mov")
  expect_null(animate_frames(frames, file.mov, verbose = F, overwrite = T, display = F))
})