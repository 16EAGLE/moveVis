skip_on_cran()
context("animate_frames")

test_that("animate_frames", {
  # correct calls
  frames <- frames_spatial(m.aligned, r_grad, r_times, r_type = "gradient", verbose = F)[1:10]
  file.gif <- tempfile(tmpdir = test_dir, fileext = ".gif")
  expect_null(animate_frames(frames, out_file = file.gif, verbose = F, overwrite = T, display = F))
  
  file.mov <- tempfile(tmpdir = test_dir, fileext = ".mov")
  expect_null(animate_frames(frames, out_file = file.mov, verbose = F, overwrite = T, display = T))
  
  # end pause
  expect_null(animate_frames(frames, out_file = file.mov, verbose = F, overwrite = T, display = F, end_pause = 2))
  
  # false calls
  expect_error(animate_frames(frames, out_file = file.mov, verbose = F, overwrite = F, display = F)) # overwrite error
  
  file.mov <- tempfile(tmpdir = test_dir, fileext = ".mov")
  expect_error(animate_frames(NA, out_file = file.mov, verbose = F, overwrite = T, display = F)) # wrong frames
  expect_error(animate_frames(list(frames[[1]], NA), out_file = file.mov, verbose = F, overwrite = T, display = F)) #wrong frames
  expect_error(animate_frames(frames, out_file = FALSE, verbose = F, overwrite = T, display = F)) # false out_file
  expect_error(animate_frames(frames, out_file = paste0(paste0(c(tail(strsplit(file.mov, "/")[[1]], n=-1), "three", "more", "folders"), collapse = "/"), "xyz.mov"), verbose = F, overwrite = T, display = F)) # false out_file
})