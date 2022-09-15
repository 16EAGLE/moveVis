skip_on_cran()
context("join_frames")

test_that("join_frames", {
  # correct calls
  f1 <- frames_spatial(m.aligned, r_grad, r_times, r_type = "gradient", verbose = F)[1:10]
  f2 <- frames_graph(m.aligned, r_grad, r_times, graph_type = "hist", verbose = F)[1:10]
  expect_length(expect_is(join_frames(frames_lists = list(f1, f2), verbose = F), "moveVis"), 10)
  
  # false calls
  expect_error(join_frames(frames_lists = list(f1[1:10], f2[1:20]), verbose = F)) # differing lengths
  expect_error(join_frames(frames_lists = list(f1[1:10]), verbose = F)) # nothing to join
})