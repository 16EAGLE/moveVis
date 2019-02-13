context("frames_graph")

if("frames_graph" %in% which_tests){
  test_that("frames_graph (gradient, flow)", {
    frames <- frames_graph(m.aligned, r_grad, r_times, graph_type = "flow", verbose = F)
    expect_is(frames, "list")
    expect_length(frames, 180)
    expect_is(frames[[1]], "ggplot")
  })
  
  test_that("frames_graph (gradient, hist)", {
    frames <- frames_graph(m.aligned, r_grad, r_times, graph_type = "hist", verbose = F)
    expect_is(frames, "list")
    expect_length(frames, 180)
    expect_is(frames[[1]], "ggplot")
  })
  
  
  test_that("frames_graph (discrete, flow)", {
    frames <- frames_graph(m.aligned, r_disc, r_times, r_type = "discrete", graph_type = "flow", verbose = F, val_by = 1)
    expect_is(frames, "list")
    expect_length(frames, 180)
    expect_is(frames[[1]], "ggplot")
  })
  
  test_that("frames_graph (discrete, hist)", {
    frames <- frames_graph(m.aligned, r_disc, r_times, r_type = "discrete", graph_type = "hist", verbose = F, val_by = 1)
    expect_is(frames, "list")
    expect_length(frames, 180)
    expect_is(frames[[1]], "ggplot")
  })
}