skip_on_cran()
context("frames_graph")

# if("frames_graph" %in% which_tests){
test_that("frames_graph (gradient, flow)", {
  # correct calls
  frames <- expect_length(expect_is(frames_graph(m.aligned, r_grad, r_times, graph_type = "flow", verbose = F), "moveVis"), 188) # multi raster
  expect_is(frames[[1]], "ggplot")

  frames <- expect_length(expect_is(frames_graph(m.aligned, r_grad[[5]], r_times[[5]], graph_type = "flow", verbose = F), "moveVis"), 188) # single raster
  expect_is(frames[[1]], "ggplot")

  expect_is(frames_graph(m.aligned, r_grad, r_times, graph_type = "flow", return_data = T, verbose = F), "data.frame") # return data.frame

  # false calls
  expect_error(frames_graph(NA, r_grad, r_times, graph_type = "flow", verbose = F)) # no move
  expect_error(frames_graph(m.aligned, r_grad, r_times, r_type = NA, verbose = F)) # false r_type
  expect_error(frames_graph(m.aligned, r_grad, r_times, r_type = "abc", verbose = F)) # false r_type
  expect_error(frames_graph(m.aligned, list(NA), r_times, graph_type = "flow", verbose = F)) # false r_list

  x <- r_grad[[1]]
  raster::crs(x) <- raster::crs("+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  expect_error(frames_graph(m.aligned, list(x), r_times, graph_type = "flow", verbose = F)) # false proj

  x <- list(r_grad[[1]], raster::stack(r_grad[[2]], r_grad[[2]]))
  expect_error(frames_graph(m.aligned, x, r_times, graph_type = "flow", verbose = F)) # differing numbers of layers

  expect_error(frames_graph(m.aligned, r_grad, as.character(r_times), graph_type = "flow", verbose = F)) # false r_times
  expect_error(frames_graph(m.aligned, r_grad, r_times, graph_type = "flow", fade_raster = 1, verbose = F)) # false fade_raster
  expect_error(frames_graph(m.aligned, r_grad, r_times, graph_type = "flow", path_size = "1", verbose = F))
  expect_error(frames_graph(m.aligned, r_grad, r_times, graph_type = "flow", path_legend = "1", verbose = F))
  expect_error(frames_graph(m.aligned, r_grad, r_times, graph_type = "flow", path_legend_title = 1, verbose = F))
  expect_error(frames_graph(m.aligned, r_grad, r_times, graph_type = "flow", return_data = 1, verbose = F))
  expect_error(frames_graph(m.aligned, r_grad, r_times, graph_type = NA, verbose = F)) # false graph type
  expect_error(frames_graph(m.aligned, r_grad, r_times, graph_type = "x", verbose = F)) # false graph type
})

test_that("frames_graph (gradient, hist)", {
  # correct calls
  frames <- expect_length(expect_is(frames_graph(m.aligned, r_grad, r_times, graph_type = "hist", verbose = F), "moveVis"), 188)
  expect_is(frames[[1]], "ggplot")

  # false calls
  expect_error(frames_graph(m.aligned, r_grad, r_times, graph_type = "hist", val_min = "1", verbose = F)) # false val_min
  expect_error(frames_graph(m.aligned, r_grad, r_times, graph_type = "hist", val_max = "1", verbose = F)) # false val_max
  expect_error(frames_graph(m.aligned, r_grad, r_times, graph_type = "hist", val_by = "1", verbose = F)) # false val_by
})


test_that("frames_graph (discrete, flow)", {
  frames <- expect_length(expect_is(frames_graph(m.aligned, r_disc, r_times, r_type = "discrete", graph_type = "flow", verbose = F, val_by = 1), "moveVis"), 188)
  expect_is(frames[[1]], "ggplot")

  # warning calls
  expect_warning(frames_graph(m.aligned, r_grad, r_times, r_type = "discrete", fade_raster = T, val_by = 1, verbose = F))
  expect_warning(frames_graph(m.aligned, r_grad, r_times, r_type = "discrete", fade_raster = F, verbose = F))
})

test_that("frames_graph (discrete, hist)", {
  frames <- expect_length(expect_is(frames_graph(m.aligned, r_disc, r_times, r_type = "discrete", graph_type = "hist", verbose = F, val_by = 1), "moveVis"), 188)
  expect_is(frames[[1]], "ggplot")
})
# }
