skip_on_cran()
context("frames_graph")

#if("frames_graph" %in% which_tests){
test_that("frames_graph (gradient, flow)", {
  # correct calls
  frames <- expect_length(expect_is(frames_graph(m.aligned, r_grad, graph_type = "flow", verbose = F), "moveVis"), 188) # multi raster
  expect_is(frames[[1]], "ggplot")
  
  frames <- expect_length(expect_is(frames_graph(m.aligned, r_grad[[5]], graph_type = "flow", verbose = F), "moveVis"), 188) # single raster
  expect_is(frames[[1]], "ggplot")
  
  expect_is(frames_graph(m.aligned, r_grad, graph_type = "flow", return_data = T, verbose = F), "data.frame") # return data.frame
  
  # false calls
  expect_error(frames_graph(NA, r_grad, graph_type = "flow", verbose = F)) # no move
  expect_error(frames_graph(m.aligned, r_grad, r_type = NA, verbose = F)) # false r_type
  expect_error(frames_graph(m.aligned, r_grad, r_type = "abc", verbose = F)) # false r_type
  expect_error(frames_graph(m.aligned, list(NA), graph_type = "flow", verbose = F)) # false r
  
  # warning due to deprecation
  expect_warning(frames_graph(m.aligned, r_grad, r_times = terra::time(r_grad), graph_type = "flow", verbose = F))
  expect_warning(frames_graph(m.aligned, r_grad, r_list = as.list(r_grad), r_times = terra::time(r_grad), graph_type = "flow", verbose = F))
  
  r_grad_false_proj <- terra::sds(lapply(r_grad, function(x){
    terra::crs(x) <- terra::crs(sf::st_crs(32632)$wkt)
    return(x)
  }))
  
  expect_error(frames_graph(m.aligned, r_grad_false_proj, graph_type = "flow", verbose = F)) # false proj leads to non-overlapping extents after auto-reproject
  
  x <- terra::sds(list(r_grad[[1]], c(r_grad[[2]],  r_grad[[2]])))
  expect_error(frames_graph(m.aligned, x, graph_type = "flow", verbose = F)) # differing numbers of layers
  
  time(r_grad) <- NULL
  expect_error(frames_graph(m.aligned, r_grad, graph_type = "flow", verbose = F)) # no times
  
  time(r_grad) <- r_times
  expect_error(frames_graph(m.aligned, r_grad, graph_type = "flow", fade_raster = 1, verbose = F)) # false fade_raster
  expect_error(frames_graph(m.aligned, r_grad, graph_type = "flow", path_size = "1", verbose = F))
  expect_error(frames_graph(m.aligned, r_grad, graph_type = "flow", path_legend = "1", verbose = F))
  expect_error(frames_graph(m.aligned, r_grad, graph_type = "flow", path_legend_title = 1, verbose = F))
  expect_error(frames_graph(m.aligned, r_grad, graph_type = "flow", return_data = 1, verbose = F))
  expect_error(frames_graph(m.aligned, r_grad, graph_type = NA, verbose = F)) # false graph type
  expect_error(frames_graph(m.aligned, r_grad, graph_type = "x", verbose = F)) # false graph type
})

test_that("frames_graph (gradient, hist)", {
  # correct calls
  frames <- expect_length(expect_is(frames_graph(m.aligned, r_grad, graph_type = "hist", verbose = F), "moveVis"), 188)
  expect_is(frames[[1]], "ggplot")
  
  # false calls
  expect_error(frames_graph(m.aligned, r_grad, graph_type = "hist", val_min = "1", verbose = F)) # false val_min
  expect_error(frames_graph(m.aligned, r_grad, graph_type = "hist", val_max = "1", verbose = F)) # false val_max
  expect_error(frames_graph(m.aligned, r_grad, graph_type = "hist", val_by = "1", verbose = F)) # false val_by
})


test_that("frames_graph (discrete, flow)", {
  frames <- expect_length(expect_is(frames_graph(m.aligned, r_disc, r_type = "discrete", graph_type = "flow", verbose = F, val_by = 1), "moveVis"), 188)
  expect_is(frames[[1]], "ggplot")
  
  # warning calls
  expect_warning(frames_graph(m.aligned, r_grad, r_type = "discrete", fade_raster = T, val_by = 1, verbose = F))
  expect_warning(frames_graph(m.aligned, r_grad, r_type = "discrete", fade_raster = F, verbose = F))
})

test_that("frames_graph (discrete, hist)", {
  frames <- expect_length(expect_is(frames_graph(m.aligned, r_disc, r_type = "discrete", graph_type = "hist", verbose = F, val_by = 1), "moveVis"), 188)
  expect_is(frames[[1]], "ggplot")
})
#}

test_that("frames_graph maps correct colours to tracks (flow)", {
  fr <- frames_graph(
    m = m.aligned,
    r_grad,
    graph_type = "flow",
    verbose = F,
    path_colours = c("#F2A08F", "#65A7C9", "#461A6B")
  )
  
  built <- ggplot2::ggplot_build(fr[[50]])
  sc <- built$plot$scales$get_scales("colour")
  
  lims <- sc$get_limits()
  cols <- sc$map(lims)
  
  pal <- setNames(cols, lims)
  
  expect_equal(pal[["T246a"]],  "#F2A08F")
  expect_equal(pal[["T342g"]],  "#65A7C9")
  expect_equal(pal[["T932u"]],  "#461A6B")
})

test_that("frames_graph maps correct colours to tracks (hist)", {
  fr <- frames_graph(
    m = m.aligned,
    r_grad,
    graph_type = "hist",
    verbose = F,
    path_colours = c("#F2A08F", "#65A7C9", "#461A6B")
  )
  
  built <- ggplot2::ggplot_build(fr[[50]])
  sc <- built$plot$scales$get_scales("colour")
  
  lims <- sc$get_limits()
  cols <- sc$map(lims)
  
  pal <- setNames(cols, lims)
  
  expect_equal(pal[["T246a"]],  "#F2A08F")
  expect_equal(pal[["T342g"]],  "#65A7C9")
  expect_equal(pal[["T932u"]],  "#461A6B")
})

test_that("frames_graph can color by track attributes", {
  m <- move2::mutate_track_data(m.aligned, var = c("A", "A", "B"))
  
  fr <- frames_graph(
    m, 
    r_grad,
    verbose = FALSE,
    map_res = 0.1,
    colour_paths_by = "var"
  )
  
  built <- ggplot2::ggplot_build(fr[[50]])
  sc <- built$plot$scales$get_scales("colour")
  lims <- sc$get_limits()
  cols <- sc$map(lims)
  
  expect_equal(lims, c("A", "B"))
  expect_equal(cols, .standard_colours(2))
})

test_that("frames_graph can color by event attributes", {
  m <- m.aligned
  m[["var"]] <- ifelse(m[["track"]] == "T246a", "A", "B")
  
  fr <- frames_graph(
    m, 
    r_grad,
    verbose = FALSE,
    map_res = 0.1,
    colour_paths_by = "var"
  )
  
  built <- ggplot2::ggplot_build(fr[[50]])
  sc <- built$plot$scales$get_scales("colour")
  lims <- sc$get_limits()
  cols <- sc$map(lims)
  
  expect_equal(lims, unique(m[["var"]]))
  expect_equal(cols, .standard_colours(2))
  
  expect_error(
    frames_graph(m.aligned, r_grad, verbose = FALSE, colour_paths_by = "foo"),
    "Column 'foo' not found"
  )
})

test_that("User can provide `path_colours` when colouring by attribute", {
  m <- move2::mutate_track_data(
    m.aligned, 
    var = factor(c("A", "A", "B"), levels = c("B", "A"))
  )
  
  # User specified color vector
  fr <- frames_graph(
    m, 
    r_grad,
    verbose = FALSE,
    map_res = 0.1,
    colour_paths_by = "var",
    graph_type = "hist",
    path_colours = c("#F2A08F", "#65A7C9")
  )
  
  built <- ggplot2::ggplot_build(fr[[50]])
  sc <- built$plot$scales$get_scales("colour")
  lims <- sc$get_limits()
  cols <- sc$map(lims)
  
  expect_equal(lims, c("B", "A"))
  expect_equal(cols, c("#F2A08F", "#65A7C9"))
})

test_that("path_colours accepts palette function", {
  m <- m.aligned
  m[["var"]] <- ifelse(m[["track"]] == "T246a", "A", "B")
  
  pal <- function(x) grDevices::hcl.colors(x, palette = "viridis")
  
  fr <- frames_graph(
    m, 
    r_grad,
    verbose = FALSE,
    map_res = 0.1,
    path_colours = pal
  )
  
  built <- ggplot2::ggplot_build(fr[[50]])
  sc <- built$plot$scales$get_scales("colour")
  lims <- sc$get_limits()
  cols <- sc$map(lims)
  
  expect_equal(lims, levels(move2::mt_track_id(m)))
  expect_equal(cols, pal(3))
  
  # Palette adjusts to number of levels in coloring variable
  fr <- frames_graph(
    m, 
    r_grad,
    verbose = FALSE,
    map_res = 0.1,
    colour_paths_by = "var",
    path_colours = pal
  )
  
  built <- ggplot2::ggplot_build(fr[[50]])
  sc <- built$plot$scales$get_scales("colour")
  lims <- sc$get_limits()
  cols <- sc$map(lims)
  
  expect_equal(lims, unique(m[["var"]]))
  expect_equal(cols, pal(length(unique(m[["var"]]))))
})

test_that("Coloring by attributes orders correctly for factor vs. character", {
  m <- m.aligned
  m[["var"]] <- ifelse(m[["track"]] == "T246a", "A", "B")
  
  fr1 <- frames_graph(
    m, 
    r_grad,
    verbose = FALSE,
    map_res = 0.1,
    colour_paths_by = "var"
  )
  
  built1 <- ggplot2::ggplot_build(fr1[[50]])
  sc1 <- built1$plot$scales$get_scales("colour")
  lims1 <- sc1$get_limits()
  cols1 <- sc1$map(lims1)
  
  m[["var"]] <- factor(m[["var"]], levels = c("B", "A"))
  
  fr2 <- frames_graph(
    m, 
    r_grad,
    verbose = FALSE,
    map_res = 0.1, 
    colour_paths_by = "var"
  )
  
  built2 <- ggplot2::ggplot_build(fr2[[50]])
  sc2 <- built2$plot$scales$get_scales("colour")
  lims2 <- sc2$get_limits()
  cols2 <- sc2$map(lims2)
  
  expect_equal(lims1, rev(lims2))
  expect_equal(cols1, cols2)
})

test_that("Error when coloring by continuous attribute", {
  m <- m.aligned
  m[["var"]] <- 1:nrow(m)
  
  expect_error(
    frames_graph(
      m,
      r_grad,
      verbose = FALSE,
      colour_paths_by = "var"
    ),
    "Cannot color by continuous variables"
  )
})

test_that("Legend title uses attribute variable", {
  m <- m.aligned
  m[["var"]] <- ifelse(m[["track"]] == "T246a", "A", "B")
  
  fr <- frames_graph(
    m, 
    r_grad,
    verbose = FALSE,
    map_res = 0.1,
    colour_paths_by = "var"
  )
  
  built <- ggplot2::ggplot_build(fr[[50]])
  gt <- ggplot2::ggplot_gtable(built)
  
  grobs <- gt$grobs[[15]]$grobs[[1]]$grobs
  i <- which(sapply(grobs, function(x) grepl("guide.title.titleGrob", x$name)))
  
  # This isn't super robust, but difficult to fully automate checking
  # the ggplot2 internals. In the future a snapshot test would likely be
  # more effective.
  expect_equal(grobs[[i]]$children[[1]]$label, "var")
})

