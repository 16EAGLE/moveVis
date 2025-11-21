skip_on_cran()
context("frames_spatial")

test_that("frames_spatial (default maps)", {
  # correct call
  frames <- expect_length(expect_is(frames_spatial(m = m.aligned, verbose = F, map_res = 0.1), "moveVis"), 188)
  expect_is(frames, "moveVis")
  expect_is(frames, "frames_spatial")
  expect_is(frames[1:10], "moveVis")
  expect_is(frames[[10]], "ggplot")
  
  # false calls
  expect_error(frames_spatial(m.aligned, map_service = "abc", map_res = 0.1, verbose = F)) # false map service
  expect_error(frames_spatial(m.aligned, map_service = "osm", map_type = "light", map_res = 0.1, verbose = F)) # false map service
  expect_error(frames_spatial(m.aligned, map_service = "osm", map_type = "streets", map_res = "abc", verbose = F)) # false map res
  expect_error(frames_spatial(m.aligned, map_service = "osm", map_type = "streets", map_res = -1, verbose = F)) # false map res
  expect_error(frames_spatial(m.aligned, map_service = "mapbox", map_type = "satellite", map_res = 0.1, verbose = F)) # missing token
  expect_error(frames_spatial(m.aligned, map_dir = "abc/abc/abc", map_res = 0.1, verbose = F)) # false map_dir
  expect_error(frames_spatial(m.aligned, path_arrow = "abc", map_res = 0.1, verbose = F)) # false path arrow
  expect_error(frames_spatial(m.aligned, path_colours = "abc", map_res = 0.1, verbose = F)) # false path_colours
  expect_error(frames_spatial(m.aligned, path_legend = "abc", map_res = 0.1, verbose = F)) # false path_legend
  expect_error(frames_spatial(m.aligned, equidistant = "abc", map_res = 0.1, verbose = F)) # false path_legend
})

test_that("frames_spatial maps correct colours to tracks", {
  fr <- frames_spatial(
    m = m.aligned,
    verbose = F,
    map_res = 0.1,
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

test_that("frames_spatial (raster, gradient)", {
  # correct calls
  frames <- expect_length(expect_is(frames_spatial(m.aligned, r = r_grad, r_type = "gradient", verbose = F), "moveVis"), 188)
  expect_is(frames[[1]], "ggplot") # ggplot
  frames <- expect_length(expect_is(frames_spatial(split(m.aligned, mt_track_id(m.aligned))[[1]], r = r_grad, r_type = "gradient", verbose = F), "moveVis"), 143)
  expect_is(frames[[1]], "ggplot") # single move
  
  frames <- expect_length(expect_is(frames_spatial(m.aligned, r = r_grad[[5]],  r_type = "gradient", verbose = F), "moveVis"), 188)
  expect_is(frames[[1]], "ggplot") # single raster
  
  frames <- expect_length(expect_is(frames_spatial(m.aligned, r_grad, r_type = "gradient", path_arrow = grid::arrow(), verbose = F), "moveVis"), 188)
  expect_is(frames[[1]], "ggplot") # path arrow
  frames <- expect_length(expect_is(frames_spatial(m.aligned, r_grad, r_type = "gradient", trace_show = T,  trace_size = 4, trace_colour = "black", verbose = F), "moveVis"), 188)
  expect_is(frames[[1]], "ggplot") # trace_ arguments
  frames <- expect_length(expect_is(frames_spatial(m.aligned, r_grad, r_type = "gradient", tail_length = 25, tail_size = 3, tail_colour = "black", verbose = F), "moveVis"), 188)
  expect_is(frames[[1]], "ggplot") # tail_ arguments
  
  # false calls
  expect_error(frames_spatial(m, r_grad, r_type = "gradient", verbose = F)) # diveriging temporal resolution (m not aligend)
  expect_error(frames_spatial(NA, r_grad, r_type = "gradient", verbose = F)) # false m
  
  r_grad_false_proj <- terra::sds(lapply(r_grad, function(x){
    terra::crs(x) <- terra::crs(sf::st_crs(32632)$wkt)
    return(x)
  }))
  expect_error(frames_spatial(m.aligned, r_grad_false_proj, r_type = "gradient", verbose = F)) # false proj
  
  x <- terra::sds(list(r_grad[[1]], c(r_grad[[2]],  r_grad[[2]])))
  expect_error(frames_spatial(m.aligned, x, r_type = "gradient", verbose = F)) # differing numbers of layers
  
  # downward compatibility
  expect_warning(frames_spatial(m.aligned, r_grad, r_times = r_times, r_type = "gradient", verbose = F)) # defined r_times
  expect_warning(frames_spatial(m.aligned, r_grad, r_list = r_grad, r_times = r_times, r_type = "gradient", verbose = F)) # defined r_times
  expect_warning(frames_spatial(m.aligned, r_list = r_grad, r_times = r_times, r_type = "gradient", verbose = F)) # defined r_times
  
  expect_error(frames_spatial(m.aligned, r_grad, r_type = "abc", verbose = F)) # false r_type
  expect_error(frames_spatial(m.aligned, r_grad, r_type = "gradient", fade_raster = 1, verbose = F)) # false fade_raster
  expect_error(frames_spatial(m.aligned, r_grad, r_type = "gradient", crop_raster = 1, verbose = F)) # false crop_raster
})

test_that("frames_spatial (raster, gradient, fade)", {
  frames <- expect_length(expect_is(frames_spatial(m.aligned, r_grad, r_type = "gradient", fade_raster = T, verbose = F), "moveVis"), 188)
  expect_is(frames[[1]], "ggplot")
})

test_that("frames_spatial (raster, discrete)", {
  frames <-  expect_length(expect_is(frames_spatial(m.aligned, r_disc, r_type = "discrete", verbose = F), "moveVis"), 188)
  expect_is(frames[[1]], "ggplot")
})

# check special arguments, including ext and path_length
test_that("frames_spatial (different extent/proj settings)", {
  ext <- sf::st_bbox(move_data)
  ext[["xmin"]] <- ext[["xmin"]] - (ext[["xmin"]]*0.03)
  ext[["xmax"]] <- ext[["xmax"]] + (ext[["xmax"]]*0.03)

  # custom extent
  frames <- expect_length(expect_is(frames_spatial(m.aligned, map_service = "osm", map_type = get_maptypes("osm")[1], map_res = 0.1, ext = ext, verbose = F), "moveVis"), 188)
  expect_is(frames[[1]], "ggplot")

  # equidistant FALSE
  frames <- expect_length(expect_is(frames_spatial(m.aligned, map_service = "osm", map_type = get_maptypes("osm")[1], map_res = 0.1, equidistant = F, verbose = F), "moveVis"), 188)
  expect_is(frames[[1]], "ggplot")

  # equidistant on TRUE
  frames <- expect_length(expect_is(frames_spatial(m.aligned, map_service = "osm", map_type = get_maptypes("osm")[1], map_res = 0.1, equidistant = T, ext = ext, verbose = F), "moveVis"), 188)
  expect_is(frames[[1]], "ggplot")

  # other projections
  frames <- lapply(list(st_crs(32632), st_crs(3857)), function(p){
    
    # transform using sf
    m_tf <- st_transform(m.aligned, crs = p)
   
    frames <- expect_length(expect_is(frames_spatial(m_tf, map_service = "osm", map_type = get_maptypes("osm")[1], map_res = 0.1, equidistant = F, verbose = F), "moveVis"), 188)
    frames <- expect_length(expect_is(frames_spatial(m.aligned, map_service = "osm", map_type = get_maptypes("osm")[1], map_res = 0.1, equidistant = F, 
                                                     crs = p, verbose = F), "moveVis"), 188)
    expect_is(frames[[1]], "ggplot")
    frames[[100]]
  })
  
  # false calls
  expect_error(frames_spatial(m.aligned, map_res = 0.1, ext = "abc", verbose = F))
  #expect_warning(frames_spatial(m.aligned, map_res = 0.1, ext = raster::extent(m.aligned)*0.1, verbose = F))
  
})

test_that("frames_spatial (cross_dateline)", {
  
  frames <- expect_warning(expect_length(expect_is(frames_spatial(m = m.shifted, map_service = "carto", map_type = "light",
                                                   verbose = F, cross_dateline = T), "moveVis"), 188))
  frames <- expect_length(expect_is(frames_spatial(m = m.shifted, map_service = "carto", map_type = "light",
                                                                  verbose = F, crs = st_crs(4326), cross_dateline = T), "moveVis"), 188)
  frames <- expect_error(frames_spatial(m = m.shifted, r_grad, r_type = "gradient", verbose = F, cross_dateline = T))
})

test_that("frames_spatial can color by track attributes", {
  m <- move2::mutate_track_data(m.aligned, var = c("A", "A", "B"))
  
  fr <- frames_spatial(
    m, 
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

test_that("frames_spatial can color by event attributes", {
  m <- m.aligned
  m[["var"]] <- ifelse(m[["track"]] == "T246a", "A", "B")
  
  # Default
  fr <- frames_spatial(
    m, 
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
    capture.output(frames_spatial(m, colour_paths_by = "foo")),
    "Column 'foo' not found"
  )
})

test_that("User can provide `path_colours` when colouring by attribute", {
  m <- move2::mutate_track_data(
    m.aligned, 
    var = factor(c("A", "A", "B"), levels = c("B", "A"))
  )
  
  # User specified color vector
  fr <- frames_spatial(
    m, 
    verbose = FALSE,
    map_res = 0.1,
    colour_paths_by = "var",
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
  
  fr <- frames_spatial(
    m, 
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
  fr <- frames_spatial(
    m, 
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
  
  fr1 <- frames_spatial(
    m, 
    verbose = FALSE,
    map_res = 0.1,
    colour_paths_by = "var"
  )
  
  built1 <- ggplot2::ggplot_build(fr1[[50]])
  sc1 <- built1$plot$scales$get_scales("colour")
  lims1 <- sc1$get_limits()
  cols1 <- sc1$map(lims1)
  
  m[["var"]] <- factor(m[["var"]], levels = c("B", "A"))
  
  fr2 <- frames_spatial(
    m, 
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

test_that("Can color by continuous attribute", {
  m.aligned[["row"]] <- 1:nrow(m.aligned)
  
  # Default
  fr <- frames_spatial(
    m.aligned,
    verbose = FALSE,
    map_res = 0.1,
    colour_paths_by = "row"
  )
  
  built <- ggplot2::ggplot_build(fr[[50]])
  sc <- built$plot$scales$get_scales("colour")
  
  lims <- sc$get_limits()
  brks <- sc$get_breaks()
  cols <- sc$map(brks)
  
  expect_equal(lims, range(m.aligned[["row"]]))
  expect_equal(brks, seq(0, 500, by = 100))
  expect_equal(
    cols, 
    c("grey50", "#1C4E85", "#008C98", "#00BD7E", "#B4DC3B", "grey50")
  )
  expect_equal(sc$guide, "colourbar")
  
  # With user-specified palette
  fr <- frames_spatial(
    m.aligned,
    verbose = FALSE,
    map_res = 0.1,
    colour_paths_by = "row",
    path_colours = function(x) grDevices::hcl.colors(x, "Blues")
  )
  
  built <- ggplot2::ggplot_build(fr[[50]])
  sc <- built$plot$scales$get_scales("colour")
  
  lims <- sc$get_limits()
  brks <- sc$get_breaks()
  cols <- sc$map(brks)
  
  expect_equal(lims, range(m.aligned[["row"]]))
  expect_equal(brks, seq(0, 500, by = 100))
  expect_equal(
    cols, 
    c("grey50", "#316BB1", "#6F9ECD", "#AACBE3", "#DFEEF7", "grey50")
  )
  expect_equal(sc$guide, "colourbar")
  
  # Can handle units:
  m.aligned[["row"]] <- units::set_units(m.aligned[["row"]], "m/s")
  
  fr2 <- frames_spatial(
    m.aligned,
    verbose = FALSE,
    map_res = 0.1,
    colour_paths_by = "row",
    path_colours = function(x) grDevices::hcl.colors(x, "Blues")
  )
  
  expect_equal(fr$aesthetics, fr2$aesthetics)
  expect_silent(fr2[[1]])
})

test_that("Legend title uses attribute variable", {
  m.aligned[["var"]] <- ifelse(m.aligned[["track"]] == "T246a", "A", "B")
  
  fr <- frames_spatial(
    m.aligned, 
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
