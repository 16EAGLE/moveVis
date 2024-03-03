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
  expect_error(frames_spatial(m.aligned, map_service = "abc", verbose = F)) # false map service
  expect_error(frames_spatial(m.aligned, map_service = "osm", map_type = "light", verbose = F)) # false map service
  expect_error(frames_spatial(m.aligned, map_service = "osm", map_type = "streets", map_res = "abc", verbose = F)) # false map res
  expect_error(frames_spatial(m.aligned, map_service = "osm", map_type = "streets", map_res = -1, verbose = F)) # false map res
  expect_error(frames_spatial(m.aligned, map_service = "mapbox", map_type = "satellite", verbose = F)) # missing token
  expect_error(frames_spatial(m.aligned, map_dir = "abc/abc/abc", verbose = F)) # false map_dir
  expect_error(frames_spatial(m.aligned, path_arrow = "abc", verbose = F)) # false path arrow
  expect_error(frames_spatial(m.aligned, path_colours = "abc", verbose = F)) # false path_colours
  expect_error(frames_spatial(m.aligned, path_legend = "abc", verbose = F)) # false path_legend
  expect_error(frames_spatial(m.aligned, equidistant = "abc", verbose = F)) # false path_legend
})

test_that("frames_spatial (raster, gradient)", {
  # correct calls
  frames <- expect_length(expect_is(frames_spatial(m.aligned, r_grad, r_times, r_type = "gradient", verbose = F), "moveVis"), 188)
  expect_is(frames[[1]], "ggplot") # move stack
  frames <- expect_length(expect_is(frames_spatial(m.aligned[[1]], r_grad, r_times, r_type = "gradient", verbose = F), "moveVis"), 143)
  expect_is(frames[[1]], "ggplot") # single move
  frames <- expect_length(expect_is(frames_spatial(m.aligned, r_grad[[5]], r_times[[5]], r_type = "gradient", verbose = F), "moveVis"), 188)
  expect_is(frames[[1]], "ggplot") # single raster
  frames <- expect_length(expect_is(frames_spatial(m.aligned, r_grad, r_times, r_type = "gradient", path_arrow = grid::arrow(), verbose = F), "moveVis"), 188)
  expect_is(frames[[1]], "ggplot") # path arrow
  frames <- expect_length(expect_is(frames_spatial(m.aligned, r_grad, r_times, r_type = "gradient", trace_show = T,  trace_size = 4, trace_colour = "black", verbose = F), "moveVis"), 188)
  expect_is(frames[[1]], "ggplot") # trace_ arguments
  frames <- expect_length(expect_is(frames_spatial(m.aligned, r_grad, r_times, r_type = "gradient", tail_length = 25, tail_size = 3, tail_colour = "black", verbose = F), "moveVis"), 188)
  expect_is(frames[[1]], "ggplot") # tail_ arguments
  
  
  # false calls
  expect_error(frames_spatial(m, r_grad, r_times, r_type = "gradient", verbose = F)) # diveriging temporal resolution (m not aligend)
  expect_error(frames_spatial(NA, r_grad, r_times, r_type = "gradient", verbose = F)) # false m
  
  x <- r_grad[[1]]
  raster::crs(x) <- raster::crs("+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  expect_error(frames_spatial(m.aligned, list(x), r_times, r_type = "gradient", verbose = F)) # false proj
  
  x <- list(r_grad[[1]], raster::stack( r_grad[[2]],  r_grad[[2]]))
  expect_error(frames_spatial(m.aligned, x, r_times, r_type = "gradient", verbose = F)) # differing numbers of layers
  
  expect_error(frames_spatial(m.aligned, r_grad, as.character(r_times), r_type = "gradient", verbose = F)) # false r_times
  expect_error(frames_spatial(m.aligned, r_grad, r_times, r_type = "abc", verbose = F)) # false r_type
  expect_error(frames_spatial(m.aligned, r_grad, r_times, r_type = "gradient", fade_raster = 1, verbose = F)) # false fade_raster
  expect_error(frames_spatial(m.aligned, r_grad, r_times, r_type = "gradient", crop_raster = 1, verbose = F)) # false crop_raster
})

test_that("frames_spatial (raster, gradient, fade)", {
  frames <- expect_length(expect_is(frames_spatial(m.aligned, r_grad, r_times, r_type = "gradient", fade_raster = T, verbose = F), "moveVis"), 188)
  expect_is(frames[[1]], "ggplot")
})

test_that("frames_spatial (raster, discrete)", {
  frames <-  expect_length(expect_is(frames_spatial(m.aligned, r_disc, r_times, r_type = "discrete", verbose = F), "moveVis"), 188)
  expect_is(frames[[1]], "ggplot")
})
#}
## check special arguments, including ext and path_length

test_that("frames_spatial (different extent/proj settings)", {
  ext <- raster::extent(m)*1.1

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
  frames <- lapply(c("+init=epsg:32632", "+init=epsg:3857"), function(p){
    
    # transform using sf
    m_tf <- sf::st_transform(sf::st_as_sf(m), sf::st_crs(p))
    m_tf <- cbind.data.frame(sf::st_coordinates(m_tf), time = m_tf$time, id = move::trackId(m))
    m <- quiet(df2move(m_tf, proj = p, x = "X", y = "Y", time = "time", track_id = "id")) # warnings are exected sometimes
    
    frames <- expect_length(expect_is(frames_spatial(m.aligned, map_service = "osm", map_type = get_maptypes("osm")[1], map_res = 0.1, equidistant = F, verbose = F), "moveVis"), 188)
    expect_is(frames[[1]], "ggplot")
    frames[[100]]
  })
  
  # false calls
  expect_error(frames_spatial(m.aligned, map_res = 0.1, ext = "abc", verbose = F))
  #expect_warning(frames_spatial(m.aligned, map_res = 0.1, ext = raster::extent(m.aligned)*0.1, verbose = F))
  
})

test_that("frames_spatial (cross_dateline)", {
  
  frames <- expect_length(expect_is(frames_spatial(m = m.shifted, map_service = "carto", map_type = "light",
                                                   verbose = F, cross_dateline = T), "moveVis"), 188)
  frames <- expect_warning(frames_spatial(m= m.shifted.repro, verbose = F, cross_dateline = T))
  frames <- expect_error(frames_spatial(m = m.shifted, r_list = r_grad, r_times = r_times, r_type = "gradient", verbose = F, cross_dateline = T))
  
})