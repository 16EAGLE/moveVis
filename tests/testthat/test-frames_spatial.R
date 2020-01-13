skip_on_cran()
context("frames_spatial")

#if("frames_spatial" %in% which_tests){
test_that("frames_spatial (m not aligned)", {
  expect_error(frames_spatial(m, map_service = "osm", map_type = "streets", map_res = 0.7,
                              map_dir = test_dir, verbose = F))
})

if(isTRUE(test_maps)){
  if(isTRUE(run_mapbox)) test_services <- names(get_maptypes()) else test_services <- grep("mapbox", names(get_maptypes()), value = T, invert = T)
  test_that("frames_spatial (map_types)", {
  
    frames_types <- lapply(test_services, function(service) lapply(get_maptypes(service), function(x, s = service){
      cat(paste0(" ", s, ": ", x, "\n"))
      frames <- frames_spatial(m.aligned, map_service = s, map_type = x, map_token = Sys.getenv("moveVis_map_token"),
                               map_dir = test_dir, verbose = F)
      expect_is(frames, "list")
      expect_length(frames, 180)
      expect_is(frames[[1]], "ggplot")
      return(frames[[100]])
    }))
  })
}

## add plot comparison? expected plot
test_that("frames_spatial (raster, gradient)", {
  frames <- frames_spatial(m.aligned, r_grad, r_times, r_type = "gradient", verbose = F)
  expect_is(frames, "list")
  expect_length(frames, 180)
  expect_is(frames[[1]], "ggplot")
})

test_that("frames_spatial (raster, gradient, fade)", {
  frames <- frames_spatial(m.aligned, r_grad, r_times, r_type = "gradient", fade_raster = T, verbose = F)
  expect_is(frames, "list")
  expect_length(frames, 180)
  expect_is(frames[[1]], "ggplot")
})

test_that("frames_spatial (raster, discrete)", {
  frames <- frames_spatial(m.aligned, r_disc, r_times, r_type = "discrete", verbose = F)
  expect_is(frames, "list")
  expect_length(frames, 180)
  expect_is(frames[[1]], "ggplot")
})
#}
## check special arguments, including ext and path_length

test_that("frames_spatial (different extent/proj settings)", {
  ext <- raster::extent(m)*1.1

  # custom extent
  frames <- frames_spatial(m.aligned, map_service = "osm", map_type = get_maptypes("osm")[1], map_res = 0.1, ext = ext, verbose = F)
  expect_is(frames, "list")
  expect_length(frames, 180)
  expect_is(frames[[1]], "ggplot")

  # equidistant FALSE
  frames <- frames_spatial(m.aligned, map_service = "osm", map_type = get_maptypes("osm")[1], map_res = 0.1, equidistant = F, verbose = F)
  expect_is(frames, "list")
  expect_length(frames, 180)
  expect_is(frames[[1]], "ggplot")

  # equidistant on TRUE
  frames <- frames_spatial(m.aligned, map_service = "osm", map_type = get_maptypes("osm")[1], map_res = 0.1, equidistant = T, ext = ext, verbose = F)
  expect_is(frames, "list")
  expect_length(frames, 180)
  expect_is(frames[[1]], "ggplot")

  # other projections
  frames <- lapply(c("+init=epsg:32632", "+init=epsg:3857"), function(p){
    m <- sp::spTransform(m, raster::crs(p))
    frames <- frames_spatial(m.aligned, map_service = "osm", map_type = get_maptypes("osm")[1], map_res = 0.1, equidistant = F, verbose = F)
    expect_is(frames, "list")
    expect_length(frames, 180)
    expect_is(frames[[1]], "ggplot")
    frames[[100]]
  })
})