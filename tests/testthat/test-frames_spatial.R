skip_on_cran()
context("frames_spatial")

#if("frames_spatial" %in% which_tests){
if(isTRUE(run_mapbox)){
  test_that("frames_spatial (mapbox, satellite, not aligned)", {
    expect_error(frames_spatial(m, map_service = "mapbox", map_type = "satellite", map_res = 0.7,
                                map_token = map_token, map_dir = test_dir, verbose = F))
  })
  
  test_that("frames_spatial (mapbox, satellite)", {
    frames <- frames_spatial(m.aligned, map_service = "mapbox", map_type = "satellite", map_res = 0.7,
                             map_token = map_token, map_dir = test_dir, verbose = F)
    expect_is(frames, "list")
    expect_length(frames, 180)
    expect_is(frames[[1]], "ggplot")
  })
}

test_that("frames_spatial (osm, streets)", {
  frames <- frames_spatial(m.aligned, map_service = "osm", map_type = "streets", map_res = 0.7, map_dir = test_dir, verbose = F)
  expect_is(frames, "list")
  expect_length(frames, 180)
  expect_is(frames[[1]], "ggplot")
})

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