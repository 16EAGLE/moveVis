context("frames_spatial")

if(isTRUE(run_mapbox)){
  test_that("frames_spatial (mapbox, satellite, not aligned)", {
    expect_error(frames_spatial(m, map_service = "mapbox", map_type = "satellite", map_res = 0.7,
                                map_token = map_token, map_dir = test_dir, verbose = F))
  })
  
  test_that("frames_spatial (mapbox, satellite)", {
    frames <- frames_spatial(m.aligned, map_service = "mapbox", map_type = "satellite", map_res = 0.7,
                             map_token = map_token, map_dir = test_dir, verbose = F)
    expect_is(frames, "list")
    expect_length(frames, 517)
    expect_is(frames[[1]], "ggplot")
  })
}

test_that("frames_spatial (osm, streets)", {
  frames <- frames_spatial(m.aligned, map_service = "osm", map_type = "streets", map_res = 0.7, map_dir = test_dir, verbose = F)
  expect_is(frames, "list")
  expect_length(frames, 517)
  expect_is(frames[[1]], "ggplot")
})