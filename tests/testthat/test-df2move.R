skip_on_cran()
context("df2move")

# if("df2move" %in% which_tests){
test_that("df2move (stack)", {
  # correct calls
  move_df <- expect_is(methods::as(m.aligned, "data.frame"), "data.frame")
  expect_is(df2move(move_df,
    proj = raster::crs("+init=epsg:4326"),
    x = "coords.x1", y = "coords.x2", time = "timestamps", track_id = "trackId"
  ), "MoveStack") # test without data
  expect_is(df2move(move_df,
    proj = raster::crs("+init=epsg:4326"),
    x = "coords.x1", y = "coords.x2", time = "timestamps", track_id = "trackId", data = move_df[, c("optional", "sensor")]
  ), "MoveStack") # test with data
  expect_is(df2move(move_df[move_df$trackId == "T246a", ],
    proj = raster::crs("+init=epsg:4326"),
    x = "coords.x1", y = "coords.x2", time = "timestamps", track_id = "trackId"
  ), "Move") # test with track_id but only one individual

  # false calls
  expect_error(df2move(NA,
    proj = raster::crs("+init=epsg:4326"),
    x = "coords.x1", y = "coords.x2", time = "timestamps", track_id = "trackId"
  )) # df is NA
  expect_error(df2move(move_df,
    proj = "abcdef",
    x = "coords.x1", y = "coords.x2", time = "timestamps", track_id = "trackId"
  )) # false proj
  expect_error(df2move(move_df,
    proj = raster::crs("+init=epsg:4326"),
    x = "coords.x1", y = "coords.x2", time = "timestamps", track_id = "trackId", data = move_df[1:10, c("optional", "sensor")]
  )) # data not long enough
  expect_error(df2move(move_df,
    proj = raster::crs("+init=epsg:4326"),
    x = "coords.x1", y = "coords.x2", time = "coords.x2", track_id = "trackId"
  )) # time not POSIXct
  expect_error(df2move(move_df,
    proj = raster::crs("+init=epsg:4326"),
    x = "coords.x1", y = "coords.x2", time = "timestamps", track_id = "abc"
  )) # track id not existing
})

test_that("df2move (single)", {
  move_df <- expect_is(methods::as(m.aligned[[1]], "data.frame"), "data.frame")
  expect_is(df2move(move_df,
    proj = raster::crs("+init=epsg:4326"),
    x = "coords.x1", y = "coords.x2", time = "timestamps"
  ), "Move")
  expect_is(df2move(move_df,
    proj = raster::crs("+init=epsg:4326"),
    x = "coords.x1", y = "coords.x2", time = "timestamps", data = move_df[, c("optional", "sensor")]
  ), "Move")
})
# }
