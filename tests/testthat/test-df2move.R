skip_on_cran()
context("df2move")

#if("df2move" %in% which_tests){
test_that("df2move", {
  move_df <- expect_is(methods::as(m.aligned), "data.frame")
  expect_is(df2move(move_df, proj = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0", 
                    x = "coords.x1", y = "coords.x2", time = "timestamps", track_id = "trackId"), "MoveStack")
  expect_is(df2move(move_df, proj = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                    x = "coords.x1", y = "coords.x2", time = "timestamps", track_id = "trackId", data = move_df[,c("optional", "sensor")]), "MoveStack")
})
#}