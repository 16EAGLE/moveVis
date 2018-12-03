context("test-get_libraries")

test_that("get_libraries() – all", {
  skip_on_cran()
  expect_true(all(get_libraries() %in% c("convert", "ffmpeg")))
})

test_that("get_libraries() - options", {
  skip_on_cran()
  expect_is(getOption("moveVis.convert_cmd"), "character")
  expect_is(getOption("moveVis.ffmpeg_cmd"), "character")
   
  expect_is(getOption("moveVis.convert_avail"), "logical")
  expect_is(getOption("moveVis.ffmpeg_avail"), "logical")
})