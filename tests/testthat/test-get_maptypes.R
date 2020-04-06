skip_on_cran()
context("get_maptypes")

#if("get_maptypes" %in% which_tests){
test_that("get_maptypes", {
  x <- expect_is(get_maptypes(), "list")
  expect_is(names(x), "character")
  expect_equal(names(x), c("osm", "carto", "mapbox", "esri"))
  expect_gte(length(x$mapbox), 1)
  expect_gte(length(x$osm), 1)
  expect_equal(grep("satellite", x$mapbox, value = T), "satellite")
  expect_equal(grep("streets", x$osm, value = T)[1], "streets")
})
#}