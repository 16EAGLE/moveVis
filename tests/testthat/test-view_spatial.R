skip_on_cran()
context("view_spatial")

test_that("view_spatial", {
  expect_is(view_spatial(m), "mapview")
  expect_is(view_spatial(m, render_as = "leaflet"), "leaflet")
  expect_is(view_spatial(m, time_labels = FALSE, path_legend = FALSE), "mapview")
})