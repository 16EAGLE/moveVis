skip_on_cran()
context("view_spatial")

test_that("view_spatial", {
  # correct calls
  expect_is(view_spatial(m), "mapview")
  expect_is(view_spatial(m, render_as = "leaflet"), "leaflet")
  expect_is(view_spatial(m, time_labels = FALSE, path_legend = FALSE), "mapview")
  expect_is(view_spatial(m[[1]], time_labels = FALSE, path_legend = FALSE), "mapview")
  
  # false calls
  expect_error(view_spatial(m, render_as = "abc"))
  expect_error(view_spatial(m, render_as = NA))
  expect_error(view_spatial(NA))
  expect_error(view_spatial(m, path_legend = "1"))
  expect_error(view_spatial(m, time_labels = "1"))
  expect_error(view_spatial(m, path_legend_title = 1))
})