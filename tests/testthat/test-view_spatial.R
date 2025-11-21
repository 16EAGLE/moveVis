skip_on_cran()
context("view_spatial")

test_that("view_spatial", {
  
  # correct calls
  if(isTRUE(check_mapview)){
    expect_is(view_spatial(m), "mapview")
    expect_is(view_spatial(m, time_labels = FALSE, path_legend = FALSE), "mapview")
  }
  
  if(isTRUE(check_leaflet)) expect_is(view_spatial(m, render_as = "leaflet"), "leaflet") else expect_error(view_spatial(m))
  
  # false calls
  expect_error(view_spatial(m, render_as = "abc"))
  expect_error(view_spatial(m, render_as = NA))
  expect_error(view_spatial(NA))
  expect_error(view_spatial(m, path_colours = "foo"))
  expect_error(view_spatial(m, path_legend = "1"))
  expect_error(view_spatial(m, time_labels = "1"))
  expect_error(view_spatial(m, path_legend_title = 1))
})

test_that("view_spatial() colours tracks correctly by track ID (default)", {
  skip_if_not_installed("mapview")
  
  v <- view_spatial(m)
  
  calls <- v@map$x$calls
  
  i <- sapply(calls, function(x) x$method == "addLegend")
  expect_equal(
    unclass(calls[i][[1]]$args[[1]]$colors),
    c("#FF0000", "#00FF00", "#0000FF")
  )
  expect_equal(
    unclass(calls[i][[1]]$args[[1]]$labels),
    c("T246a", "T342g", "T932u")
  )
})

test_that("view_spatial() colours tracks correctly by track ID: leaflet", {
  skip_if_not_installed("leaflet")
  
  v <- view_spatial(m, render_as = "leaflet")
  
  calls <- v$x$calls
  
  i <- sapply(calls, function(x) x$method == "addLegend")
  expect_equal(
    unclass(calls[i][[1]]$args[[1]]$colors),
    c("#FF0000", "#00FF00", "#0000FF")
  )
  expect_equal(
    unclass(calls[i][[1]]$args[[1]]$labels),
    c("T246a", "T342g", "T932u")
  )
})

test_that("view_spatial() can colour tracks by variable", {
  skip_if_not_installed("mapview")
  
  m2 <- m
  m2[["var"]] <- ifelse(m2[["track"]] == "T246a", "A", "B")
  
  v <- view_spatial(
    m2, 
    path_colours = c("#F2A08F", "#65A7C9"), 
    colour_paths_by = "var"
  )
  
  calls <- v@map$x$calls
  
  i <- sapply(calls, function(x) x$method == "addLegend")
  expect_equal(
    unclass(calls[i][[1]]$args[[1]]$colors),
    c("#F2A08F", "#65A7C9")
  )
  expect_equal(
    unclass(calls[i][[1]]$args[[1]]$labels),
    c("A", "B")
  )
})

test_that("view_spatial() can colour tracks by variable: leaflet", {
  skip_if_not_installed("leaflet")
  
  m2 <- m
  m2[["var"]] <- ifelse(m2[["track"]] == "T246a", "A", "B")
  
  v <- view_spatial(
    m2, 
    render_as = "leaflet",
    path_colours = c("#F2A08F", "#65A7C9"), 
    colour_paths_by = "var"
  )
  
  calls <- v$x$calls
  
  i <- sapply(calls, function(x) x$method == "addLegend")
  expect_equal(
    unclass(calls[i][[1]]$args[[1]]$colors),
    c("#F2A08F", "#65A7C9")
  )
  expect_equal(
    unclass(calls[i][[1]]$args[[1]]$labels),
    c("A", "B")
  )
})

test_that("view_spatial() can colour tracks by variable", {
  skip_if_not_installed("mapview")
  
  m2 <- move2::mutate_track_data(
    m, 
    var = factor(c("A", "A", "B"), levels = c("B", "A"))
  )
  
  v <- view_spatial(
    m2, 
    path_colours = c("#F2A08F", "#65A7C9"), 
    colour_paths_by = "var"
  )
  
  calls <- v@map$x$calls
  
  i <- sapply(calls, function(x) x$method == "addLegend")
  expect_equal(
    unclass(calls[i][[1]]$args[[1]]$colors),
    c("#F2A08F", "#65A7C9")
  )
  expect_equal(
    unclass(calls[i][[1]]$args[[1]]$labels),
    c("B", "A")
  )
})

test_that("view_spatial() can colour tracks by variable: leaflet", {
  skip_if_not_installed("leaflet")
  
  m2 <- move2::mutate_track_data(
    m, 
    var = factor(c("A", "A", "B"), levels = c("B", "A"))
  )

  v <- view_spatial(
    m2, 
    render_as = "leaflet",
    path_colours = c("#F2A08F", "#65A7C9"), 
    colour_paths_by = "var"
  )
  
  calls <- v$x$calls
  
  i <- sapply(calls, function(x) x$method == "addLegend")
  expect_equal(
    unclass(calls[i][[1]]$args[[1]]$colors),
    c("#F2A08F", "#65A7C9")
  )
  expect_equal(
    unclass(calls[i][[1]]$args[[1]]$labels),
    c("B", "A")
  )
})
  