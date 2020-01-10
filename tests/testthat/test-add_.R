skip_on_cran()
context("add_ functions")

#if("add_" %in% which_tests){
test_that("add_ functions, gradient", {
  frames <- frames_spatial(m.aligned, r_grad, r_times, r_type = "gradient", verbose = F)
  expect_length(expect_is(add_colourscale(frames, type = "gradient", colours = c("orange", "white", "darkgreen"), legend_title = "NDVI"), "list"), 180)
  expect_length(expect_is(add_labels(frames, x = "Longitude", y = "Latitude"), "list"), 180)
  expect_length(expect_is(add_progress(frames), "list"), 180)
  expect_length(expect_is(add_timestamps(frames, m.aligned, type = "label", colour = "black"), "list"), 180)
  expect_length(expect_is(add_text(frames, "Some text", x = 8.96, y = 47.73, type = "text", colour = "black"), "list"), 180)
  expect_length(expect_is(add_scalebar(frames, distance = 1.5, colour = "black", 0.018), "list"), 180)
  expect_length(expect_is(add_northarrow(frames, colour = "black"), "list"), 180)
  
  ## polygons
  data <- data.frame(x = c(8.96, 8.955, 8.959, 8.963, 8.968, 8.963, 8.96),
                     y = c(47.725, 47.728, 47.729, 47.728, 47.725, 47.723, 47.725))
  data <- rep(list(data), length.out = length(frames))
  data <- lapply(data, function(x){
    y <- rnorm(nrow(x)-1, mean = 0.00001, sd = 0.0001) 
    x + c(y, y[1])
  })
  expect_length(expect_is(add_gg(frames, gg = ggplot2::expr(ggplot2::geom_path(ggplot2::aes(x = x, y = y), data = data, colour = "black")), data = data), "list"), 180)
})
#}
