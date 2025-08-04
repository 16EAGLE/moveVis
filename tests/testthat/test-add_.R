skip_on_cran()
context("add_ functions")
frames <- frames_spatial(m.aligned, r_grad, r_type = "gradient", verbose = F)
frames_nocrs <- frames_spatial(m = m.shifted, crs = st_crs(4326), verbose = F, cross_dateline = T)

#if("add_" %in% which_tests){
test_that("add_colourscale", {
  # correct calls for type="gradient"
  expect_length(expect_is(add_colourscale(frames, type = "gradient", colours = c("orange", "white", "darkgreen"), legend_title = "NDVI"), "moveVis"), 188) # defualt
  expect_length(expect_is(add_colourscale(frames, type = "gradient", colours = c("0.1" = "orange", "0.2" = "white", "0.3" = "darkgreen"), legend_title = "NDVI"), "moveVis"), 188) # named colours
  
  # correct calls for type="discrete"
  expect_length(expect_is(add_colourscale(frames, type = "discrete", colours = c("orange", "white", "darkgreen"), legend_title = "NDVI"), "moveVis"), 188)
  expect_length(expect_is(add_colourscale(frames, type = "discrete", colours = c("0.1" = "orange", "0.2" = "white", "0.3" = "darkgreen"), legend_title = "NDVI"), "moveVis"), 188) # named colours
  
  # false calls for type="gradient"
  expect_error(add_colourscale(NA, type = "gradient", colours = c("orange", "white", "darkgreen"), legend_title = "NDVI")) # false frames
  expect_error(add_colourscale(list(frames[[1]], NA), type = "gradient", colours = c("orange", "white", "darkgreen"), legend_title = "NDVI")) # false frames
  expect_error(add_colourscale(frames, type = 25, colours = c("orange", "white", "darkgreen"), legend_title = "NDVI")) # false type
  expect_error(add_colourscale(frames, type = "RGB", colours = c("orange", "white", "darkgreen"), legend_title = "NDVI")) # false type
  expect_error(add_colourscale(frames, type = "gradient", colours = c(2,3,4), legend_title = "NDVI")) # false coulous
  expect_error(add_colourscale(frames, type = "gradient", colours = c("orange", "white", "darkgreen"), legend_title = "NDVI", na.colour = 1)) # false na.colour
  
  # false calls for type="discrete"
  expect_error(add_colourscale(frames, type = "discrete", colours = c("orange", "white", "darkgreen"), labels = c(1,2,3), legend_title = "NDVI")) # false labels
  expect_error(add_colourscale(frames, type = "discrete", colours = c("orange", "white", "darkgreen"), labels = c("1", "2"), legend_title = "NDVI")) # false labels
  expect_error(add_colourscale(frames, type = "discrete", colours = c("orange", "white", "darkgreen"), legend_title = "NDVI", na.show = "hi")) # false na.show
})

test_that("add_gg", {
  # correct call
  data <- data.frame(x = c(8.96, 8.955, 8.959, 8.963, 8.968, 8.963, 8.96),
                     y = c(47.725, 47.728, 47.729, 47.728, 47.725, 47.723, 47.725))
  data <- rep(list(data), length.out = length(frames))
  data <- lapply(data, function(x){
    y <- rnorm(nrow(x)-1, mean = 0.00001, sd = 0.0001) 
    x + c(y, y[1])
  })
  expect_length(expect_is(add_gg(frames, gg = ggplot2::expr(ggplot2::geom_path(ggplot2::aes(x = x, y = y), data = data, colour = "black")), data = data), "moveVis"), 188)
  
  # false calls
  expect_error(add_gg(frames, gg = ggplot2::expr(ggplot2::geom_path(ggplot2::aes(x = x, y = y), data = data, colour = "black")), data = data[-1]))
  expect_error(add_gg(frames, gg = list(ggplot2::expr(ggplot2::geom_path(ggplot2::aes(x = x, y = y), data = data, colour = "black"))), data = data))
  expect_error(add_gg(frames, gg = "hi", data = data))
  
  # with sf
  data <- cbind(
    x = c(8.96, 8.955, 8.959, 8.963, 8.968, 8.963, 8.96),
    y = c(47.725, 47.728, 47.729, 47.728, 47.725, 47.723, 47.725)
  )
  data <- list(data) %>% 
    st_polygon() %>% 
    st_geometry() %>% 
    st_as_sf(crs = st_crs(4326)) %>% 
    st_transform(crs = st_crs(3857))
  
  expect_length(expect_is(add_gg(frames, gg = ggplot2::expr(geom_sf(data = data, colour = "black", fill = "transparent", linetype = "dashed", lwd = 1)), data = data), "moveVis"), 188)
  
  # with sf dynamic
  data <- cbind(
    x = c(8.96, 8.955, 8.959, 8.963, 8.968, 8.963, 8.96),
    y = c(47.725, 47.728, 47.729, 47.728, 47.725, 47.723, 47.725)
  )
  data <- rep(list(data), length.out = length(frames))
  data <- lapply(data, function(x){
    y <- rnorm(nrow(x)-1, mean = 0.00001, sd = 0.0001) 
    x + c(y, y[1])
  })
  data <- lapply(data, function(x){
    list(x) %>% 
      st_polygon() %>% 
      st_geometry() %>% 
      st_as_sf(crs = st_crs(4326)) %>% 
      st_transform( crs = st_crs(3857))
  })
  expect_length(expect_is(add_gg(frames, gg = ggplot2::expr(geom_sf(data = data, colour = "black", fill = "transparent", linetype = "dashed", lwd = 1)), data = data), "moveVis"), 188)
})

test_that("add_labels", {
  # correct call
  expect_length(expect_is(add_labels(frames, x = "Longitude", y = "Latitude"), "moveVis"), 188)
  
  # false calls
  expect_error(add_labels("x", x = "Longitude", y = "Latitude"))
  expect_error(add_labels(list(frames[[1]], NA), x = "Longitude", y = "Latitude"))
  expect_error(add_labels(frames))
  expect_error(add_labels(frames, x = 1))
})

test_that("add_northarrow", {
  # correct call
  expect_length(expect_is(add_northarrow(frames, colour = "black"), "moveVis"), 188)
  
  # false calls
  expect_error(add_northarrow(NA, colour = "black"))
  expect_error(add_northarrow(list(frames[[1]], NA), colour = "black"))
  expect_error(add_northarrow(frames, colour = "black", position = 1))
})

test_that("add_progress", {
  # correct call
  expect_length(expect_is(add_progress(frames), "moveVis"), 188)
  
  # false calls
  expect_error(add_progress(NA))
  expect_error(add_progress(list(frames[[1]], NA)))
  expect_error(add_progress(frames, colour = 1))
  expect_error(add_progress(frames, size = "1"))
})

test_that("add_scalebar", {
  # correct calls
  expect_length(expect_is(add_scalebar(frames, distance = 1.5), "moveVis"), 188)
  expect_length(expect_is(add_scalebar(frames, distance = 1.5, units = "miles"), "moveVis"), 188)
  expect_length(expect_is(add_scalebar(frames), "moveVis"), 188)
  expect_length(expect_is(add_scalebar(frames_nocrs), "moveVis"), 188)
  
  # false call
  expect_error(add_scalebar(NA, distance = 1.5, colour = "black", 0.018)) # false frames
  expect_error(add_scalebar(list(frames[[1]], NA), distance = 1.5, colour = "black", 0.018)) # false frames
  expect_error(add_scalebar(frames, distance = 1.5, colour = "black", position = 1)) # false position
  expect_error(add_scalebar(frames, distance = 1.5, colour = "black", units = "abc")) # false units
})

test_that("add_text", {
  # correct call
  expect_length(expect_is(add_text(frames, "Some text", x = 8.96, y = 47.73, type = "text", colour = "black"), "moveVis"), 188)
  
  # false calls
  expect_error(add_text(NA, "Some text", x = 8.96, y = 47.73, type = "text", colour = "black")) # false frames
  expect_error(add_text(list(frames[[1]], NA), "Some text", x = 8.96, y = 47.73, type = "text", colour = "black")) # false frames
  expect_error(add_text(frames, 1, x = 8.96, y = 47.73, type = "text", colour = "black")) # false label
  expect_error(add_text(frames, "Some text", x = 8.96, y = 47.73, type = "text", colour = 1)) # false colour
  expect_error(add_text(frames, "Some text", x = "8.96", y = 47.73, type = "text", colour = "black")) # false x
  expect_error(add_text(frames, "Some text", x = 8.96, y = "47.73", type = "text", colour = "black")) # false y
  expect_error(add_text(frames, "Some text", x = 8.96, y = 47.73, size = "x", type = "text", colour = "black")) # false size
  expect_error(add_text(frames, "Some text", x = c(8.96, 8.96), y = 47.73, type = "text", colour = "black")) # false x length
})

test_that("add_timestamps", {
  # correct call
  expect_length(expect_is(add_timestamps(frames, type = "label", colour = "black"), "moveVis"), 188)
  
  # false calls
  expect_error(add_timestamps(NA, type = "label", colour = "black")) # false frames
  expect_error(add_timestamps(list(frames[[1]], NA), type = "label", colour = "black")) # false frames
})
#}
