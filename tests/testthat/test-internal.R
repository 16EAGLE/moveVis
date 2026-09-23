skip_on_cran()
context("internal")

test_that("internal (labels)", {
  expect_length(expect_is(moveVis:::.x_labels(c(NA, -10,0,10,20,30)), "expression"), 5)
  expect_length(expect_is(moveVis:::.y_labels(c(NA, -10,0,10,20,30)), "expression"), 5)
})

test_that("internal (onLoad)", {
  expect_invisible(moveVis:::.onLoad())
})

test_that("internal (.ext splits extent across the dateline)", {
  ext <- sf::st_bbox(
    c(xmin = 178.44, ymin = 51.8, xmax = 181.56, ymax = 52.9),
    crs = sf::st_crs(4326)
  )
  halves <- .split_dateline_ext(ext)

  expect_named(halves, c("east", "west"))
  expect_equal(as.numeric(halves$east[c("xmin", "xmax")]), c(178.44, 180))
  expect_equal(as.numeric(halves$west[c("xmin", "xmax")]), c(-180, -178.44))
})

test_that("internal (.ext does not clip shifted extents at the dateline)", {
  m <- sf::st_shift_longitude(m.dateline)
  ext <- .ext(
    m, crs = sf::st_crs(4326), margin_factor = 1.3, cross_dateline = TRUE
  )

  # extent must stay in shifted (0-360) space rather than being cut at 180
  expect_gt(ext[["xmax"]], 180)
  expect_lt(ext[["xmax"]] - ext[["xmin"]], 10)
})

test_that("internal (.ext still clips lon/lat maxima when not crossing)", {
  # margin expansion must not push a non-crossing extent outside the domain
  ext <- .ext(
    m.dateline,
    crs = sf::st_crs(4326),
    margin_factor = 1.3,
    cross_dateline = FALSE
  )

  expect_lte(ext[["xmax"]], 180)
  expect_gte(ext[["xmin"]], -180)
})

test_that("internal (.basemap_dateline leaves a non-crossing extent unsplit)", {
  # splitting an extent that stops short of the dateline yields an invalid
  # western half, which would merge into a raster spanning most of the globe
  ext <- sf::st_bbox(
    c(xmin = 178.8, ymin = 51.9, xmax = 179.9, ymax = 52.9),
    crs = sf::st_crs(4326)
  )
  
  r <- .basemap_dateline(
    ext,
    crs = sf::st_crs(4326),
    map_service = "carto",
    map_type = "light",
    verbose = FALSE
  )

  r.ext <- as.vector(terra::ext(r))
  expect_lt(r.ext[2], 180)
  expect_lt(r.ext[2] - r.ext[1], 2)
})

test_that("internal (.equidistant does not clip a shifted extent)", {
  ext <- sf::st_bbox(
    c(xmin = 178.8, ymin = 51.83, xmax = 181.2, ymax = 52.87),
    crs = sf::st_crs(4326)
  )
  sq <- .equidistant(ext, margin_factor = 1.3, cross_dateline = TRUE)

  # the square extent stays centered on the data instead of being cut at 180
  expect_gt(sq[["xmax"]], 180)
  expect_equal(unname(mean(c(sq[["xmin"]], sq[["xmax"]]))), 180, tolerance = 1e-6)
})

test_that("internal (.equidistant still clips lon maxima when not crossing)", {
  ext <- sf::st_bbox(
    c(xmin = 178, ymin = 51.83, xmax = 179.9, ymax = 52.87),
    crs = sf::st_crs(4326)
  )
  sq <- .equidistant(ext, margin_factor = 1.3, cross_dateline = FALSE)

  expect_lte(sq[["xmax"]], 180)
})
