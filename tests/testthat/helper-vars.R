## env vars
n_cores <- as.numeric(Sys.getenv("moveVis_n_cores"))
if (!is.na(n_cores)) if (n_cores > 1) use_multicore(n_cores)

check_mapview <- any(grepl("mapview", installed.packages()[, 1]))
check_leaflet <- any(grepl("leaflet", installed.packages()[, 1]))


## which tests to run
# which_tests = Sys.getenv("moveVis_which_tests")
# if(which_tests != ""){
#   which_tests <- strsplit(which_tests, ";")[[1]]
# } else{
#   which_test <- c("add_", "align_move", "deprecated", "frames_graph", "frames_spatial", "get_maptypes", "suggest_formats")
# }

## directories
test_dir <- Sys.getenv("moveVis_test_dir")
if (test_dir != "") {
  if (!dir.exists(test_dir)) dir.create(test_dir)
} else {
  test_dir <- tempdir()
}
cat("Test directory: ", test_dir, "\n")

data("move_data", package = "moveVis", envir = environment())
data("basemap_data", package = "moveVis", envir = environment())

## movement
m <- move_data
m.aligned <- align_move(m, res = 4, unit = "mins", verbose = F)

# shift across dateline
l.df <- lapply(move::split(m.aligned), as.data.frame)
df <- do.call(rbind, mapply(x = names(l.df), y = l.df, function(x, y) {
  y$id <- x
  return(y)
}, SIMPLIFY = F))
df$x <- df$x + 171.06
df$x[df$x > 180] <- df$x[df$x > 180] - 360

m.shifted <- df2move(df, proj = raster::crs("+init=epsg:4326"), x = "x", y = "y", time = "time", track_id = "id")

# transform using sf
df <- sf::st_transform(sf::st_as_sf(m.shifted), sf::st_crs(3995))
df <- cbind.data.frame(sf::st_coordinates(df), time = df$time, id = move::trackId(m.shifted))
m.shifted.repro <- df2move(df = df, proj = 3995, x = "X", y = "Y", time = "time", track_id = "id")

## base map
r_grad <- basemap_data[[1]]
r_disc <- lapply(r_grad, function(x) {
  y <- raster::setValues(x, round(raster::getValues(x) * 10))
  return(y)
})
r_times <- basemap_data[[2]]
