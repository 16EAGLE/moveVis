## env vars
n_cores <- as.numeric(Sys.getenv("moveVis_n_cores"))
if(!is.na(n_cores)) if(n_cores > 1) use_multicore(n_cores)

check_mapview <- any(grepl("mapview", installed.packages()[,1]))
check_leaflet <- any(grepl("leaflet", installed.packages()[,1]))

## which tests to run
# which_tests = Sys.getenv("moveVis_which_tests")
# if(which_tests != ""){
#   which_tests <- strsplit(which_tests, ";")[[1]]
# } else{
#   which_test <- c("add_", "align_move", "deprecated", "frames_graph", "frames_spatial", "get_maptypes", "suggest_formats")
# }

## directories
test_dir <- Sys.getenv("moveVis_test_dir")
if(test_dir != ""){
  if(!dir.exists(test_dir)) dir.create(test_dir)
}else{
  test_dir <- tempdir()
}
cat("Test directory: ", test_dir, "\n")

data("move_data", package = "moveVis", envir = environment())
basemap_data <- readRDS(example_data())

## movement
m <- move_data
m.aligned <- align_move(move_data, res = units::set_units(4, "min"))

# shift across dateline
l.df <- lapply(split(m.aligned, mt_track_id(m.aligned)), function(x){
  as.data.frame(cbind(x, sf::st_coordinates(x)))
})
df <- do.call(rbind, mapply(x = names(l.df), y = l.df, function(x, y){
  y$id = x
  return(y)
}, SIMPLIFY = F))
df$X <- df$X+171.06
df$X[df$X > 180] <- df$X[df$X > 180]-360
df$geometry <- NULL

m.shifted <- mt_as_move2(df, coords = c("X", "Y"), time_column = "timestamp", track_id_column = "track", crs = st_crs(m))

# transform using sf
m.shifted.repro <- sf::st_transform(m.shifted, sf::st_crs(3995))

## base map
r_grad <- basemap_data
r_disc <- terra::sds(lapply(r_grad, function(x){
  terra::values(x) <- round(terra::values(x)*10)
  return(x)
}))
r_times <- terra::time(basemap_data)
