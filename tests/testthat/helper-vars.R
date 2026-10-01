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
r_grad <- terra::unwrap(readRDS(example_data("raster_NDVI.rds")))
r_disc <- terra::unwrap(readRDS(example_data("raster_classification.rds")))
r_times <- terra::time(r_grad)

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
#r_disc <- terra::as.list(r_grad) # not working
#r_disc <- lapply(1:length(r_grad), function(i) `[[`(r_grad, i)) # not working
# r_grad_list <- list()
# for(i in 1:length(r_grad)){
#   r_grad_list[[i]] <- `[[`(r_grad, i)
# } # not working
# r_grad <- methods::as(r_grad, "list") # not working
# r_grad <- selectMethod("as.list", class(r_grad))(r_grad)
# r_disc <- terra::as.list(r_grad) # not working
# for(i in 1:length(r_disc)){
#   r_disc[[i]] <- terra::classify(r_disc[[i]], rcl = matrix(c(
#     -1, 0, 1,
#     0, 0.2, 2,
#     0.2, 0.4, 3,
#     0.4, 0.8, 4,
#     0.8, 1, 5), ncol = 3, byrow = TRUE), include.lowest = TRUE)
# }
# r_disc <- terra::sds(r_disc)
# r_disc <- terra::sds(lapply(r_grad, function(x){
#   terra::values(x) <- round(terra::values(x)*10)
#   return(x)
# }))

# small synthetic dataset with two tracks crossing the dateline, used to test
# cross_dateline handling without the cost of the full m.shifted dataset
m.dateline <- local({
  wrap_lon <- function(x) ((x + 180) %% 360) - 180
  n <- 16
  ts <- as.POSIXct("2024-06-01 00:00:00", tz = "UTC") + (seq_len(n) - 1) * 1800
  df <- data.frame(
    x = c(wrap_lon(seq(178.8, 181.2, length.out = n)),
          wrap_lon(seq(181.2, 178.8, length.out = n))),
    y = c(52.5 + sin(seq(0, pi, length.out = n)) * 0.25,
          52.2 - sin(seq(0, pi, length.out = n)) * 0.25),
    timestamp = rep(ts, 2),
    track = rep(c("A_east", "B_west"), each = n)
  )
  mt_as_move2(df, coords = c("x", "y"), time_column = "timestamp",
              track_id_column = "track", crs = st_crs(4326))
})
