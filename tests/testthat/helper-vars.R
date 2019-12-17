## env vars
map_token = Sys.getenv("moveVis.map_token")
if(map_token != "") run_mapbox <- TRUE else run_mapbox <- FALSE
test_maps = as.logical(Sys.getenv("moveVis.test_maps"))

n_cores <- as.numeric(Sys.getenv("moveVis.n_cores"))
if(!is.na(n_cores)) use_multicore(n_cores)

## which tests to run
# which_tests = Sys.getenv("moveVis.which_tests")
# if(which_tests != ""){
#   which_tests <- strsplit(which_tests, ";")[[1]]
# } else{
#   which_test <- c("add_", "align_move", "deprecated", "frames_graph", "frames_spatial", "get_maptypes", "suggest_formats")
# }

## directories
test_dir = Sys.getenv("moveVis.test_dir")
if(test_dir != ""){ if(!dir.exists(test_dir)) dir.create(test_dir)} else{ test_dir <- tempdir()}

data("move_data", package = "moveVis", envir = environment())
data("basemap_data", package = "moveVis", envir = environment())

## movement
m <- move_data
m.aligned <- align_move(m, res = 4, unit = "mins")

## base map
r_grad <- basemap_data[[1]]
r_disc <- lapply(r_grad, function(x){
  y <- raster::setValues(x, round(raster::getValues(x)*10))
  return(y)
})
r_times <- basemap_data[[2]]
