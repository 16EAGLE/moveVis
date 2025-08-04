#' Get frame times from frames
#'
#' This function extracts the timestamps associated with each frame from a \code{moveVis} object created using \code{\link{frames_spatial}} or \code{\link{frames_graph}} and returns them as a vector.
#'
#' @inheritParams add_gg
#' @return A POSIXct vector of timestamps representing the time associated with each frame.
#' 
#' 
#' @examples 
#' library(moveVis)
#' library(move2)
#' library(terra)
#'
#' data("move_data", package = "moveVis")
#' r <- readRDS(example_data(file = "raster_NDVI.rds"))
#'
#' # align movement
#' m <- align_move(move_data, res = units::set_units(4, "min"))
#'
#' # create frames 
#' frames <- frames_spatial(m, r, r_type = "gradient", fade_raster = TRUE) 
#'
#' # get frametimes
#' get_frametimes(frames)
#' 
#' @seealso \code{\link{frames_spatial}} \code{\link{frames_graph}}
#' @export

get_frametimes <- function(frames){
  if(!inherits(frames, "moveVis")) out("Argument 'frames' needs to be of class 'moveVis'. See frames_spatial()).", type = 3)
  return(unique(frames$m$time))
}