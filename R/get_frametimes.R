#' Get frame times from frames
#'
#' This function extracts the timestamps associated with each frame of a list of frames created using \code{\link{frames_spatial}} or \code{\link{frames_graph}} and returns them as a vector.
#'
#' @param frames list, list of frames created using \code{\link{frames_spatial}} or \code{\link{frames_graph}}.
#' @return A POSIXct vector of timestamps representing the time assoicated with each frame in \code{frames}.
#' 
#' @details \code{moveVis} stores the times represented by a frame as an attribute \code{"time"} for each \code{ggplot} frame.
#' 
#' @examples 
#' library(moveVis)
#' library(move)
#' 
#' data("move_data")
#' # align movement
#' m <- align_move(move_data, res = 4, unit = "mins")
#' 
#' \dontrun{
#' frames <- frames_spatial(m, map_service = "osm", map_type = "watercolor")
#' frames.ts <- get_frametimes(frames)
#' print(frames.ts)
#' }
#' 
#' @seealso \code{\link{frames_spatial}} \code{\link{frames_graph}}
#' @export

get_frametimes <- function(frames){
  if(!inherits(frames, "moveVis")) out("Argument 'frames' needs to be of class 'moveVis'. See frames_spatial()).", type = 3)
  return(unique(frames$move_data$time))
}