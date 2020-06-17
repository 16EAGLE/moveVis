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