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
#' \donttest{
#' frames <- frames_spatial(m, map_service = "osm", map_type = "watercolor")
#' frames.ts <- get_frametimes(frames)
#' print(frames.ts)
#' }
#' 
#' @seealso \code{\link{frames_spatial}} \code{\link{frames_graph}}
#' @export

get_frametimes <- function(frames){
  
  x <- lapply(frames, function(x){
    y <- attr(x, "time")
    if(!inherits(y, "POSIXct")) y <- NA
    return(y)
  })
  
  ft <- unlist(x)
  attributes(ft) <- attributes(x[[1]])
  
  if(all(is.na(ft))) out("No associated timestamps per frame could be found in frames. Did you use frames_spatial or frames_graph to create the supplied list of frames?", type = 3)
  if(any(is.na(ft))) out("At least one frame is missing the time attribute.", type = 2)
  
  return(ft)
}