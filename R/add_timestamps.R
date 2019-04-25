#' Add timestamps to frames
#'
#' This function adds timestamps to animation frames created with \code{\link{frames_spatial}}.
#'
#' @inheritParams add_text
#' @param m \code{move} or \code{moveStack}, optional. If defined, timestamps are extracted from \code{m} that must be the same object used to create \code{frames} with \code{\link{frames_spatial}}. If undefined (recommended), timestamps are extracted from the attributes of \code{frames} directly.
#' @param x numeric, optional, position of timestamps on the x scale. By default, timestamps will be displayed in the top center.
#' @param y numeric, optional, position of timestamps on the y scale.
#' @param ... optional, arguments passed to \code{\link{add_text}}, such as \code{colour}, \code{size}, \code{type}.
#' 
#' @return List of frames.
#' @author Jakob Schwalb-Willmann
#'
#' @importFrom ggplot2 ggplot_build
#' @importFrom move timestamps
#' @importFrom dplyr bind_rows
#'
#' @examples 
#' library(moveVis)
#' library(move)
#' 
#' data("move_data", "basemap_data")
#' m <- align_move(move_data, res = 4, unit = "mins")
#' 
#' # create spatial frames using a custom NDVI base layer
#' r_list <- basemap_data[[1]]
#' r_times <- basemap_data[[2]]
#' 
#' \donttest{
#' frames <- frames_spatial(m, r_list = r_list, r_times = r_times, r_type = "gradient",
#'                          fade_raster = TRUE)
#' 
#' # add timestamps as text
#' frames.a <- add_timestamps(frames, type = "text")
#' frames.a[[100]]
#' 
#' # or use the ggplot2 "label" type:
#' frames.b <- add_timestamps(frames, type = "label")
#' frames.b[[100]]
#' }
#' 
#' @seealso \code{\link{frames_spatial}} \code{\link{frames_graph}} \code{\link{animate_frames}}
#' @export

add_timestamps <- function(frames, m = NULL, x = NULL, y = NULL, ..., verbose = TRUE){
  
  ## checks
  if(inherits(verbose, "logical")) options(moveVis.verbose = verbose)
  if(!inherits(frames, "list")) out("Argument 'frames' needs to be a list of ggplot objects. See frames_spatial()).", type = 3)
  if(!all(sapply(frames, function(x) inherits(x, "ggplot")))) out("At least one element of argument 'frames' is not a ggplot object.", type = 3)
  
  if(!is.null(m)){
    if(all(!c(inherits(m, "MoveStack"), inherits(m, "Move")))) out("Argument 'm' must be of class 'Move' or 'MoveStack', if defined. Do not define 'm', if timestamps should be extracted from the attributes of 'frames' directly.", type = 3)
    ts <- as.character(sort(unique(timestamps(m))))
    if(length(ts) != length(frames)) out("Lengths of unique timestamps of 'm' and 'frames' do not match. if 'm' is defined. Do only use the same move or moveStack object that you have used to create 'frames'. Otherwise, you can leave 'm' undefined so that frame times are extracted from the attributes of 'frames' directly.", type = 3)
  } else{
    ts <- as.character(get_frametimes(frames))
  }
  
  if(is.null(x)){
    gg.xy <- lapply(ggplot_build(frames[[1]])$data, function(x) cbind.data.frame(x = x$x, y= x$y))
    gg.xy <- bind_rows(gg.xy[!sapply(gg.xy, is.null)])
  
    x <- min(gg.xy$x)+((max(gg.xy$x)-min(gg.xy$x))/2)
    y <- max(gg.xy$y)-((max(gg.xy$y)-min(gg.xy$y))*0.05)
  }
  
  add_text(frames, ts, x, y, ...)
}
