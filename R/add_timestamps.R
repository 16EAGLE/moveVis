#' Add timestamps to frames
#'
#' This function adds timestamps to animation frames created with \code{\link{create_frames}}.
#'
#' @inheritParams add_text
#' @param m \code{move} or \code{moveStack} used to create \code{frames} with \code{\link{create_frames}} of uniform time scale and time lag, e.g. prepared with \code{\link{align_move}}.
#' @param x numeric, optioanl, position of timestamps on the x scale. By default, timestamps will be displayed in the top center.
#' @param y numeric, optioanl, position of timestamps on the y scale.
#' @param ... optional, arguments passed to \code{\link{add_text}}, such as \code{colour}, \code{size}, \code{type}.
#' 
#' @return List of frames.
#' @author Jakob Schwalb-Willmann
#'
#' @importFrom ggplot2 ggplot_build
#' @importFrom move timestamps
#' @importFrom dplyr bind_rows
#'
#' @seealso \link{create_frames}
#' @export

add_timestamps <- function(frames, m, x = NULL, y = NULL, ..., verbose = TRUE){
  
  ## checks
  if(inherits(verbose, "logical")) options(moveVis.verbose = verbose)
  if(!inherits(frames, "list")) out("Argument 'frames' needs to be a list of ggplot objects. See create_frames()).", type = 3)
  if(!all(sapply(frames, function(x) inherits(x, "ggplot")))) out("At least one element of argument 'frames' is not a ggplot object.", type = 3)
  
  if(all(!c(inherits(m, "MoveStack"), inherits(m, "Move")))) out("Argument 'm' must be of class 'Move' or 'MoveStack'.", type = 3)
  
  ts <- as.character(sort(unique(timestamps(m))))
  if(length(ts) != length(frames)) out("Unique timestamps of 'm' must be of same length as 'frames'. Do only use the same move or moveStack object that you have used to create 'frames'.", type = 3)
  
  if(is.null(x)){
    gg.xy <- lapply(ggplot_build(frames[[1]])$data, function(x) cbind.data.frame(x = x$x, y= x$y))
    gg.xy <- bind_rows(gg.xy[!sapply(gg.xy, is.null)])
  
    x <- min(gg.xy$x)+((max(gg.xy$x)-min(gg.xy$x))/2)
    y <- max(gg.xy$y)-((max(gg.xy$y)-min(gg.xy$y))*0.05)
  }
  
  add_text(frames, ts, x, y, ...)
}
