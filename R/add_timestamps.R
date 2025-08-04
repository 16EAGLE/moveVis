#' Add timestamps to frames
#'
#' This function adds timestamps to frames created with \code{\link{frames_spatial}}.
#'
#' @inheritParams add_text
#' @param x numeric, optional, position of timestamps on the x scale. By default, timestamps will be displayed in the top center.
#' @param y numeric, optional, position of timestamps on the y scale.
#' @param format character, optional, format of timestamps to be displayed in, passed to \code{\link{strftime}}).
#' @param ... optional, arguments passed to \code{\link{add_text}}, such as \code{colour}, \code{size}, \code{type}.
#' 
#' @return A frames object of class \code{moveVis}.
#' @author Jakob Schwalb-Willmann
#'
#' @importFrom ggplot2 ggplot_build
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
#' # create frames and add timestamps as text
#' frames <- frames_spatial(m, r, r_type = "gradient", fade_raster = TRUE) %>%
#'   add_timestamps(type = "text")
#' frames[[100]]
#'
#' # or use the ggplot2 "label" type:
#' frames <- frames_spatial(m, r, r_type = "gradient", fade_raster = TRUE) %>%
#'   add_timestamps(type = "label")
#' frames[[100]]
#' 
#' @seealso \code{\link{frames_spatial}} \code{\link{frames_graph}} \code{\link{animate_frames}}
#' @export

add_timestamps <- function(frames, x = NULL, y = NULL, format = "%Y-%m-%d %H:%M:%S", ..., verbose = TRUE){
  
  ## checks
  if(inherits(verbose, "logical")) options(moveVis.verbose = verbose)
  if(!inherits(frames, "moveVis")) out("Argument 'frames' needs to be of class 'moveVis'. See frames_spatial()).", type = 3)
  # if(!is.null(m)) out("Argument 'm' is deprecated and thus being ignored.", type = 2)
  
  ts <- strftime(get_frametimes(frames), format = format)
  
  if(is.null(x)){
    x <- frames$aesthetics$gg.ext[1]+((frames$aesthetics$gg.ext[3]-frames$aesthetics$gg.ext[1])/2)
    y <- frames$aesthetics$gg.ext[4]-((frames$aesthetics$gg.ext[4]-frames$aesthetics$gg.ext[2])*0.05)
  }
  
  add_text(frames, ts, x, y, ...)
}
