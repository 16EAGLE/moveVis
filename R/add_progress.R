#' Add progress bar to frames
#'
#' This function adds a progress bar to frames created with \code{\link{frames_spatial}}.
#'
#' @inheritParams add_labels
#' @param colour character, progress bar colour.
#' @param size numeric, progress bar line size (line width).
#' @param y_nudge numeric, amount of vertical distance to move the progress bar (default: top of plot area).
#'
#' @return A frames object of class \code{moveVis}.
#' @author Jakob Schwalb-Willmann
#'
#' @importFrom ggplot2 geom_line aes_string ggplot_build expr position_nudge
#'
#' @examples 
#' library(moveVis)
#' library(move2)
#' library(terra)
#' 
#' data("move_data", package = "moveVis")
#' r <- readRDS(example_data(file = "basemap_data.rds"))
#' 
#' # align movement
#' m <- align_move(move_data, res = units::set_units(4, "min"))
#' 
#' # create frames and add northarrow
#' frames <- frames_spatial(m, r, r_type = "gradient", fade_raster = TRUE) %>% 
#'   add_progress()
#' frames[[100]]
#' 
#' # or in white at another position
#' frames <- frames_spatial(m, r, r_type = "gradient", fade_raster = TRUE) %>% 
#'   add_progress(colour = "red", size = 2.5)
#' frames[[100]]
#' 
#' @seealso \code{\link{frames_spatial}} \code{\link{frames_graph}} \code{\link{animate_frames}}
#' 
#' @importFrom rlang .data
#' @export

add_progress <- function(frames, colour = "grey", size = 1.8, y_nudge = 0, verbose = TRUE){
  
  ## checks
  if(inherits(verbose, "logical")) options(moveVis.verbose = verbose)
  if(!inherits(frames, "moveVis")) out("Argument 'frames' needs to be of class 'moveVis'. See frames_spatial()).", type = 3)
  
  if(!inherits(colour, "character")) out("Argument 'colour' needs to be of type 'character'.", type = 3)
  if(!inherits(size, "numeric")) out("Argument 'size' needs to be of type 'numeric'.", type = 3)
  
  gg.xy <- ggplot_build(frames[[1]])$data[[1]]
  
  data <- lapply(seq(min(gg.xy$xmin), max(gg.xy$xmax), length.out = length(frames)), function(x, x.min = min(gg.xy$xmin), y = max(gg.xy$ymax)){
    cbind.data.frame(x = c(x.min, x), y = c(y, y))
  })
  # data <- lapply(seq(min(gg.xy$xmin), max(gg.xy$xmax), length.out = length(frames)), function(
  #   x, x.min = min(gg.xy$xmin), y = (max(gg.xy$ymax) - (((max(gg.xy$ymax)-min(gg.xy$ymax))/100)*3.5))){
  #   cbind.data.frame(x = c(x.min, x), y = c(y, y))
  # })
  
  add_gg(frames, gg = expr(
    geom_line(aes(x = .data$x, y = .data$y), data = data, colour = colour, linewidth = size, position = position_nudge(x = 0, y = y_nudge))),
    data = data, colour = colour, size = size, y_nudge = y_nudge)
}
