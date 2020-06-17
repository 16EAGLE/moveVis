#' Add progress bar to frames
#'
#' This function adds a progress bar to animation frames created with \code{\link{frames_spatial}}.
#'
#' @inheritParams add_labels
#' @param colour character, progress bar colour.
#' @param size numeric, progress bar line size..
#'
#' @return List of frames.
#' @author Jakob Schwalb-Willmann
#'
#' @importFrom ggplot2 geom_line aes_string ggplot_build expr
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
#' \dontrun{
#' frames <- frames_spatial(m, r_list = r_list, r_times = r_times, r_type = "gradient",
#'                          fade_raster = TRUE)
#' 
#' # add a progress bar:
#' frames.a <- add_progress(frames)
#' frames.a[[100]]
#' 
#' # or in red and larger
#' frames.b <- add_progress(frames, colour = "red", size = 2.5)
#' frames.b[[100]]
#' }
#' 
#' @seealso \code{\link{frames_spatial}} \code{\link{frames_graph}} \code{\link{animate_frames}}
#' @export

add_progress <- function(frames, colour = "grey", size = 1.8, verbose = TRUE){
  
  ## checks
  if(inherits(verbose, "logical")) options(moveVis.verbose = verbose)
  if(!inherits(frames, "moveVis")) out("Argument 'frames' needs to be of class 'moveVis'. See frames_spatial()).", type = 3)
  
  if(!inherits(colour, "character")) out("Argument 'colour' needs to be of type 'character'.", type = 3)
  if(!inherits(size, "numeric")) out("Argument 'size' needs to be of type 'numeric'.", type = 3)
  
  gg.xy <- ggplot_build(frames[[1]])$data[[1]]
  
  data <- lapply(seq(min(gg.xy$xmin), max(gg.xy$xmax), length.out = length(frames)), function(x, x.min = min(gg.xy$xmin), y = max(gg.xy$ymax)){
    cbind.data.frame(x = c(x.min, x), y = c(y, y))
  })
  
  add_gg(frames, gg = expr(geom_line(aes_string(x = "x", y = "y"), data = data, colour = colour, size = size)),
         data = data, colour = colour, size = size)
}
