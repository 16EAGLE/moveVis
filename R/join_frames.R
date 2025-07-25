#' Join multiple frames side-by-side
#' 
#' This function joins two or more \code{moveVis} frame objects of equal lengths side-by-side into a single plot per frame using \code{\link{plot_grid}}. This is useful if you want to side-by-side combine spatial frames returned by \code{\link{frames_spatial}} with graph frames returned by \code{\link{frames_graph}}.
#' 
#' @inheritParams frames_spatial
#' @param frames_list list, a list of two or more \code{moveVis} frame objects that you want to combine into onw. Must be of equal lengths. Frames are being passed to the \code{plotlist} argument of \code{\link{plot_grid}} and combined frame-by-frame.
#' @param guides character, controls how to treat the scales/legends of both frames. See \code{\link{wrap_plots}} for details. Defaults to 'collect'.
#' @param design character, controls how frames are arranged. See \code{\link{wrap_plots}} for details. By default, frames are joined sidy-by-side horizontally.
#' @param render_all_legends logical, whether legends should be preserved (\code{TRUE}) or only the legend of the first frames object should be rendered (\code{FALSE}, default).
#' @param ... Further arguments, specifying the appearance of the joined \code{ggplot2} objects, passed to \code{\link{wrap_plots}}. See \code{\link{wrap_plots}} for further options.
#' @param frames_lists deprecated 
#'
#' @return A frames object of class \code{moveVis}.
#' 
#' @examples
#' \dontrun{
#' library(moveVis)
#' library(move)
#' 
#' data("move_data", "basemap_data")
#' # align movement
#' m <- align_move(move_data, res = 4, unit = "mins")
#' 
#' # create spatial frames and graph frames:
#' r_list <- basemap_data[[1]]
#' r_times <- basemap_data[[2]]
#' 
#' frames.sp <- frames_spatial(m, r_list = r_list, r_times = r_times, r_type = "gradient",
#'                             fade_raster = TRUE)
#' frames.sp <- add_colourscale(frames.sp, type = "gradient",
#'                              colours = c("orange", "white", "darkgreen"), legend_title = "NDVI")
#' frames.flow <- frames_graph(m, r_list, r_times, path_legend = FALSE, graph_type = "flow")
#' frames.hist <- frames_graph(m, r_list, r_times, path_legend = FALSE, graph_type = "hist")
#' 
#' # check lengths (must be equal)
#' sapply(list(frames.sp, frames.flow, frames.hist), length)
#' 
#' # Let's join the graph frames vertically
#' frames.join.gr <- join_frames(list(frames.flow, frames.hist), ncol = 1, nrow = 2)
#' frames.join.gr[[100]]
#' 
#' # Now, let's join the joined graph frames with the spatial frames horizontally
#' # in 2:1 ration and align all axis
#' frames.join <- join_frames(list(frames.sp, frames.join.gr),
#'                            ncol = 2, nrow = 1, rel_widths = c(2, 1), axis = "tb")
#' frames.join[[100]]
#' # in a standard graphics device, this looks a bit unproportional
#' # however when setting the correct width, height and resolution of a graphic device,
#' # it will come out well aligend.
#' 
#' # Do so for example with animate_move() with width = 900, dheight = 500 and res = 90
#' animate_frames(frames.join, out_file = tempfile(fileext = ".gif"), fps = 25, 
#'                width = 900, height = 500, res = 90, display = TRUE, overwrite = TRUE)
#' }
#' @seealso \code{\link{frames_spatial}} \code{\link{frames_graph}} \code{\link{animate_frames}}
#' 
#' @export

join_frames <- function(frames_list, guides = "collect", design = NULL, render_all_legends = FALSE, ..., frames_lists = NULL, verbose = T){
  
  if(is.null(frames_list)) if(!is.null(frames_lists)){
    out("Argument 'frames_lists' is deprecated. Use 'frames_list' instead.", type = 2)
    frames_list <- frames_lists
  }
  
  ## Check arguments
  if(inherits(verbose, "logical")) options(moveVis.verbose = verbose)
  if(length(unique(sapply(frames_list, length))) > 1) out("Frames in 'frames_list' must be of equal lengths for joining their ggplot2 frames.", type = 3)
  if(length(frames_list) <= 1) out("There must be at least two moveVis frames objects for joining their ggplot2 frames.", type = 3)
  
  ## create joined frames  
  frames <- list(
    frames_list = frames_list,
    render_all_legends = FALSE,
    wrap_plots_guides = guides,
    wrap_plots_design = design,
    wrap_plots_args = list(...)
  )
  attr(frames, "class") <- c("moveVis", "frames_joined")
  
  return(frames)
}