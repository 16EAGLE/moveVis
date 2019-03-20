#' Join multiple frames lists into a single frames list
#' 
#' This function side-by-side joins the \code{ggplot2} objects of two or more frames lists of equal lengths into a single plot per frame using \code{\link{plot_grid}}. This is useful if you want to side-by-side combine spatial frames returned by \code{\link{frames_spatial}} with graph frames returned by \code{\link{frames_graph}}.
#' 
#' @inheritParams frames_spatial
#' @param frames_lists list, a list of two or more frames lists that you want to combine. All frames lists contained in \code{frames_lists} must be of equal lengths. The contained \code{ggplot2} objects are passed frame-wise to the \code{plotlist} argument of \code{\link{plot_grid}}.
#' @param ... Further arguments, specifying the appearance of the joined \code{ggplot2} objects, passed to \code{\link{plot_grid}}. See \code{\link{plot_grid}} for further options.
#'
#' @return List of ggplot2 objects, each representing a single frame.
#' 
#' @importFrom cowplot plot_grid
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

join_frames <- function(frames_lists, ..., verbose = T){
  
  #frames_lists <- list(...)
  
  ## Check arguments
  if(inherits(verbose, "logical")) options(moveVis.verbose = verbose)
  if(length(unique(sapply(frames_lists, length))) > 1) out("Frames lists provided through argument 'frames_lists' must be of equal lengths for joining their ggplot2 frames.", type = 3)
  if(length(frames_lists) <= 1) out("There must be at least 2 frames lists for joining their ggplot2 frames.", type = 3)
  
  out("Joining frames...")
  .lapply(1:length(frames_lists[[1]]), function(i){
    quiet(plot_grid(plotlist = lapply(frames_lists, "[[", i), ...))
  })
}