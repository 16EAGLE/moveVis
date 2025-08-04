#' Join multiple frames side-by-side
#' 
#' This function joins two or more \code{moveVis} frames of equal lengths side-by-side into a single plot per frame using \code{\link[patchwork]{wrap_plots}}. This is useful if you want to side-by-side combine spatial frames returned by \code{\link{frames_spatial}} with graph frames returned by \code{\link{frames_graph}}.
#' 
#' @inheritParams frames_spatial
#' @param frames_list list, a list of two or more \code{moveVis} frame objects that you want to combine into onw. Must be of equal lengths. Frames are being passed to \code{\link[patchwork]{wrap_plots}} and combined frame-by-frame.
#' @param guides character, controls how to treat the scales/legends of both frames. See \code{\link[patchwork]{wrap_plots}} for details. Defaults to 'collect'.
#' @param design character, controls how frames are arranged. See \code{\link[patchwork]{wrap_plots}} for details. By default, frames are joined sidy-by-side horizontally.
#' @param render_all_legends logical, whether legends should be preserved (\code{TRUE}) or only the legend of the first frames object should be rendered (\code{FALSE}, default).
#' @param ... Further arguments, specifying the appearance of the joined \code{ggplot2} objects, passed to \code{\link[patchwork]{wrap_plots}}. See \code{\link[patchwork]{wrap_plots}} for further options.
#' @param frames_lists deprecated 
#'
#' @return A frames object of class \code{moveVis}.
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
#' # create spatial frames 
#' frames_sp <- frames_spatial(m, r, r_type = "gradient", fade_raster = TRUE) %>% 
#'   add_colourscale(
#'     type = "gradient", colours = c("orange", "white", "darkgreen"),
#'     legend_title = "NDVI"
#'   )
#'
#' # create graph frames
#' frames_flow <- frames_graph(m, r, path_legend = FALSE, graph_type = "flow")
#' frames_hist <- frames_graph(m, r, path_legend = FALSE, graph_type = "hist")
#'
#' # check lengths (must be equal)
#' sapply(list(frames_sp, frames_flow, frames_hist), length)
#'
#' # define a patchwork design (see ?wrap_plots)
#' design <- "
#' AAB
#' AAC
#' "
#'
#' # create joined frames
#' frames_joined <- join_frames(
#'   list(frames_sp, frames_flow, frames_hist), design = design
#' )
#' frames_joined[[100]]
#'
#' # adjust width and height to suite your joined frames
#' \dontrun{
#' out_file <- tempfile(fileext = ".gif")
#' animate_frames(
#'   frames_joined, out_file = out_file, fps = 25, width = 900, height = 500, 
#'   res = 90, display = TRUE, overwrite = TRUE
#' )
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
  
  # at least track id, times and coordinates must be equal across frames
  equal_m <- length(unique(lapply(frames_list, function(x){
    cbind(
      sf::st_geometry(x$m),
      mt_track_id(x$m),
      mt_time(x$m)
    )
  }))) == 1
  if(isFALSE(equal_m)) out("At least one set of frames in 'frames_list' originates from a different move2 object. Frames to join have to be derived from the same move2 object.", type = 3)
  
  ## create joined frames  
  frames <- list(
    m = frames_list[[1]]$m,
    frames_list = frames_list,
    render_all_legends = FALSE,
    wrap_plots_guides = guides,
    wrap_plots_design = design,
    wrap_plots_args = list(...)
  )
  attr(frames, "class") <- c("moveVis", "frames_joined")
  
  return(frames)
}