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