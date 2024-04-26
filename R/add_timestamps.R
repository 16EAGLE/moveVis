#' Add timestamps to frames
#'
#' This function adds timestamps to frames created with \code{\link{frames_spatial}}.
#'
#' @inheritParams add_text
#' @param m deprecated, times are natively stored in \code{moveVis frames_spatial} objects.
#' @param x numeric, optional, position of timestamps on the x scale. By default, timestamps will be displayed in the top center.
#' @param y numeric, optional, position of timestamps on the y scale.
#' @param ... optional, arguments passed to \code{\link{add_text}}, such as \code{colour}, \code{size}, \code{type}.
#'
#' @return A frames object of class \code{moveVis}.
#' @author Jakob Schwalb-Willmann
#'
#' @importFrom ggplot2 ggplot_build
#' @importFrom move timestamps
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
#' frames <- frames_spatial(m,
#'   r_list = r_list, r_times = r_times, r_type = "gradient",
#'   fade_raster = TRUE
#' )
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

add_timestamps <- function(frames, m = NULL, x = NULL, y = NULL, ..., verbose = TRUE) {
  ## checks
  if (inherits(verbose, "logical")) options(moveVis.verbose = verbose)
  if (!inherits(frames, "moveVis")) out("Argument 'frames' needs to be of class 'moveVis'. See frames_spatial()).", type = 3)
  if (!is.null(m)) out("Argument 'm' is deprecated and thus being ignored.", type = 2)

  ts <- as.character(get_frametimes(frames))

  if (is.null(x)) {
    x <- frames$aesthetics$gg.ext[1] + ((frames$aesthetics$gg.ext[3] - frames$aesthetics$gg.ext[1]) / 2)
    y <- frames$aesthetics$gg.ext[4] - ((frames$aesthetics$gg.ext[4] - frames$aesthetics$gg.ext[2]) * 0.05)
  }

  add_text(frames, ts, x, y, ...)
}
