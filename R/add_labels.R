#' Add labels to frames
#'
#' This function adds character labels such as titles or axis labels to frames created with \code{\link{frames_spatial}}.
#'
#' @inheritParams frames_spatial
#' @param frames an object of class \code{moveVis} created with \code{\link{frames_spatial}}.
#' @param title character, frame title. If \code{NULL}, an existing title of \code{frames} is removed. If \code{waiver()} (default, see \code{\link[ggplot2]{waiver}}), an existing title of \code{frames} is kept.
#' @param subtitle character, frame subtitle. If \code{NULL}, an existing title of \code{frames} is removed. If \code{waiver()} (default, see \code{\link[ggplot2]{waiver}}), an existing title of \code{frames} is kept.
#' @param caption character, frame caption. If \code{NULL}, an existing title of \code{frames} is removed. If \code{waiver()} (default, see \code{\link[ggplot2]{waiver}}), an existing title of \code{frames} is kept.
#' @param tag character, frame tag. If \code{NULL}, an existing title of \code{frames} is removed. If \code{waiver()} (default, see \code{\link[ggplot2]{waiver}}), an existing title of \code{frames} is kept.
#' @param x character, label of the x axis. If \code{NULL}, an existing title of \code{frames} is removed. If \code{waiver()} (default, see \code{\link[ggplot2]{waiver}}), an existing title of \code{frames} is kept.
#' @param y character, label of the y axis. If \code{NULL}, an existing title of \code{frames} is removed. If \code{waiver()} (default, see \code{\link[ggplot2]{waiver}}), an existing title of \code{frames} is kept.
#'
#'
#' @return A frames object of class \code{moveVis}.
#' @author Jakob Schwalb-Willmann
#'
#' @importFrom ggplot2 labs waiver theme element_text expr
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
#' # create frames and add labels
#' frames <- frames_spatial(
#'   m, r, r_type = "gradient", fade_raster = TRUE
#' )
#'
#' # add labels
#' frames <- add_labels(
#'   frames,
#'   title = "Example animation using moveVis::add_labels()", 
#'   subtitle = "Adding a subtitle to frames created using frames_spatial()",
#'   caption = "Projection: Geographical, WGS84. Sources: moveVis examples.",
#'   x = "Longitude", y = "Latitude"
#' )
#'
#' # have a look at one frame
#' frames[[100]]
#'
#' @seealso \code{\link{frames_spatial}} \code{\link{frames_graph}} \code{\link{animate_frames}}
#' @export

add_labels <- function(frames, title = waiver(), subtitle = waiver(), caption = waiver(), tag = waiver(),
                       x = waiver(), y = waiver(), verbose = TRUE){

  ## checks
  if(inherits(verbose, "logical")) options(moveVis.verbose = verbose)
  if(!inherits(frames, "moveVis")) out("Argument 'frames' needs to be of class 'moveVis'. See frames_spatial()).", type = 3)
  
  waiver.args <- list(title = title, subtitle = subtitle, caption = caption, tag = tag, x = x, y = y)
  waiver.which <- sapply(waiver.args, function(x) inherits(x, "waiver"))
  if(all(waiver.which)) out("At least one label argument has to be defined.", type = 3)
  if(any(!sapply(waiver.args[!waiver.which], function(x) any(is.character(x), is.null(x))))) out("Label arguments must be of type character, NULL to remove a label or waiver() to keep an already set label.", type = 3)
  
  add_gg(frames, gg = expr(
    list(labs(title = title, subtitle = subtitle, caption = caption, tag = tag, x = x, y = y),
         theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5))
    )
  ),title = title, subtitle = subtitle, caption = caption, tag = tag, x = x, y = y)
}
