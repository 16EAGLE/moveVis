#' Deprecated functions
#'
#' Several functions have been deprecated due to a rewrite of moveVis with version 0.10.
#' 
#' @param ... deprecated arguments.
#' 
#' @details The new version of moveVis makes it much easier to animate movement data and multi-temporal imagery (see \code{?moveVis}). You gain more control about the preprocessing of your movement data as well as the visual customization of each animation frame through a more consequent link of \code{moveVis} to \code{gpplot2}.
#'
#' @note To install the old version of moveVis (0.9.9), see \url{https://github.com/16EAGLE/moveVis/releases/tag/v0.9.9}.
#' 
#' @name deprecated
#' @seealso \code{\link{frames_spatial}} \code{\link{frames_graph}} \code{\link{join_frames}} \code{\link{animate_frames}}
#' @export

subset_move <- function(...) out("subset_move() has been deprecated when updating moveVis to support move2. Use move2 indexing to subset movement data.", type = 3)

#' @rdname deprecated
#' @export

animate_move <- function(...) out("animate_move() has been deprecated. Use animate_frames() to animate frames. See ?moveVis and http://movevis.org for help. ", type = 3)

#' @rdname deprecated
#' @export

animate_raster <- function(...) out("animate_raster() has been deprecatedhelp. See ?moveVis and http://movevis.org for help. ", type = 3)

#' @rdname deprecated
#' @export

animate_stats <- function(...) out("animate_stats() has been deprecatedhelp. See ?moveVis and http://movevis.org for help. ", type = 3)

#' @rdname deprecated
#' @export

get_formats <- function(...) out("get_formats() has been deprecatedhelp. See ?moveVis and http://movevis.org for help. ", type = 3)

#' @rdname deprecated
#' @export

get_libraries <- function(...) out("get_libraries() has been deprecatedhelp. See ?moveVis and http://movevis.org for help. ", type = 3)