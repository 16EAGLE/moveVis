#' Deprecated functions
#'
#' Several functions have been deprecated over the development cycles of moveVis.
#' 
#' @param ... deprecated arguments.
#' 
#' @note To install older versions of moveVis, see \url{https://github.com/16EAGLE/moveVis/releases/} or visit the CRAN archive.
#' 
#' @name deprecated
#' @seealso \code{\link{frames_spatial}} \code{\link{frames_graph}} \code{\link{join_frames}} \code{\link{animate_frames}}
#' @export

subset_move <- function(...) out("subset_move() has been deprecated when updating moveVis to support move2. Use move2 indexing to subset movement data.", type = 3)

#' @rdname deprecated
#' @export

df2move <- function(...) out("df2move() has been deprecated when updating moveVis to support move2. Use move2 indexing to subset movement data.", type = 3)

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