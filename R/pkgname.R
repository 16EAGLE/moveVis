#' Overview of moveVis tools
#'
#' \code{moveVis} shall provide tools to visualize movement data of any kind, e. g by creating path animations from GPS point data.
#' The package is under ongoing development, new functions will be added. The \code{moveVis} package shall be closely connected to the \code{move} package
#' functionalities.
#'
#' @details At the moment, the package includes the \code{animate_move()} function, which can create GIF file animations from
#' movement data. Among other funtionalities, the function is able to
#' \itemize{
#'   \item visualize move class point data as (multiple) movement paths
#'   \item display static basemap layers downloaded from Google Maps
#'   \item display static basemap layers provided by the user
#'   \item display dynamic, time-referenced raster data, e. g. to visualize land cover changes etc.
#'   \item compute temporal interpolations from time-referneced raster data
#'   ...
#' }
#'
#' @seealso \code{\link{animate_move}}
#' @author Jakob Schwalb-Willmann. Maintainer: Jakob Schwalb-Willmann jakob@schwalb-willmann.de
"_PACKAGE"