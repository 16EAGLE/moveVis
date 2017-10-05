#' Overview of moveVis tools
#'
#' \code{moveVis} provides tools to visualize movement data of any kind, e. g by creating path animations from GPS point data.
#' The package is under ongoing development, new functionalities are added constantly.
#' The \code{moveVis} package is closely connected to the \code{move} package and mainly builds up on \code{ggplot2}.
#'
#' @details At the moment, the package includes the following functions:
#' 
#' \code{\link{animate_move}}, which can create spatial movement data animations as GIF file. Among other funtionalities, the function is able to
#' \itemize{
#'   \item visualize move class point data as (multiple) movement paths,
#'   \item display static basemap layers downloaded from Google Maps,
#'   \item display static basemap layers provided by the user,
#'   \item display dynamic, time-referenced raster data, e. g. to visualize land cover changes etc.,
#'   \item compute temporal interpolations from time-referneced raster data,
#'   \item create statistic plots, displaying the interaction of the individual movement paths with environmental data.
#'   ...
#' }
#' 
#' \code{\link{animate_stats}}, which can create animated statistic plots from movement and basemap data as GIF file.
#' 
#' \code{\link{animate_raster}}, which can create animated spatial plots of basemap data as GIF file.
#' 
#' \code{\link{get_libraries}}, a helper function to locate/download/install the extern libraries ImageMagick, FFmpeg and libav and their tools, which are needed for different output file format support.
#' 
#' \code{\link{get_formats}}, a helper function which returns all available output file formats (system-dependent).
#' 
#' @seealso \code{\link{animate_move}}
#' @author Jakob Schwalb-Willmann. Maintainer: Jakob Schwalb-Willmann jakob@schwalb-willmann.de
"_PACKAGE"