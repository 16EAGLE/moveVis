#' Example simulated movement tracks
#'
#' This dataset contains a \code{Move} object, representing coordinates and acquisition times of three simulated movement tracks, covering a location nearby Lake of Constance, Germany. Individual names are made up for demonstration purposes.
#'
#' @details This object is used by some \code{moveVis} examples and unit tests.
#' @note All data contained should only be used for testing \code{moveVis} and are not suitable to be used for analysis or interpretation.
#'
#' @format \code{Move} object, as used by the \code{move} package.
#' @usage data(move_data)
#' @docType data
#'
"move_data"

#' White Stork LifeTrack tracks
#'
#' This dataset contains a \code{data.frame} object, representing coordinates and acquisition times of 15 White Storks, migrating from Lake of Constance, SW Germany, to Africa.
#'
#' @details These objects are used by some \code{moveVis} examples and have been included for demonstrational purposes.
#'
#' The dataset represents a subset of the LifeTrack White Stork dataset by Cheng et al. (2019) and Fiedler et al. (2019), available under the Creative Commons license "CC0 1.0 Universal Public Domain Dedication" on Movebank (doi:10.5441/001/1.ck04mn78/1).
#'
#' @references
#' Cheng Y, Fiedler W, Wikelski M, Flack A (2019) "Closer-to-home" strategy benefits juvenile survival in a long-distance migratory bird. Ecology and Evolution. doi:10.1002/ece3.5395
#'
#' Fiedler W, Flack A, Schäfle W, Keeves B, Quetting M, Eid B, Schmid H, Wikelski M (2019) Data from: Study "LifeTrack White Stork SW Germany" (2013-2019). Movebank Data Repository. doi:10.5441/001/1.ck04mn78
#'
#' @format
#' \itemize{
#'    \item \code{df} is a \code{data.frame} object
#'    \item \code{m} is a \code{moveStack} object
#' }
#'
#'
#' @usage data(whitestork_data)
#' @docType data
#' @name whitestork_data
#'
"df"

#' @rdname whitestork_data
#' @usage NULL
"m"

#' Example manipulated NDVI data
#'
#' This dataset contains two lists of equal lenghts:
#' \itemize{
#'    \item a list of ten single-layer \code{raster} objects, representing NDVI images covering the Lake of Constance area.
#'    \item a list of made-up times that simulate acquisition times with a temporal resolution, remote sensing scientiest would dream of...
#' }
#'
#' @details This object is used by some \code{moveVis} examples and unit tests.
#' @note All data contained should only be used for testing \code{moveVis} and are not suitable to be used for analysis or interpretation.
#'
#'
#' @format List containing two lists of equal lengths: a list of \code{raster} objects and a list of \code{POSIXct} times.
#' @source MODIS (MOD13Q1 NDVI)
#' @usage data(basemap_data)
#' @docType data
#'
"basemap_data"
