#' Exemplary manipulated NDVI data
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