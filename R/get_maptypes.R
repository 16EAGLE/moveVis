#' Get all supported map types
#'
#' This function returns every supported map type that can be used as input to the \code{map_type} argument of \code{\link{frames_spatial}}.
#'
#' @param map_service character, optional, either \code{"mapbox"} or \code{"osm"}. Otherwise, a list of map types for both services is returned.
#' @return A character vector of supported map types
#' 
#' @examples 
#' # for all services
#' get_maptypes()
#' 
#' # for osm only
#' get_maptypes("osm")
#' # or
#' get_maptypes()$osm
#' 
#' # for mapbox only
#' get_maptypes("mapbox")
#' # or
#' get_maptypes()$mapbox
#' 
#' @seealso \code{\link{frames_spatial}}
#' @export

get_maptypes <- function(map_service = NULL){
  map_types <- list(mapbox = names(getOption("moveVis.mapbox_types")), osm = names(getOption("moveVis.map_api")$osm))
  if(!is.null(map_service)) map_types[[map_service]] else map_types
}
