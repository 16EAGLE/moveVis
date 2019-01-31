#' Get all supported map types
#'
#' This function returns every supported map type that can be used as input to the \code{map_type} argument of \code{\link{create_frames}}.
#'
#' @param map_service character, optional, either \code{"mapbox"} or \code{"osm"}. Otherwise, a list of map types for both services is returned.
#' @return A character vector of supported map types
#' 
#' @seealso \code{\link{create_frames}}
#' 
#' @export

get_maptypes <- function(map_service = NULL){
  map_types <- list(mapbox = names(getOption("moveVis.mapbox_types")), osm = names(getOption("moveVis.map_api")$osm))
  if(!is.null(map_service)) map_types[[map_service]] else map_types
}
