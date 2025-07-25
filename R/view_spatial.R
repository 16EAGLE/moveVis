#' View movements on an interactive map
#'
#' \code{view_spatial} is a simple wrapper that displays movement tracks on an interactive \code{mapview} or \code{leaflet} map.
#'
#' @inheritParams frames_spatial
#' @param m \code{move} or \code{moveStack}. May contain a column named \code{colour} to control path colours (see \code{details}).
#' @param render_as character, either \code{'mapview'} to return a \code{mapview} map or \code{'leaflet'} to return a \code{leaflet} map. 
#' @param time_labels logical, wether to display timestamps for each track fix when hovering it with the mouse cursor.
#' @param stroke logical, whether to draw stroke around circles.
#' 
#' @details If argument \code{path_colours} is not defined (set to \code{NA}), path colours can be defined by adding a character column named \code{colour} to \code{m}, containing a colour code or name per row (e.g. \code{"red"}. This way, for example, column \code{colour} for all rows belonging to individual A can be set to \code{"green"}, while column \code{colour} for all rows belonging to individual B can be set to \code{"red"}.
#' Colours could also be arranged to change through time or by behavioral segments, geographic locations, age, environmental or health parameters etc. If a column name \code{colour} in \code{m} is missing, colours will be selected automatically. Call \code{colours()} to see all available colours in R.
#' 
#' @return An interatcive \code{mapview} or \code{leaflet} map.
#' 
#' @author Jakob Schwalb-Willmann
#' 
#' 
#' @examples 
#' \dontrun{
#' library(moveVis)
#' library(move)
#' 
#' data("move_data")
#' 
#' # return a mapview map (mapview must be installed)
#' view_spatial(move_data)
#' 
#' # return a leaflet map (leaflet must be installed)
#' view_spatial(move_data, render_as = "leaflet")
#' 
#' # turn off time labels and legend
#' view_spatial(move_data, time_labels = FALSE, path_legend = FALSE)
#' 
#' }
#' @seealso \code{\link{frames_spatial}}
#' 
#' @importFrom move2 mt_n_tracks mt_track_id mt_track_id_column
#' @importFrom sf st_coordinates
#' @export

view_spatial <- function(m, render_as = "mapview", time_labels = TRUE, stroke = TRUE, path_colours = NA, path_legend = TRUE,
                         path_legend_title = "Names", verbose = TRUE){
  
  ## dependency check
  if(is.character(render_as)){
    if(!isTRUE(render_as %in% c("mapview", "leaflet"))) out("Argument 'render_as' must be either 'mapview' or 'leaflet'.", type = 3)
  } else{out("Argument 'render_as' must be of type 'character'.", type = 3)}

  ## check input arguments
  if(inherits(verbose, "logical")) options(moveVis.verbose = verbose)
  if(all(!inherits(m, "move2"))) out("Argument 'm' must be of class 'move2'.", type = 3)
  
  if(is.character(path_colours)) if(length(path_colours) != mt_n_tracks(m)) out("Argument 'path_colours' must be of same length as the number of individual tracks of 'm', if defined. Alternatively, use a column 'colour' for individual colouring per coordinate within 'm' (see details of ?frames_spatial).", type = 3)
  if(!is.logical(path_legend)) out("Argument 'path_legend' must be of type 'logical'.", type = 3)
  if(!is.logical(time_labels)) out("Argument 'time_labels' must be of type 'logical'.", type = 3)
  if(!is.character(path_legend_title)) out("Argument 'path_legend_title' must be of type 'character'.", type = 3)
  
  ## preprocess movement data
  m <- .add_m_attributes(m, path_colours = path_colours)
  
  ## render as mapview object
  if(render_as == "mapview"){
    if(length(grep("mapview", rownames(utils::installed.packages()))) == 0) out("'mapview' has to be installed to use this function. Use install.packages('mapview').", type = 3)
    
    # compose
    map <- mapview::mapview(
      m, map.types = "OpenStreetMap", xcol = "x", ycol = "y", zcol = mt_track_id_column(m), legend = path_legend,
      crs = st_crs(m)$proj4string, grid = F, layer.name = path_legend_title,
      col.regions = unique(m$colour),
      label = if(isTRUE(time_labels)) mt_time(m) else NULL, stroke = stroke
    )
  }
  
  ## render as leaflet object
  if(render_as == "leaflet"){
    if(length(grep("leaflet", rownames(utils::installed.packages()))) == 0) out("'leaflet' has to be installed to use this function. Use install.packages('leaflet').", type = 3)
    
    # compose
    m.split <- split(m, mt_track_id(m))
    map <- leaflet::addTiles(map = leaflet::leaflet(m))
    for(i in 1:length(m.split)) map <- leaflet::addCircleMarkers(
      map = map, lng = st_coordinates(m.split[[i]])[,1], 
      lat = st_coordinates(m.split[[i]])[,2],
      radius = 5.5, color = "black", stroke = stroke, fillColor = m.split[[i]]$colour, fillOpacity = 0.6, weight = 2, opacity = 1, 
      label = if(isTRUE(time_labels)) as.character(mt_time(m.split[[i]])) else NULL)
    map <- leaflet::addScaleBar(map = leaflet::addLegend(
      map = map, colors = unique(m$colour),
      labels = as.character(unique(mt_track_id(m))), opacity = 1, title = path_legend_title), position = "bottomleft"
    )
    
  }
  return(map)
}