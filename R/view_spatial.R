#' View movements on an interactive map
#'
#' \code{view_spatial} is a simple wrapper that displays movement tracks on an interactive \code{mapview} or \code{leaflet} map.
#'
#' @inheritParams frames_spatial
#' @param m \code{move2} object.
#' @param render_as character, either \code{'mapview'} to return a \code{mapview} map or \code{'leaflet'} to return a \code{leaflet} map. 
#' @param time_labels logical, whether to display timestamps for each track fix when hovering it with the mouse cursor.
#' @param stroke logical, whether to draw stroke around circles.
#' 
#' @return An interactive \code{mapview} or \code{leaflet} map.
#' 
#' @author Jakob Schwalb-Willmann
#' 
#' 
#' @examples 
#' \dontrun{
#' library(moveVis)
#' library(move2)
#'
#' data("move_data", package = "moveVis")
#'
#' view_spatial(move_data)
#'  
#' # return a leaflet map (leaflet must be installed)
#' view_spatial(move_data, render_as = "leaflet")
#'  
#' # turn off time labels and legend
#' view_spatial(move_data, time_labels = FALSE, path_legend = FALSE)
#' }
#' @seealso \code{\link{frames_spatial}}
#' 
#' @importFrom move2 mt_n_tracks mt_track_id mt_track_id_column
#' @importFrom sf st_coordinates
#' @export

view_spatial <- function(m, render_as = "mapview", time_labels = TRUE, stroke = TRUE, path_colours = NULL, colour_paths_by = move2::mt_track_id_column(m), path_legend = TRUE,
                         path_legend_title = colour_paths_by, verbose = TRUE){
  
  ## dependency check
  if(is.character(render_as)){
    if(!isTRUE(render_as %in% c("mapview", "leaflet"))) out("Argument 'render_as' must be either 'mapview' or 'leaflet'.", type = 3)
  } else{out("Argument 'render_as' must be of type 'character'.", type = 3)}
  
  ## check input arguments
  if(inherits(verbose, "logical")) options(moveVis.verbose = verbose)
  if(all(!inherits(m, "move2"))) out("Argument 'm' must be of class 'move2'.", type = 3)
  
  if(!is.logical(path_legend)) out("Argument 'path_legend' must be of type 'logical'.", type = 3)
  if(!is.logical(time_labels)) out("Argument 'time_labels' must be of type 'logical'.", type = 3)
  if(!is.character(path_legend_title)) out("Argument 'path_legend_title' must be of type 'character'.", type = 3)
  
  # Units do not always cooperate with color scales...
  m[[colour_paths_by]] <- .drop_units_safe(m[[colour_paths_by]])
  m <- .expand_track_attr(m, var = colour_paths_by)
  
  pal <- .build_pal(m[[colour_paths_by]], path_colours)

  ## preprocess movement data
  m <- .add_m_attributes(m)
  
  ## render as mapview object
  if(render_as == "mapview"){
    if(length(grep("mapview", rownames(utils::installed.packages()))) == 0) out("'mapview' has to be installed to use this function. Use install.packages('mapview').", type = 3)
    
    # compose
    map <- mapview::mapview(
      m, map.types = "OpenStreetMap", xcol = "x", ycol = "y", zcol = colour_paths_by, legend = path_legend,
      crs = st_crs(m)$proj4string, grid = F, layer.name = path_legend_title,
      col.regions = pal,
      label = if(isTRUE(time_labels)) mt_time(m) else NULL, stroke = stroke
    )
  }
  
  ## render as leaflet object
  if(render_as == "leaflet"){
    if(length(grep("leaflet", rownames(utils::installed.packages()))) == 0) out("'leaflet' has to be installed to use this function. Use install.packages('leaflet').", type = 3)
    
    map <- leaflet::addTiles(map = leaflet::leaflet(m))
    
    var_type <- .scale_type(m[[colour_paths_by]])
    
    # Need to convert palette to leaflet format (colorNumeric and colorFactor)
    # to get correct legend behavior
    if (var_type == "continuous") {
      # Remove units, which leaflet can't handle.
      m[[colour_paths_by]] <- as.numeric(m[[colour_paths_by]])
      
      leaflet_scale <- leaflet::colorNumeric(
        palette = pal(256),
        domain = m[[colour_paths_by]]
      )
    } else {
      leaflet_scale <- leaflet::colorFactor(
        palette = pal(length(unique(m[[colour_paths_by]]))),
        domain = unique(m[[colour_paths_by]])
      )
    }
    
    map <- leaflet::addCircleMarkers(
      map = map, 
      lng = st_coordinates(m)[,1],
      lat = st_coordinates(m)[,2],
      radius = 5.5, 
      color = "black", 
      stroke = stroke, 
      fillColor = ~ leaflet_scale(m[[colour_paths_by]]),
      fillOpacity = 0.6, 
      weight = 2, 
      opacity = 1, 
      label = if(isTRUE(time_labels)) as.character(mt_time(m)) else m[[colour_paths_by]]
    )
    
    map <- leaflet::addScaleBar(
      map = leaflet::addLegend(
        map = map, 
        pal = leaflet_scale,
        values = m[[colour_paths_by]], 
        opacity = 1, 
        title = path_legend_title
      ), 
      position = "bottomleft"
    )
  }
  
  return(map)
}