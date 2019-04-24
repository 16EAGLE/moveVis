#' Create frames of spatial movement maps for animation
#'
#' \code{frames_spatial} creates a list of \code{ggplot2} maps displaying movement. Each object represents a single frame. Each frame can be viewed or modified individually. The returned list of frames can be animated using \code{\link{animate_frames}}.
#'
#' @param m \code{move} or \code{moveStack} of uniform time scale and time lag, e.g. prepared with \code{\link{align_move}} (recommended). May contain a column named \code{colour} to control path colours (see \code{details}).
#' @param r_list list of \code{raster} or \code{rasterStack}. Each list element referrs to the times given in \code{r_times}. Use single-layer \code{raster} objects for gradient or discrete data (see \code{r_type}). Use a  \code{rasterStack} containing three bands for RGB imagery (in the order red, green, blue).
#' @param r_times list of \code{POSIXct} times. Each list element represents the time of the corresponding element in \code{r_list}. Must be of same length as \code{r_list}.
#' @param r_type character, either \code{"gradient"} or \code{"discrete"}. Ignored, if \code{r_list} contains \code{rasterStacks} of three bands, which are treated as RGB.
#' @param fade_raster logical, if \code{TRUE}, \code{r_list} is interpolated over time based on \code{r_times}. If \code{FALSE}, \code{r_list} elements are assigned to those frames closest to the equivalent times in \code{r_times}.
#' @param path_size numeric, size of each path.
#' @param path_end character, either \code{"round"}, \code{"butt"} or \code{"square"}, indicating the path end style.
#' @param path_join character, either \code{"round"}, \code{"mitre"} or \code{"bevel"}, indicating the path join style.
#' @param path_mitre numeric, path mitre limit (number greater than 1).
#' @param path_arrow arrow, path arrow specification, as created by grid::arrow().
#' @param path_colours character, a vector of colours. Must be of same length as number of individual tracks in \code{m} and refers to the order of tracks in \code{m}. If undefined (\code{NA}) and \code{m} contains a column named \code{colour}, colours provided within \code{m} are used (see details). Othwersie, colours are selected randomly per individual track.
#' @param path_alpha numeric, defines alpha (transparency) of the path. Value between 0 and 1. Default is 1.
#' @param path_legend logical, wether to add a path legend from \code{m} or not. Legend tracks and colours will be ordered by the tracks' temporal apperances, not by their order in \code{m}.
#' @param path_legend_title character, path legend title. Default is \code{"Names"}.
#' @param tail_length numeric, length of tail per movement path.
#' @param tail_size numeric, size of the last tail element. Default is 1.
#' @param tail_colour character, colour of the last tail element, to which the path colour is faded. Default is "white".
#' @param trace_show logical, whether to show the trace of the complete path or not.
#' @param trace_colour character, colour of the trace. Default is "white". It is recommended to define the same colours for both \code{trace_colour} and  \code{tail_colour} to enforce an uninterrupted colour transition form the tail to the trace.
#' @param margin_factor numeric, factor relative to the extent of \code{m} by which the frame extent should be increased around the movement area. Ignored, if \code{ext} is set.
#' @param equidistant logical, whether to make the map extent equidistant (squared) with y and x axis measuring equal distances or not. Especially in polar regions of the globe it might be necessaray to set \code{equidistant} to \code{FALSE} to avoid strong stretches. By default (\code{equidistant = NULL}), equidistant is set automatically to \code{FALSE}, if \code{ext} is set, otherwise \code{TRUE}. Read more in the details.
#' @param ext \code{sf bbox} or \code{sp extent} in same CRS as \code{m}, optional. If set, frames are cropped to this extent. If not set, a squared extent around \code{m}, optional with a margin set by \code{margin_factor}, is used (default).
#' @param map_service character, either \code{"osm"}, \code{"carto"} or \code{"mapbox"}. Default is \code{"osm"}.
#' @param map_type character, a map type, e.g. \code{"streets"}. For a full list of available map types, see \code{\link{get_maptypes}}.
#' @param map_res numeric, resolution of base map in range from 0 to 1.
#' @param map_token character, mapbox authentification token for mapbox basemaps. Register at \url{https://www.mapbox.com/} to get a mapbox token. Mapbox is free of charge after registration for up to 50.000 map requests per month. Ignored, if \code{map_service = "osm"}.
#' @param map_dir character, directory where downloaded basemap tiles can be stored. By default, a temporary directory is used. 
#' If you use moveVis often for the same area it is recommended to set this argument to a directory persistent throughout sessions (e.g. in your user folder), 
#' so that baesmap tiles that had been already downloaded by moveVis do not have to be requested again.
#' @param ... Additional arguments customizing the frame background, passed to \code{RStoolbox::ggR} or \code{RStoolbox::ggRGB}:
#'    \itemize{
#'        \item \code{alpha}, numeric, background transparency (0-1).
#'        \item \code{hue}, numeric, hue value for color calculation (0-1). Change if you need anything else than greyscale. Only effective if sat > 0.
#'        \item \code{sat}, numeric, saturation value for color calculation (0,1). Change if you need anything else than greyscale.
#'        \item \code{stretch}, character, either 'none', 'lin', 'hist', 'sqrt' or 'log' for no stretch, linear, histogram, square-root or logarithmic stretch.
#'        \item \code{quantiles}, numeric vector with two elements, min and max quantiles to stretch to. Defaults to 2% stretch, i.e. \code{c(0.02,0.98)}.
#'    }
#' @param verbose logical, if \code{TRUE}, messages on the function's progress are displayed (default).
#' 
#' @details If argument \code{path_colours} is not defined (set to \code{NA}), path colours can be defined by adding a character column named \code{colour} to \code{m}, containing a colour code or name per row (e.g. \code{"red"}. This way, for example, column \code{colour} for all rows belonging to individual A can be set to \code{"green"}, while column \code{colour} for all rows belonging to individual B can be set to \code{"red"}.
#' Colours could also be arranged to change through time or by behavioral segments, geographic locations, age, environmental or health parameters etc. If a column name \code{colour} in \code{m} is missing, colours will be selected automatically. Call \code{colours()} to see all available colours in R.
#' 
#' Basemap colour scales can be changed/added using \code{\link{add_colourscale}} or by using \code{ggplot2} commands (see \code{examples}). For continous scales, use \code{r_type = "gradient"}. For discrete scales, use \code{r_type = "discrete"}.
#' 
#' The projection of \code{m} is treated as target projection. Default base maps accessed through a map service will be reprojected into the projection of \code{m}. Thus, depending on the projection of \code{m}, it may happen that map labels are distorted. To get undistorted map labels, reproject \code{m} to the web mercator projection (the default projection of the base maps): \code{spTransform(m, crs("+init=epsg:3857"))}. The \code{ggplot2} coordinate system will be computed based on the projection of \code{m} using \code{coord_sf}. If argument \code{equidistant} is set, the map extent is calculated (thus enlarged into one axis direction) to represent equal surface distances on the x and y axis.
#'
#' @return List of ggplot2 objects, each representing a single frame.
#' 
#' @author Jakob Schwalb-Willmann
#' 
#' @importFrom raster compareCRS nlayers
#' @importFrom sf st_crs
#' @importFrom sp proj4string
#' @importFrom raster crs
#' @importFrom move n.indiv moveStack
#' 
#' @examples 
#' library(moveVis)
#' library(move)
#' library(ggplot2)
#' 
#' data("move_data")
#' # align movement
#' m <- align_move(move_data, res = 4, unit = "mins")
#' 
#' \donttest{
#' # with osm watercolor base map
#' frames <- frames_spatial(m, map_service = "osm", map_type = "watercolor")
#' # take a look at one of the frames, e.g. the 100th
#' frames[[100]]
#' 
#' # make base map a bit transparent
#' frames <- frames_spatial(m, map_service = "osm", map_type = "watercolor", alpha = 0.5)
#' frames[[100]] # take a look
#' 
#' # use a larger margin around extent
#' frames <- frames_spatial(m, map_service = "osm", map_type = "watercolor", alpha = 0.5,
#'                          margin_factor = 1.8)
#' 
#' # use a extent object as your AOI
#' ext <- extent(m)
#' ext@xmin <- ext@xmin - (ext@xmin*0.003)
#' ext@xmax <- ext@xmax + (ext@xmax*0.003)
#' frames <- frames_spatial(m, map_service = "osm", map_type = "watercolor", alpha = 0.5,
#'                          ext = ext)
#' 
#' # alter path appearance (make it longer and bigger)
#' frames <- frames_spatial(m, map_service = "osm", map_type = "watercolor", alpha = 0.5,
#'                          path_size = 4, tail_length = 29)
#'                          
#' # adjust path colours manually
#' frames <- frames_spatial(m, map_service = "osm", map_type = "watercolor", alpha = 0.5,
#'                          path_colours = c("black", "blue", "purple"))
#' 
#' # or do it directly within your moveStack, e.g. like:
#' m.list <- split(m) # split m into list by individual
#' m.list <- mapply(x = m.list, y = c("orange", "purple", "darkgreen"), function(x, y){
#'   x$colour <- y
#'   return(x)
#' }) # add colour per individual
#' m <- moveStack(m.list) # putting it back together into a moveStack
#' frames <- frames_spatial(m, map_service = "osm", map_type = "watercolor", alpha = 0.5)
#' # this way, you do not have to assign colours per individual track
#' # instead, you could assign colours by segment, age, speed or other variables
#' 
#' # get available map types
#' get_maptypes()
#' 
#' # use mapbox to get a satellite or other map types (register to on mapbox.com to get a token)
#' frames <- frames_spatial(m, map_service = "mapbox",
#'                          map_token = "your_token_from_your_mapbox_account",
#'                          map_type = "satellite")
#' 
#' # if you make a lot of calls to frames_spatial during mutliple sessions, use a map directory
#' # to save all base maps offline so that you do not have to query the servers each time
#' frames <- frames_spatial(m, map_service = "mapbox",
#'                          map_token = "your_token_from_your_mapbox_account",
#'                          map_type = "satellite",
#'                          map_dir = "your/map_directory/")                         
#' 
#' # use your own custom base maps
#' data("basemap_data")
#' r_list <- basemap_data[[1]]
#' r_times <- basemap_data[[2]]
#' 
#' # using gradient data (e.g. NDVI)
#' frames <- frames_spatial(m, r_list = r_list, r_times = r_times, r_type = "gradient",
#'                          fade_raster = TRUE)
#' 
#' # using discrete data (e.g. classifications)
#' # let's make up some classification data with 10 classes
#' r_list <- lapply(r_list, function(x){
#'   y <- raster::setValues(x, round(raster::getValues(x)*10))
#'   return(y)
#' })
#' # turn fade_raster to FALSE, since it makes no sense to temporally interpolate discrete classes
#' frames <- frames_spatial(m, r_list = r_list, r_times = r_times, r_type = "discrete",
#'                          fade_raster = FALSE)
#' 
#' # animate the frames created with frames_spatial;
#' animate_frames(frames, out_file = tempfile(fileext = ".gif"))
#' 
#' # see ?add_colourscale to learn how to change colours of custom base maps
#' # see all add_ functions on how to customize your frames created with frames_spatial
#' # or frames_graph
#' # see ?animate_frames on how to animate your list of frames
#' }
#' @seealso \code{\link{frames_graph}} \code{\link{join_frames}} \code{\link{animate_frames}}
#' @export

frames_spatial <- function(m, r_list = NULL, r_times = NULL, r_type = "gradient", fade_raster = FALSE, map_service = "osm", map_type = "streets", map_res = 1, map_token = NULL, map_dir = NULL,
                           margin_factor = 1.1, equidistant = NULL, ext = NULL, path_size = 3, path_end = "round", path_join = "round", path_mitre = 10, path_arrow = NULL, path_colours = NA, path_alpha = 1,
                           path_legend = TRUE, path_legend_title = "Names", tail_length = 19, tail_size = 1, tail_colour = "white", trace_show = FALSE, trace_colour = "white", ..., verbose = TRUE){
  
  ## check input arguments
  if(inherits(verbose, "logical")) options(moveVis.verbose = verbose)
  if(all(!c(inherits(m, "MoveStack"), inherits(m, "Move")))) out("Argument 'm' must be of class 'Move' or 'MoveStack'.", type = 3)
  if(inherits(m, "Move")) m <- moveStack(m)
  
  ## check m time conformities
  .time_conform(m)

  if(!is.null(r_list)){
    if(all(!is.list(r_list), inherits(r_list, "Raster"))) r_list <- list(r_list)
    if(any(!sapply(r_list, compareCRS, y = m))) out("Projections of 'm' and 'r_list' differ.", type = 3)
    if(length(unique(sapply(r_list, nlayers))) > 1) out("Number of layers per raster object in list 'r' differ.", type = 3)
    if(!inherits(r_times, "POSIXct")) out("Argument 'r_times' must be of type 'POSIXct' if 'r_list' is defined.", type = 3)
    if(!isTRUE(r_type %in% c("gradient", "discrete", "RGB"))) out("Argument 'r_type' must eihter be 'gradient', 'discrete' or 'RGB'.", type = 3)
    if(!is.logical(fade_raster)) out("Argument 'fade_raster' has to be either TRUE or FALSE.", type = 3)
  } else{
    if(!isTRUE(tolower(map_service) %in% names(get_maptypes()))) out(paste0("Argument 'map_service' must be ", paste0(names(moveVis::get_maptypes()), collapse = ", ")))
    if(!isTRUE(tolower(map_type) %in% get_maptypes(map_service))) out("The defined map type is not supported for the selected service. Use get_maptypes() to get all available map types.", type = 3)
    if(!is.numeric(map_res)) out("Argument 'map_res' must be 'numeric'.", type = 3)
    if(any(map_res < 0, map_res > 1)) out("Argument 'map_res' must be a value between 0 and 1.", type = 3)
    if(all(!inherits(map_token, "character"), map_service == "mapbox")) out("Argument 'map_token' must be defined to access a basemap, if 'r_list' is not defined and 'map_service' is 'mapbox'.", type = 3)
    if(is.null(map_dir)){
      map_dir <- paste0(tempdir(), "/moveVis/basemap/")
      if(!dir.exists(map_dir)) dir.create(map_dir, recursive = T)
    } else{
      if(!dir.exists(map_dir)) out("The directory defined with 'map_dir' does not exist.", type = 3)
    }
  }
  num.args <- c(margin_factor = margin_factor, tail_length = tail_length, tail_size = tail_size, path_size = path_size, path_mitre = path_mitre)
  catch <- sapply(1:length(num.args), function(i) if(!is.numeric(num.args[[i]])) out(paste0("Argument '", names(num.args)[[i]], "' must be of type 'numeric'."), type = 3))
  char.args <- c(path_end = path_end, path_join = path_join, path_legend_title = path_legend_title)
  catch <- sapply(1:length(char.args), function(i) if(!is.character(char.args[[i]])) out(paste0("Argument '", names(char.args)[[i]], "' must be of type 'numeric'."), type = 3))
  
  if(!is.null(ext)) if(!inherits(ext, "Extent")) out("Argument 'ext' must be of type 'Extent' (see raster::extent), if defined.", type = 3)
  if(!is.null(path_arrow)) if(!inherits(path_arrow, "arrow")) out("Argument 'path_arrow' must be of type 'arrrow' (see grid::arrow), if defined.", type = 3)
  if(is.character(path_colours)) if(length(path_colours) != n.indiv(m)) out("Argument 'path_colours' must be of same length as the number of individual tracks of 'm', if defined. Alternatively, use a column 'colour' for individual colouring per coordinate within 'm' (see details of ?frames_spatial).", type = 3)
  if(!is.logical(path_legend)) out("Argument 'path_legend' must be of type 'logical'.", type = 3)
  if(is.null(equidistant)) if(is.null(ext)) equidistant <- TRUE else equidistant <- FALSE
  if(!is.logical(equidistant)) out("Argument 'equidistant' must be of type 'logical'.", type = 3)
  
  ## preprocess movement data
  out("Processing movement data...")
  m.df <- .m2df(m, path_colours = path_colours) # create data.frame from m with frame time and colour
  .stats(max(m.df$frame))
  
  gg.ext <- .ext(m.df, m.crs = st_crs(proj4string(m)), ext, margin_factor, equidistant) # calcualte extent
  m.split <- .split(m.df, tail_length = tail_length, path_size = path_size, tail_size = tail_size, tail_colour = tail_colour,
                    trace_show = trace_show, trace_colour = trace_colour) # split m by size of tail
  
  ## calculate tiles and get map imagery
  if(is.null(r_list)){
    out("Retrieving and compositing basemap imagery...")
    r_list <- .getMap(gg.ext, map_service, map_type, map_token, map_dir, map_res, m.crs = crs(m))
    r_type <- "RGB"
  }
  
  out("Assigning raster maps to frames...")
  r_list <- .rFrames(r_list, r_times, m.split, gg.ext, fade_raster)
  
  ## plot basemap
  if(length(r_list) == 1){
    if(r_type == "gradient") gg.bmap <- .lapply(r_list[[1]], ggR, ggObj = T, geom_raster = T, coord_equal = F, ...)
    if(r_type == "discrete") gg.bmap <- .lapply(r_list[[1]], ggR, ggObj = T, geom_raster = T, forceCat = T, coord_equal = F, ...)
  } else{ gg.bmap <- .lapply(1:length(r_list[[1]]), function(i) ggRGB(stack(lapply(r_list, "[[", i)),  r = 1, g = 2, b = 3, ggObj = T, geom_raster = T, coord_equal = F, ...))}
  
  ## return frames
  out("Creating frames...")
  return(.gg_spatial(m.split = m.split, gg.bmap = gg.bmap, m.df = m.df, m.crs = proj4string(m), equidistant = equidistant,
                     path_size = path_size, path_end = path_end, path_join = path_join, path_alpha = path_alpha, path_mitre = path_mitre,
                     path_arrow = path_arrow, print_plot = F, path_legend = path_legend, path_legend_title = path_legend_title))
}