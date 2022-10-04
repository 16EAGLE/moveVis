#' Create frames of spatial movement maps for animation
#'
#' \code{frames_spatial} creates frames from movement and map/raster data. If no custom raster data is provided, a basemap is pulled from a map tile service using the \code{basemaps} package. Frames are returned as an object of class \code{moveVis} and can be subsetted, viewed (see \code{\link{render_frame}}), modified (see \code{\link{add_gg}} and associated functions ) and animated (see \code{\link{animate_frames}}).
#'
#' @param m \code{move} or \code{moveStack} of uniform time scale and time lag, e.g. prepared with \code{\link{align_move}} (recommended). May contain a column named \code{colour} to control path colours (see \code{details}).
#' @param r_list list of \code{raster} or \code{rasterStack}. Each list element refers to the times given in \code{r_times}. Use single-layer \code{raster} objects for gradient or discrete data (see \code{r_type}). Use a  \code{rasterStack} containing three bands for RGB imagery (in the order red, green, blue).
#' @param r_times list of \code{POSIXct} times. Each list element represents the time of the corresponding element in \code{r_list}. Must be of same length as \code{r_list}.
#' @param r_type character, either \code{"gradient"} or \code{"discrete"}. Ignored, if \code{r_list} contains \code{rasterStacks} of three bands, which are treated as RGB.
#' @param fade_raster logical, if \code{TRUE}, \code{r_list} is interpolated over time based on \code{r_times}. If \code{FALSE}, \code{r_list} elements are assigned to those frames closest to the equivalent times in \code{r_times}.
#' @param crop_raster logical, whether to crop rasters in \code{r_list} to plot extent before plotting or not.
#' @param path_size numeric, size of each path.
#' @param path_end character, either \code{"round"}, \code{"butt"} or \code{"square"}, indicating the path end style.
#' @param path_join character, either \code{"round"}, \code{"mitre"} or \code{"bevel"}, indicating the path join style.
#' @param path_mitre numeric, path mitre limit (number greater than 1).
#' @param path_arrow arrow, path arrow specification, as created by grid::arrow().
#' @param path_colours character, a vector of colours. Must be of same length as number of individual tracks in \code{m} and refers to the order of tracks in \code{m}. If undefined (\code{NA}) and \code{m} contains a column named \code{colour}, colours provided within \code{m} are used (see details). Othwersie, colours are selected from a standard rainbow palette per individual track.
#' @param path_alpha numeric, defines alpha (transparency) of the path. Value between 0 and 1. Default is 1.
#' @param path_fade logical, whether paths should be faded towards the last frame or not. Useful, if \code{trace_show = TRUE} and you want to hold the last frame using \code{end_pause} in \code{\link{animate_frames}}.
#' @param path_legend logical, wether to add a path legend from \code{m} or not. Legend tracks and colours will be ordered by the tracks' temporal apperances, not by their order in \code{m}.
#' @param path_legend_title character, path legend title. Default is \code{"Names"}.
#' @param tail_length numeric, length of tail per movement path.
#' @param tail_size numeric, size of the last tail element. Default is 1.
#' @param tail_colour character, colour of the last tail element, to which the path colour is faded. Default is "white".
#' @param trace_show logical, whether to show the trace of the complete path or not.
#' @param trace_size numeric, size of the trace. Default is same as \code{tail_size}.
#' @param trace_colour character, colour of the trace. Default is "white". It is recommended to define the same colours for both \code{trace_colour} and  \code{tail_colour} to enforce an uninterrupted colour transition form the tail to the trace.
#' @param cross_dateline logical, whether tracks are crossing the dateline (longitude 180/-180) or not. If \code{TRUE}, frames are expanded towards the side of the dateline that is smaller in space. Applies only if the CRS of \code{m} is not projected (geographical, lon/lat). If \code{FALSE} (default), frames are clipped at the minimum and maximum longitudes and tracks cannot cross.
#' @param margin_factor numeric, factor relative to the extent of \code{m} by which the frame extent should be increased around the movement area. Ignored, if \code{ext} is set.
#' @param equidistant logical, whether to make the map extent equidistant (squared) with y and x axis measuring equal distances or not. Especially in polar regions of the globe it might be necessaray to set \code{equidistant} to \code{FALSE} to avoid strong stretches. By default (\code{equidistant = NULL}), equidistant is set automatically to \code{FALSE}, if \code{ext} is set, otherwise \code{TRUE}. Read more in the details.
#' @param ext \code{sf bbox} or \code{sp extent} in same CRS as \code{m}, optional. If set, frames are cropped to this extent. If not set, a squared extent around \code{m}, optional with a margin set by \code{margin_factor}, is used (default).
#' @param map_service character, a map service, e.g. \code{"osm"}. Use \code{\link{get_maptypes}} for a list of available map services and types..
#' @param map_type character, a map type, e.g. \code{"streets"}. Use \code{\link{get_maptypes}} for available map services and types.
#' @param map_res numeric, resolution of base map in range from 0 to 1.
#' @param map_token character, mapbox authentification token for mapbox basemaps. Register at \url{https://www.mapbox.com/} to get a mapbox token. Mapbox is free of charge after registration for up to 50.000 map requests per month. Ignored, if \code{map_service = "osm"}.
#' @param map_dir character, directory where downloaded basemap tiles can be stored. By default, a temporary directory is used. 
#' If you use moveVis often for the same area it is recommended to set this argument to a directory persistent throughout sessions (e.g. in your user folder), 
#' so that baesmap tiles that had been already downloaded by moveVis do not have to be requested again.
#' @param ... Additional arguments customizing the frame background:
#'    \itemize{
#'        \item \code{alpha}, numeric, background transparency (0-1).
#'        \item \code{maxpixels}, maximum number of pixels to be plotted per frame. Defaults to 500000. Reduce to decrease detail and increase rendering speeds.
#'        \item \code{macColorValue}, numeric, only relevant for RGB backgrounds (i.e. if \code{r_type = "RGB"} or if a default base map is used). Maximum colour value (e.g. 255). Defaults to maximum raster value.
#'    }
#' @param verbose logical, if \code{TRUE}, messages and progress information are displayed on the console (default).
#' 
#' @details If argument \code{path_colours} is not defined (set to \code{NA}), path colours can be defined by adding a character column named \code{colour} to \code{m}, containing a colour code or name per row (e.g. \code{"red"}. This way, for example, column \code{colour} for all rows belonging to individual A can be set to \code{"green"}, while column \code{colour} for all rows belonging to individual B can be set to \code{"red"}.
#' Colours could also be arranged to change through time or by behavioral segments, geographic locations, age, environmental or health parameters etc. If a column name \code{colour} in \code{m} is missing, colours will be selected automatically. Call \code{colours()} to see all available colours in R.
#' 
#' Basemap colour scales can be changed/added using \code{\link{add_colourscale}} or by using \code{ggplot2} commands (see \code{examples}). For continous scales, use \code{r_type = "gradient"}. For discrete scales, use \code{r_type = "discrete"}.
#' 
#' If argument \code{equidistant} is set, the map extent is calculated (thus enlarged into one axis direction) to represent equal surface distances on the x and y axis.
#'
#' @note 
#' 
#' The use of the map services \code{"osm_thunderforest"} and \code{"mapbox"} require registration to obtain an API token/key which can be supplied to \code{map_token}. Register at \url{https://www.thunderforest.com/} and/or \url{https://www.mapbox.com/} to get a token.
#' 
#' The projection of \code{m} is treated as target projection. Default basemaps accessed through a map service will be reprojected into the projection of \code{m}. Thus, depending on the projection of \code{m}, it may happen that map labels are distorted. To get undistorted map labels, reproject \code{m} to the web mercator projection (the default projection for basemaps): \code{spTransform(m, crs("+init=epsg:3857"))}
#'
#' @return A frames object of class \code{moveVis}.
#' 
#' @author Jakob Schwalb-Willmann
#' 
#' @importFrom raster compareCRS nlayers crs
#' @importFrom sf st_crs st_bbox
#' @importFrom move n.indiv moveStack
#' @importFrom basemaps basemap_raster
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
#' \dontrun{
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
#' # frames <- frames_spatial(m, map_service = "mapbox",
#' #                          map_token = "your_token_from_your_mapbox_account",
#' #                          map_type = "satellite")
#' 
#' # if you make a lot of calls to frames_spatial during mutliple sessions, use a map directory
#' # to save all base maps offline so that you do not have to query the servers each time
#' # frames <- frames_spatial(m, map_service = "mapbox",
#' #                          map_token = "your_token_from_your_mapbox_account",
#' #                          map_type = "satellite",
#' #                          map_dir = "your/map_directory/")                         
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
#' # then simply animate the frames using animate_frames
#' # see ?add_colourscale to learn how to change colours of custom base maps
#' # see all add_ functions on how to customize your frames created with frames_spatial
#' # or frames_graph
#' # see ?animate_frames on how to animate a frames
#' }
#' @seealso \code{\link{frames_graph}} \code{\link{join_frames}} \code{\link{animate_frames}}
#' @export

frames_spatial <- function(m, r_list = NULL, r_times = NULL, r_type = "gradient", fade_raster = FALSE, crop_raster = TRUE, map_service = "osm", map_type = "streets", map_res = 1, map_token = NULL, map_dir = NULL,
                           margin_factor = 1.1, equidistant = NULL, ext = NULL, path_size = 3, path_end = "round", path_join = "round", path_mitre = 10, path_arrow = NULL, path_colours = NA, path_alpha = 1, path_fade = FALSE,
                           path_legend = TRUE, path_legend_title = "Names", tail_length = 19, tail_size = 1, tail_colour = "white", trace_show = FALSE, trace_size = tail_size, trace_colour = "white", cross_dateline = FALSE, ..., verbose = TRUE){
  
  ## check input arguments
  if(inherits(verbose, "logical")) options(moveVis.verbose = verbose)
  if(all(!c(inherits(m, "MoveStack"), inherits(m, "Move")))) out("Argument 'm' must be of class 'Move' or 'MoveStack'.", type = 3)
  if(inherits(m, "Move")) m <- moveStack(m)
  
  if(!is.null(r_list)){
    if(all(!is.list(r_list), inherits(r_list, "Raster"))) r_list <- list(r_list)
    if(any(!sapply(r_list, compareCRS, y = m))) out("Projections of 'm' and 'r_list' differ.", type = 3)
    if(length(unique(sapply(r_list, nlayers))) > 1) out("Number of layers per raster object in list 'r' differ.", type = 3)
    if(!inherits(r_times, "POSIXct")) out("Argument 'r_times' must be of type 'POSIXct' if 'r_list' is defined.", type = 3)
    if(!isTRUE(r_type %in% c("gradient", "discrete", "RGB"))) out("Argument 'r_type' must eihter be 'gradient', 'discrete' or 'RGB'.", type = 3)
    if(!is.logical(fade_raster)) out("Argument 'fade_raster' has to be either TRUE or FALSE.", type = 3)
    if(!is.logical(crop_raster)) out("Argument 'crop_raster' has to be either TRUE or FALSE.", type = 3)
  } else{
    if(isFALSE(tolower(map_service) %in% names(get_maptypes()))) out(paste0("Argument 'map_service' must be ", paste0(names(moveVis::get_maptypes()), collapse = ", ")))
    if(isFALSE(tolower(map_type) %in% get_maptypes(map_service))) out("The defined map type is not supported for the selected service. Use get_maptypes() to get all available map types.", type = 3)
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
  num.args <- c(margin_factor = margin_factor, tail_length = tail_length, tail_size = tail_size, path_size = path_size, path_mitre = path_mitre, trace_size = trace_size)
  catch <- sapply(1:length(num.args), function(i) if(!is.numeric(num.args[[i]])) out(paste0("Argument '", names(num.args)[[i]], "' must be of type 'numeric'."), type = 3))
  char.args <- c(path_end = path_end, path_join = path_join, path_legend_title = path_legend_title)
  catch <- sapply(1:length(char.args), function(i) if(!is.character(char.args[[i]])) out(paste0("Argument '", names(char.args)[[i]], "' must be of type 'numeric'."), type = 3))
  extras <- list(...)
  
  #if(!is.null(ext)){
  #  if(!inherits(ext, "Extent")) out("Argument 'ext' must be of type 'Extent' (see raster::extent), if defined.", type = 3)
  #  if(isTRUE(ext < extent(m))) out("The frame extent defined using argument 'ext' is smaller than extent(m). Be aware that movements outside of 'ext' will be clipped.", type = 2)
  #}
  if(!is.null(path_arrow)) if(!inherits(path_arrow, "arrow")) out("Argument 'path_arrow' must be of type 'arrrow' (see grid::arrow), if defined.", type = 3)
  if(is.character(path_colours)) if(length(path_colours) != n.indiv(m)) out("Argument 'path_colours' must be of same length as the number of individual tracks of 'm', if defined. Alternatively, use a column 'colour' for individual colouring per coordinate within 'm' (see details of ?frames_spatial).", type = 3)
  if(!is.logical(path_legend)) out("Argument 'path_legend' must be of type 'logical'.", type = 3)
  if(is.null(equidistant)) if(is.null(ext)) equidistant <- TRUE else equidistant <- FALSE
  if(!is.logical(equidistant)) out("Argument 'equidistant' must be of type 'logical'.", type = 3)
  
  if(all(isTRUE(cross_dateline), !is.null(r_list))) out("Argument 'cross_dateline' only works with default base maps. Arguments 'r_list' and 'r_times' cannot be used, if cross_dateline = TRUE.\nTip: Reproject 'm' to another CRS that better suits the region if you want to use 'r_list' with tracks crossing the dateline.", type = 3)
  
  ## check m time conformities
  out("Checking temporal alignment...")
  .time_conform(m)
  
  ## preprocess movement data
  out("Processing movement data...")
  m.crs <- st_crs(m)
  if(isTRUE(cross_dateline)){
    equidistant <- FALSE
    if(m.crs != st_crs(4326)) out("Since arugment 'cross_dateline' is TRUE, 'm' will be transformed to Geographic Coordinates (EPSG 4326, Lat/Lon WGS84)", type = 2)
    m.crs <- st_crs(4326) 
  }
  m.df <- .m2df(m, path_colours = path_colours, return_latlon = cross_dateline) # create data.frame from m with frame time and colour
  .stats(n.frames = max(m.df$frame))
  
  gg.ext <- .ext(m.df, m.crs, ext, margin_factor, equidistant, cross_dateline) # calculate extent
  
  
  ## shift coordinates crossing dateline
  if(isTRUE(cross_dateline)){
    rg <- c("pos" = diff(range(m.df$x[m.df$x >= 0])), "neg" = diff(range(m.df$x[m.df$x < 0])))
    if(which.max(rg) == 1){
      m.df$x[m.df$x < 0] <- 180+m.df$x[m.df$x < 0]+180
    } else{
      m.df$x[m.df$x >= 0] <- -180+m.df$x[m.df$x >= 0]-180
    }
  }
  
  ## calculate tiles and get map imagery
  if(is.null(r_list)){
    out("Retrieving and compositing basemap imagery...")
    r_list <- list(suppressWarnings(basemap_raster(
      ext = gg.ext, map_service = map_service, map_type = map_type,
      map_res = map_res, map_token = map_token, map_dir = map_dir, verbose = verbose,
      custom_crs = as.character(m.crs$wkt), ...
      #custom_crs =  as.character(raster::crs(m)), ...
    )))
    if(all(map_service == "mapbox", map_type == "terrain")) r_type = "gradient" else r_type <- "RGB"
  } else{
    map_service <- "custom"
    map_type <- "custom"
  }
  
  # calculate frames extents and coord labes
  if(isTRUE(cross_dateline)){
    gg.ext <- st_bbox(extent(r_list[[1]]), crs = m.crs)
    
    # use coord_equal for dateline crossingngs in EPSG:4326 only
    m.df$coord <- list(ggplot2::coord_sf(xlim = c(gg.ext$xmin, gg.ext$xmax), ylim = c(gg.ext$ymin, gg.ext$ymax),
                                         expand = F, clip = "on"))
    m.df$scalex <- list(ggplot2::scale_x_continuous(labels = .x_labels))
    m.df$scaley <- list(ggplot2::scale_y_continuous(labels = .y_labels))
  } else{
    
    # use coord_sf for all other cases
    m.df$coord <- list(ggplot2::coord_sf(xlim = c(gg.ext$xmin, gg.ext$xmax), ylim = c(gg.ext$ymin, gg.ext$ymax),
                                         expand = F, crs = st_crs(m), datum = st_crs(m), clip = "on"))
    m.df$scaley <- m.df$scalex <- NULL
  }
  
  out("Assigning raster maps to frames...")
  r_list <- .rFrames(r_list, r_times, m.df, gg.ext, fade_raster, crop_raster = crop_raster)
  
  # create frames object
  frames <- list(
    move_data = m.df,
    raster_data = r_list,
    aesthetics = c(list(
      equidistant = equidistant,
      path_size = path_size,
      path_end = path_end,
      path_join = path_join,
      path_alpha = path_alpha,
      path_mitre = path_mitre,
      path_arrow = path_arrow, 
      path_legend = path_legend,
      path_legend_title = path_legend_title,
      tail_length = tail_length,
      tail_size = tail_size,
      tail_colour = tail_colour,
      trace_show = trace_show,
      trace_size = trace_size,
      trace_colour = trace_colour,
      path_fade = path_fade,
      gg.ext = gg.ext,
      map_service = map_service,
      map_type = map_type,
      r_type = r_type),
      maxpixels = if(!is.null(extras$maxpixels)) extras$maxpixels else 500000,
      alpha = if(!is.null(extras$alpha)) extras$alpha else 1,
      maxColorValue = if(!is.null(extras$maxColorValue)) extras$maxColorValue else NA),
    additions = NULL
  )
  attr(frames, "class") <- c("moveVis", "frames_spatial")
  
  return(frames)
}