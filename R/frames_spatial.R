#' Create frames of spatial movement maps for animation
#'
#' \code{frames_spatial} creates a list of \code{ggplot2} objects of which each represents a single frame. Each frame can be viewed or modified individually. The returned list of frames can be animated using \code{\link{animate_frames}}.
#'
#' @param m \code{move} or \code{moveStack} of uniform time scale and time lag, e.g. prepared with \code{\link{align_move}} (recommended). May contain a column named \code{colour} to control path colours (see \code{details}).
#' @param r_list list of \code{raster} or \code{rasterStack}. Each list element referrs to the times given in \code{r_times}. Use single-layer \code{raster} objects for gradient or discrete data (see \code{r_type}). Use a  \code{rasterStack} containing three bands for RGB imagery (in the order red, green, blue).
#' @param r_times list of \code{POSIXct} times. Each list element represents the time of the corresponding element in \code{r_list}. Must be of same length as \code{r_list}.
#' @param r_type character, either \code{"gardient"} or \code{"discrete"}. Ignored, if \code{r_list} contains \code{rasterStacks} of three bands, which are treated as RGB.
#' @param fade_raster logical, if \code{TRUE}, \code{r_list} is interpolated over time based on \code{r_times}. If \code{FALSE}, \code{r_list} elements are assigned to those frames closest to the equivalent times in \code{r_times}.
#' @param tail_length numeric, length of tail per movement path.
#' @param path_size numeric, size of each path.
#' @param tail_size numeric, size of the last tail element. Default is 1.
#' @param path_end character, either \code{"round"}, \code{"butt"} or \code{"square"}, indicating the path end style.
#' @param path_join character, either \code{"round"}, \code{"mitre"} or \code{"bevel"}, indicating the path join style.
#' @param path_mitre numeric, path mitre limit (number greater than 1).
#' @param path_arrow arrow, path arrow specification, as created by grid::arrow().
#' @param path_colours character, a vector of colours. Must be of same length as number of individual tracks in \code{m} and refers to the order of tracks in \code{m}. If undefined (\code{NA}) and \code{m} contains a column named \code{colour}, colours provided within \code{m} are used (see details). Othwersie, colours are selected randomly per individual track.
#' @param path_legend logical, wether to add a path legend from \code{m} or not. Legend tracks and colours will be ordered by the tracks' temporal apperances, not by their order in \code{m}.
#' @param path_legend_title character, path legend title. Default is \code{"Names"}.
#' @param margin_factor numeric, factor relative to the extent of \code{m} by which the frame extent should be increased around the movement area. Ignored, if \code{ext} is set.
#' @param ext \code{sf bbox} or \code{sp extent} in same CRS as \code{m}, optional. If set, frames are cropped to this extent. If not set, a squared extent around \code{m}, optional with a margin set by \code{margin_factor}, is used (default).
#' @param map_service character, either \code{"mapbox"} or \code{"osm"}. Default is \code{"osm"}.
#' @param map_type character, a map type, e.g. \code{"streets"}. For a full list of available map types, see \code{\link{get_maptypes}}.
#' @param map_res numeric, resolution of base map in range from 0 to 1.
#' @param map_token character, mapbox authentification token for mapbox basemaps. Register at \url{https://www.mapbox.com/} to get a mapbox token. Mapbox is free of charge after registration for up to 50.000 map requests per month. Ignored, if \code{map_service = "osm"}.
#' @param map_dir character, directory where downloaded basemap tiles can be stored. By default, a temporary directory is used. 
#' If you use moveVis often for the same area it is recommended to set this argument to a directory persistent throughout sessions (e.g. in your user folder), 
#' so that baesmap tiles that had been already downloaded by moveVis do not have to be requested again.
#' @param verbose logical, if \code{TRUE}, messages on the function's progress are displayed (default).
#' 
#' @details If argument \code{path_colours} is not defined (set to \code{NA}), path colours can be defined by adding a character column named \code{colour} to \code{m}, containing a colour code or name per row (e.g. \code{"red"}. This way, for example, column \code{colour} for all rows belonging to individual A can be set to \code{"green"}, while column \code{colour} for all rows belonging to individual B can be set to \code{"red"}.
#' Colours could also be arranged to change through time or by behavioral segments, geographic locations, age, environmental or health parameters etc. If a column name \code{colour} in \code{m} is missing, colours will be selected automatically. Call \code{colours()} to see all available colours in R.
#' 
#' Basemap colour scales can be changed/added using \code{\link{add_colourscale}} or by using \code{ggplot2} commands (see \code{examples}). For continous scales, use \code{r_type = "gradient"}. For discrete scales, use \code{r_type = "discrete"}.
#'
#' @return List of ggplot2 objects, each representing a single frame.
#' 
#' @author Jakob Schwalb-Willmann
#' @seealso \code{\link{frames_graph}} \code{\link{animate_frames}}
#' 
#' @importFrom raster compareCRS nlayers
#' @importFrom move n.indiv
#' 
#' @export

frames_spatial <- function(m, r_list = NULL, r_times = NULL, r_type = "gradient", fade_raster = TRUE, map_service = "osm", map_type = "streets", map_res = 1, map_token = NULL, map_dir = paste0(tempdir(), "/moveVis/basemap"),
                          margin_factor = 1.1, ext = NULL, tail_length = 19, tail_size = 1, path_size = 3, path_end = "round", path_join = "round", path_mitre = 10, path_arrow = NULL, path_colours = NA, 
                          path_legend = TRUE, path_legend_title = "Names", verbose = TRUE, ...){
  
  ## check input arguments
  if(inherits(verbose, "logical")) options(moveVis.verbose = verbose)
  if(!isTRUE(dir.exists(map_dir))) dir.create(map_dir, recursive = T)
  if(!inherits(map_token, "character")) out("Argument 'map_token' must be of class 'character'.", type = 3)
  if(all(!c(inherits(m, "MoveStack"), inherits(m, "Move")))) out("Argument 'm' must be of class 'Move' or 'MoveStack'.", type = 3)
  
  ## check m time conformities
  .time_conform(m)

  if(!is.null(r_list)){
    if(all(!is.list(r_list), inherits(r_list, "Raster"))) r_list <- list(r_list)
    if(any(!sapply(r_list, compareCRS, y = m))) out("Projections of 'm' and 'r_list' differ.", type = 3)
    if(length(unique(sapply(r_list, nlayers))) > 1) out("Number of layers per raster object in list 'r' differ.", type = 3)
    if(!inherits(r_times, "POSIXct")) out("Argument 'r_times' must be of type 'POSIXct' if 'r_list' is defined.", type = 3)
    if(!isTRUE(r_type %in% c("gradient", "discrete", "RGB"))) out("Argument 'r_type' must eihter be 'gradient' or 'discrete'.", type = 3)
    if(!is.logical(fade_raster)) out("Argument 'fade_raster' has to be either TRUE or FALSE.", type = 3)
  } else{
    if(!isTRUE(map_service %in% c("mapbox", "osm"))) out("Argument 'map_service' must be 'mapbox' or 'osm'.")
    if(!isTRUE(map_type %in% get_maptypes(map_service))) out("The defined map type is not supported for the selected service. Use get_maptypes() to get all available map types.", type = 3)
    if(!is.numeric(map_res)) out("Argument 'map_res' must be 'numeric'.", type = 3)
    if(any(map_res < 0, map_res > 1)) out("Argument 'map_res' must be a value between 0 and 1.", type = 3)
    if(!is.character(map_token)) out("Argument 'map_token' must be defined to access a basemap, if 'r_list' is not defined.", type = 3)
    if(map_service == "mapbox") if(!is.character(map_dir)) out("Argument 'map_dir' must be of type 'character'.", type = 3)
    if(!dir.exists(map_dir)) out("The directory defined with 'map_dir' does not exists.", type = 3)
  }
  num.args <- c(margin_factor = margin_factor, tail_length = tail_length, tail_size = tail_size, path_size = path_size, path_mitre = path_mitre)
  catch <- sapply(1:length(num.args), function(i) if(!is.numeric(num.args[[i]])) out(paste0("Argument '", names(num.args)[[i]], "' must be of type 'numeric'."), type = 3))
  char.args <- c(path_end = path_end, path_join = path_join, path_legend_title = path_legend_title)
  catch <- sapply(1:length(char.args), function(i) if(!is.character(char.args[[i]])) out(paste0("Argument '", names(char.args)[[i]], "' must be of type 'numeric'."), type = 3))
  
  if(!is.null(ext)) if(!inherits(ext, "Extent")) out("Argument 'ext' must be of type 'Extent' (see raster::extent), if defined.", type = 3)
  if(!is.null(path_arrow)) if(!inherits(path_arrow, "arrow")) out("Argument 'path_arrow' must be of type 'arrrow' (see grid::arrow), if defined.", type = 3)
  if(is.character(path_colours)) if(length(path_colours) != n.indiv(m)) out("Argument 'path_colours' must be of same length as the number of individual tracks of 'm', if defined. Alternatively, use a column 'colour' for individual colouring per coordinate within 'm' (see details of ?frames_spatial).", type = 3)
  if(!is.logical(path_legend)) out("Argument 'path_legend' must be of type 'logical'.", type = 3)
  
  ## preprocess movement data
  out("Processing movement data...")
  m.df <- .m2df(m, path_colours = path_colours) # create data.frame from m with frame time and colour
  gg.ext <- .ext(m.df, ext, margin_factor) # calcualte square extent
  m.split <- .split(m.df, tail_length = tail_length, path_size = path_size, tail_size = tail_size) # split m by size of tail
  
  ## calculate tiles and get map imagery
  if(is.null(r_list)){
    out("Retrieving and compositing basemap imagery...")
    r_list <- .getMap(gg.ext, map_service, map_type, map_token, map_dir, map_res)
    r_type <- "RGB"
  }
  out("Assigning raster maps to frames...")
  r_list <- .rFrames(r_list, r_times, m.split, gg.ext, fade_raster)
  
  ## plot basemap
  if(length(r_list) == 1){
    if(r_type == "gradient") gg.bmap <- .lapply(r_list[[1]], ggR, ggObj = T, geom_raster = T)
    if(r_type == "discrete") gg.bmap <- .lapply(r_list[[1]], ggR, ggObj = T, geom_raster = T, forceCat = T)
  } else{ gg.bmap <- .lapply(1:length(r_list[[1]]), function(i) ggRGB(stack(lapply(r_list, "[[", i)),  r = 1, g = 2, b = 3, ggObj = T, geom_raster = T))}
  
  ## return frames
  out("Creating frames...")
  return(.gg_spatial(m.split = m.split, gg.bmap = gg.bmap, m.df = m.df, squared = if(is.null(ext)) T else F,
                     path_size = path_size, path_end = path_end, path_join = path_join, path_mitre = path_mitre, path_arrow = path_arrow,
                     print_plot = F, path_legend = path_legend, path_legend_title = path_legend_title))
}