#' Create movement animation frames from movement data
#'
#' \code{create_frames} creates a list of \code{ggplot} objects of which each represents a single frame. Each frame can be viewed or modified individually. The returned list of frames can be animated using \code{\link{animate_frames}}.
#'
#' @param m \code{move} or \code{moveStack} of uniform time scale and time lag, e.g. prepared with \code{\link{align_move}} (recommended). May contain a column named \code{colour} to control path colours (see \code{details}).
#' @param r_list list of \code{raster} or \code{rasterStack}. Each list element referrs to the times given in \code{r_times}. Use single-layer \code{raster} objects for gradient or discrete data (see \code{r_type}). Use a  \code{rasterStack} containing three bands for RGB imagery (in the order red, green, blue).
#' @param r_times list of \code{POSIXct} times. Each list element represents the time of the corresponding element in \code{r_list}. Must be of same length as \code{r_list}.
#' @param r_type character, either \code{"gardient"} or \code{"discrete"}. Ignored, if \code{r_list} contains \code{rasterStacks} of three bands, which are treated as RGB.
#' @param tail_length numeric, length of tail per movement path.
#' @param path_size numeric, size of each path. Default is 3.
#' @param tail_size numeric, size of the last tail element. Default is 1.
#' @param path_end character, either \code{"round"}, \code{"butt"} or \code{"square"}, indicating the path end style.
#' @param path_join character, either \code{"round"}, \code{"mitre"} or \code{"bevel"}, indicating the path join style.
#' @param path_mitre numeric, path mitre limit (number greater than 1).
#' @param path_arrow arrow, path arrow specification, as created by grid::arrow().
#' @param margin_factor numeric, factor relative to the extent of \code{m} by which the frame extent should be increased around the movement area. Ignored, if \code{ext} is set.
#' @param ext \code{sf bbox} or \code{sp extent} in same CRS as \code{m}, optional. If set, frames are cropped to this extent. If not set, a squared extent around \code{m}, optional with a margin set by \code{margin_factor}, is used (default).
#' @param map_service character, either \code{"mapbox"}, \code{"osm"} or \code{"bing"}.
#' @param map_type character, either \code{"satellite"}, ...
#' @param map_token character, mapbox map_token for mapbox basemaps
#' @param map_dir character, directory where downloaded basemap tiles can be stored. By default, a temporary directory is used. 
#' If you use moveVis often for the same area it is recommended to set this argument to a directory persistent throughout sessions (e.g. in your user folder), 
#' so that baesmap tiles that had been already downloaded by moveVis do not have to be requested again.
#' @param verbose logical, if \code{TRUE}, messages on the function's progress are displayed (default).
#' 
#' @details Path colours can be defined by adding a character column named \code{colour} to \code{m}, containing a colour code or name per row (e.g. \code{"red"}. This way, for example, column \code{colour} for all rows belonging to individual A can be set to \code{"green"}, while column \code{colour} for all rows belonging to individual B can be set to \code{"red"}.
#' Colours could also be arranged to change through time or by behavioral segments, geographic locations, age, environmental or health parameters etc. If a column name \code{colour} in \code{m} is missing, colours will be selected automatically. Call \code{colours()} to see all available colours in R.
#' 
#' Basemap colour scales can be changed/added using \code{\link{add_colourscale}} or by using \code{ggplot2} commands (see \code{examples}). For continous scales, use \code{r_type = "gradient"}. For discrete scales, use \code{r_type = "discrete"}.
#'
#' @return List of ggplot objects, each representing a single frame.
#' 
#' @author Jakob Schwalb-Willmann
#' @seealso \code{\link{animate_frames}}
#' 
#' @importFrom sf st_bbox st_crs st_intersects st_as_sfc
#' @importFrom raster compareCRS nlayers
#' @importFrom sp proj4string coordinates
#' @importFrom plyr mapvalues
#' @importFrom move n.indiv timestamps trackId
#' 
#' @export

create_frames <- function(m, r_list = NULL, r_times = NULL, r_type = "gradient", fade_raster = TRUE, map_service = "mapbox", map_type = "satellite", map_token = NULL, map_dir = paste0(tempdir(), "/moveVis/basemap"),
                          margin_factor = 1.1, ext = NULL, tail_length = 19, path_size = 3, tail_size = 1, path_end = "round", path_join = "round", path_mitre = 10, path_arrow = NULL, verbose = TRUE, ...){
  
  ## checks
  if(inherits(verbose, "logical")) options(moveVis.verbose = verbose)
  if(!isTRUE(dir.exists(map_dir))) dir.create(map_dir, recursive = T)
  if(!inherits(map_token, "character")) out("Argument 'map_token' must be of class 'character'.", type = 3)
  if(all(!c(inherits(m, "MoveStack"), inherits(m, "Move")))) out("Argument 'm' must be of class 'Move' or 'MoveStack'.", type = 3)
  
  if(length(unique(unlist(timeLag(m, "secs")))) > 1) out("The temporal resolution of 'm' is diverging. Use align_move() to align movement data to a uniform time scale with a consistent temporal resolution.", type = 3)
  if(any(.time_gaps(m))) out("Times of 'm' contain time gaps. Use align_move() to align movement data to a uniform time scale with a consistent temporal resolution.", type = 3)
  
  if(!is.null(r_list)){
    if(all(!is.list(r_list), inherits(r_list, "Raster"))) r_list <- list(r_list)
    if(any(!sapply(r_list, compareCRS, y = m))) out("Projections of 'm' and 'r_list' differ.", type = 3)
    if(length(unique(sapply(r_list, nlayers))) > 1) out("Number of layers per raster object in list 'r' differ.", type = 3)
  }
  
  
  ## create data.frame from m with frame time
  out("Processing movement data...")
  m.df <- cbind(as.data.frame(coordinates(m)), id = as.numeric(mapvalues(as.character(trackId(m)), unique(as.character(trackId(m))), 1:n.indiv(m))),
                time = timestamps(m), time_chr = as.character(timestamps(m)), name = as.character(trackId(m)))
  colnames(m.df)[1:2] <- c("x", "y")
  m.df$frame <- as.numeric(mapvalues(m.df$time_chr, unique(m.df$time_chr), 1:length(unique(m.df$time_chr))))
  
  ## handle colours, either provided as a field in m or computed, if not
  m.info <- as.data.frame(m)
  if(!is.null(m.info$colour)){
    m.df$colour <- m.info$colour
  }else{
    def.colours <- c("red", "green", "blue", "yellow", "darkgreen", "orange", "deepskyblue", "darkorange", "deeppink", "navy")
    def.colours <- c(def.colours, sample(colours()[-sapply(def.colours, match, table = colours())]))
    m.df$colour <- mapvalues(m.df$id, unique(m.df$id), def.colours[1:n.indiv(m)])
  }
  
  ## calcualte square extent
  m.ext <- st_bbox(c(xmin = min(m.df$x), xmax = max(m.df$x), ymin = min(m.df$y), ymax = max(m.df$y)), crs = st_crs(proj4string(m)))
  if(!is.null(ext)){
    if(inherits(ext, "Extent")) gg.ext <- st_bbox(c(xmin = ext@xmin, xmax = ext@xmax, ymin = ext@ymin, ymax = ext@ymax), crs = st_crs(proj4string(m)))
    if(!quiet(st_intersects(st_as_sfc(gg.ext), st_as_sfc(m.ext), sparse = F)[1,1])) out("Argument 'ext' does not overlap with the extent of 'm'.", type = 3)
  }else gg.ext <- .squared(m.ext, margin_factor = margin_factor)
  
  ## split m by size of tail, requires m with col x, y, id and frame time (integer)
  m.split <- .split(m.df, tail_length = tail_length, path_size = path_size, tail_size = tail_size)
  
  ## calculate tiles and get map imagery
  if(is.null(r_list)){
    out("Retrieving and compositing basemap imagery...")
    r_list <- .getMap(gg.ext, map_service, map_type, map_token, map_dir)
    r_type <- "RGB"
  }
  out("Assigning raster maps to frames...")
  gg.bmap <- .ggFrames(r_list, r_times, r_type, m.split, gg.ext, fade_raster)
  
  ## return frames
  out("Creating frames...")
  return(.gg(m.split = m.split, gg.bmap = gg.bmap, squared = if(is.null(ext)) T else F,
             path_size = path_size, path_end = path_end, path_join = path_join, path_mitre = path_mitre, path_arrow = path_arrow,
             print_plot = F))
}