#' Create movement animation frames from movement data
#'
#' \code{create_frames} creates a list of \code{ggplot} objects of which each represents a single frame. Each frame can be viewed or modified individually. The returned list of frames can be animated using \code{\link{animate_frames}}.
#'
#' @param m \code{move} or \code{moveStack} of uniform time scale and time lag, e.g. prepared with \code{\link{align_move}} (recommended). May contain a column named \code{colour} to control path colours (see \code{details}).
#' @param tail_length numeric, length of tail per movement path.
#' @param path_size numeric, size of each path. Default is 3.
#' @param tail_size numeric, size of the last tail element. Default is 1.
#' @param path_end character, either \code{"round"}, \code{"butt"} or \code{"square"}, indicating the path end style.
#' @param path_join character, either \code{"round"}, \code{"mitre"} or \code{"bevel"}, indicating the path join style.
#' @param path_mitre numeric, path mitre limit (number greater than 1).
#' @param path_arrow arrow, path arrow specification, as created by grid::arrow().
#' @param margin_factor numeric, factor relative to the extent of \code{m} by which the frame extent should be increased around the movement area. Ignored, if \code{ext} is set.
#' @param ext \code{sf bbox} or \code{sp extent} in same CRS as \code{m}, optional. If set, frames are cropped to this extent. If not set, a squared extent around \code{m}, optional with a margin set by \code{margin_factor}, is used (default).
#' @param token character, mapbox token for mapbox basemaps
#' @param dir.bm character, directory where downloaded basemap tiles can be stored. By default, a temporary directory is used. 
#' If you use moveVis often for the same area it is recommended to set this argument to a directory persistent throughout sessions (e.g. in your user folder), 
#' so that baesmap tiles that had been already downloaded by moveVis do not have to be requested again.
#' @param verbose logical, if \code{TRUE}, messages on the function's progress are displayed (default).
#' 
#' @details Path colours can be defined by adding a character column named \code{colour} to \code{m}, containing a colour code or name per row (e.g. \code{"red"}. This way, for example, column \code{colour} for all rows belonging to individual A can be set to \code{"green"}, while column \code{colour} for all rows belonging to individual B can be set to \code{"red"}.
#' Colours could also be arranged to change through time or by behavioral segments, geographic locations, age, environmental or health parameters etc. If a column name \code{colour} in \code{m} is missing, colours will be selected automatically. Call \code{colours()} to see all available colours in R.
#'
#' @return List of ggplot objects, each representing a single frame.
#' 
#' @author Jakob Schwalb-Willmann
#' @seealso \code{\link{animate_frames}}
#' 
#' @importFrom sf st_bbox st_crs st_intersects st_as_sfc
#' @importFrom slippymath bb_to_tg tg_composite
#' @importFrom curl curl_download
#' @importFrom raster crop projectRaster crs extent compareCRS nlayers brick unstack setValues
#' @importFrom sp proj4string coordinates
#' @importFrom RStoolbox ggRGB
#' @importFrom plyr mapvalues
#' @importFrom move n.indiv timestamps trackId
#' 
#' @export

create_frames <- function(m, r_list = NULL, r_times = NULL, fade_raster = TRUE, tail_length = 19, path_size = 3, tail_size = 1, path_end = "round", path_join = "round", path_mitre = 10, path_arrow = NULL,
                          margin_factor = 1.1, ext = NULL, token = NULL, dir.bm = paste0(tempdir(), "/moveVis/basemap"), verbose = TRUE, ...){
  
  ## checks
  if(inherits(verbose, "logical")) options(moveVis.verbose = verbose)
  if(!isTRUE(dir.exists(dir.bm))) dir.create(dir.bm, recursive = T)
  if(!inherits(token, "character")) out("Argument 'token' must be of class 'character'.", type = 3)
  if(all(!c(inherits(m, "MoveStack"), inherits(m, "Move")))) out("Argument 'm' must be of class 'Move' or 'MoveStack'.", type = 3)
  
  if(length(unique(unlist(timeLag(m, "secs")))) > 1) out("The temporal resolution of 'm' is diverging. Use align_move() to align movement data to a uniform time scale with a consistent temporal resolution.", type = 3)
  if(any(.time_gaps(m))) out("Times of 'm' contain time gaps. Use align_move() to align movement data to a uniform time scale with a consistent temporal resolution.", type = 3)
  
  if(!is.null(r_list)){
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
  
  if(!is.null(r_list)){
    
    ## rearrange bandwise and crop
    r.nlay <- nlayers(r_list[[1]])
    if(r.nlay > 1) r_list <- lapply(r_list, unstack) else r_list <- list(r_list)
    r.crop <- lapply(r_list, function(r.lay) lapply(r.lay, crop, y = extent(gg.ext[1], gg.ext[3], gg.ext[2], gg.ext[4]), snap = "out"))
    
    ## calcualte time differences
    rt_diff <- lapply(r_times, function(x) sapply(sort(unique(m.df$time)), difftime, time2 = x))
    rt_frames <- sapply(rt_diff, which.min)
    
    ## create frame list
    r.dummy <- setValues(r.crop[[1]][[1]], NA)
    r.frames <- rep(list(rep(list(r.dummy), max(m.df$frame))), r.nlay)
    
    for(i in 1:r.nlay){
      r.frames[[i]][rt_frames] <- r.crop[[i]]
      r.frames[[i]] <- brick(r.frames[[i]])
    }
    
    if(isTRUE(fade_raster)){
      r.frames <- lapply(r.frames, function(x) approxNA(x, ...))
    } #else{
    
      #break_pos <- c(head(rt_frames, n=-1) + round(diff(rt_frames)/2))
      #break_pos <- cbind(c(rt_frames[1], break_pos), c(break_pos-1, max(m.df$frame)), 1:length(r_times))
      #r.frames <- lapply(r.frames)
      #apply(break_pos, MARGIN = 1, function(x) rep(x[3], diff(x[1:2])))
    #}
  }
  
  ## calculate tiles and get mapbox imagery
  out("Retrieving and compositing basemap imagery...")
  tg <- bb_to_tg(gg.ext, max_tiles = 20)
  
  images <- apply(tg$tiles, MARGIN = 1, function(x){
    file <- paste0(dir.bm, x[1], "_", x[2], ".jpg")
    if(!isTRUE(file.exists(file))){
      curl_download(url = paste0("https://api.mapbox.com/v4/mapbox.satellite/", tg$zoom, "/", x[1], "/", x[2], ".jpg90", "?access_token=", token))
    }
    return(file)
  })
  
  ## composite imagery
  r <- tg_composite(tg, images)
  r <- crop(projectRaster(r, crs = crs(m)), extent(gg.ext[1], gg.ext[3], gg.ext[2], gg.ext[4]))
  
  ## return frames
  out("Creating frames...")
  return(.gg(m.split = m.split, ggbmap = ggRGB(r, r = 1, g = 2, b = 3, ggObj = T), squared = if(is.null(ext)) T else F,
             path_size = path_size, path_end = path_end, path_join = path_join, path_mitre = path_mitre, path_arrow = path_arrow,
             print_plot = F))
}