#' Create movement animation frames from movement data
#'
#' \code{create_frames} creates a list of \code{ggplot} objects of which each represents a single frame. Each frame can be viewed or modified individually. The returned list of frames can be animated using \code{\link{animate_frames}}.
#'
#' @param m movement
#' @param token mapbox token
#'
#' @return List of ggplot objects, each representing a single frame.
#' 
#' @author Jakob Schwalb-Willmann
#' @seealso \code{\link{animate_frames}}
#' 
#' @importFrom sf st_bbox st_crs
#' @importFrom slippymath bb_to_tg tg_composite
#' @importFrom purrr pmap
#' @importFrom glue glue
#' @importFrom curl curl_download
#' @importFrom raster crop projectRaster crs extent
#' @importFrom RStoolbox ggRGB
#' 
#' @export
create_frames <- function(m, token, verbose = TRUE,  dir.bm = paste0(tempdir(), "/moveVis/basemap")){
  
  ## checks
  if(inherits(verbose, "logical")) options(moveVis.verbose = verbose)
  if(!isTRUE(dir.exists(dir.bm))) dir.create(dir.bm, recursive = T)
  
  ## calcualte square extent
  out("Calculating extent...")
  m.ext <- st_bbox(c(xmin = min(m$lon), xmax = max(m$lon), ymin = min(m$lat), ymax = max(m$lat)), crs = st_crs("+proj=longlat +ellps=WGS84"))
  gg.ext <- .squared(m.ext, margin_factor = 1.1)
  
  ## calculate tiles and get mapbox imagery
  out("Retrieving basemap imagery...")
  tg <- bb_to_tg(gg.ext, max_tiles = 20)
  
  images <- apply(tg$tiles, MARGIN = 1, function(x){
    file <- paste0(dir.bm, x[1], "_", x[2], ".jpg")
    if(!isTRUE(file.exists(file))){
      curl_download(url = paste0("https://api.mapbox.com/v4/mapbox.satellite/", tg$zoom, "/", x[1], "/", x[2], ".jpg90", "?access_token=", token))
    }
    return(file)
  })
  
  ## composite imagery
  out("Compositing basemap imagery...")
  r <- tg_composite(tg, images)
  r <- crop(projectRaster(r, crs = crs("+proj=longlat +ellps=WGS84")), extent(gg.ext[1], gg.ext[3], gg.ext[2], gg.ext[4]))
  
  ## split m by size of tail, requires m with col lon, lat, id and frame time (integer)
  out("Splitting movement data per frame...")
  m.split <- .split(m, n_tail = 19)
  
  ## return frames
  out("Creating frames...")
  return(.gg(m.split = m.split, ggbmap = ggRGB(r, r = 1, g = 2, b = 3, ggObj = T), print_plot = F))
}