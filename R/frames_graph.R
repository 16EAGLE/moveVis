#' Create frames of movement-environment interaction graphs for animation
#'
#' \code{frames_graph} creates a list of \code{ggplot2} graphs displaying movement-environment interaction. Each object represents a single frame. Each frame can be viewed or modified individually. The returned list of frames can be animated using \code{\link{animate_frames}}.
#'
#' @inheritParams frames_spatial
#' @param return_data logical, if \code{TRUE}, instead of a list of frames, a \code{data.frame} containing the values extracted from \code{r_list} per individual, location and time is returned. This \code{data.frame} can be used to create your own multi- or mono-temporal \code{ggplot2} movement-environemnt interaction graphs.
#' @param graph_type character, defines the type of multi-temporal graph that should be drawn as frames. Currently supported graphs are:
#' \itemize{
#'    \item \code{"flow"}, a time flow graph with frame time on the x axis and values of the visited cell at x on the y axis per individual track
#'    \item \code{"hist"}, a cumulative histogram with cell values on the x axis and time-cumulative counts of visits on the y axis per individual track.
#' }
#' @param val_min numeric, minimum value of the value axis. If undefined, the minimum is collected automatically.
#' @param val_max numeric, maximum value of the value axis. If undefined, the maximum is collected automatically.
#' @param val_by numeric, increment of the value axis sequence. Default is 0.1. If \code{graph_type = "discrete"}, this value should be an integer of 1 or greater.
#' @param ... additional arguments, currently unused.
#' 
#' @details To later on side-by-side join spatial frames created using \code{\link{frames_spatial}} with frames created with \code{\link{frames_graph}} for animation,
#' equal inputs must have been used for both function calls for each of the arguments \code{m}, \code{r_list}, \code{r_times} and \code{fade_raster}.
#'
#' If argument \code{path_colours} is not defined (set to \code{NA}), path colours can be defined by adding a character column named \code{colour} to \code{m}, containing a colour code or name per row (e.g. \code{"red"}. This way, for example, column \code{colour} for all rows belonging to individual A can be set to \code{"green"}, while column \code{colour} for all rows belonging to individual B can be set to \code{"red"}.
#' Colours could also be arranged to change through time or by behavioral segments, geographic locations, age, environmental or health parameters etc. If a column name \code{colour} in \code{m} is missing, colours will be selected automatically. Call \code{colours()} to see all available colours in R.
#'
#' @return An object of class \code{moveVis}. If \code{return_data} is \code{TRUE}, a \code{data.frame} is returned (see \code{return_data}).
#' 
#' @author Jakob Schwalb-Willmann
#'  
#' @examples
#' library(moveVis)
#' library(move2)
#' library(terra)
#' 
#' data("move_data", package = "moveVis")
#' r <- readRDS(example_data(file = "basemap_data.rds"))
#' 
#' # align movement
#' m <- align_move(move_data, res = units::set_units(4, "min"))
#' 
#' # create frames 
#' frames <- frames_graph(
#'   m, r, r_type = "gradient", fade_raster = TRUE, graph_type = "flow"
#' )
#' # take a look
#' frames[[100]]
#' 
#' # make a histogram graph:
#' frames <- frames_graph(
#'   m, r, r_type = "gradient", fade_raster = TRUE, graph_type = "hist"
#' )
#' frames[[100]]
#' 
#' # change the value interval:
#' frames <- frames_graph(
#'   m, r, r_type = "gradient", fade_raster = TRUE, graph_type = "hist", 
#'   val_by = 0.01
#' )
#' frames[[100]]
#'                           
#' # manipulate the labels, as they are quite dense by replacing the scale
#' frames <- add_gg(frames, rlang::expr(
#'   scale_x_continuous(breaks=seq(0,1,0.1), labels=seq(0,1,0.1), expand = c(0,0)))
#' )
#' frames[[100]]
#' 
#' # to make your own graphs, use frames_graph to return data instead of frames
#' graph_data <- frames_graph(
#'   m, r, r_type = "gradient", fade_raster = TRUE, return_data = TRUE
#' )
#' @seealso \code{\link{frames_spatial}} \code{\link{join_frames}} \code{\link{animate_frames}}
#'
#' @importFrom terra extract vect minmax
#' @importFrom sf st_crs 
#' @importFrom move2 mt_n_tracks
#' 
#' 
#' @export

frames_graph <- function(m, r, r_type = "gradient", fade_raster = FALSE, crop_raster = TRUE, return_data = FALSE, graph_type = "flow", path_size = 1, path_colours = NA, path_legend = TRUE, path_legend_title = "Names", 
                         val_min = NULL, val_max = NULL, val_by = 0.1, ..., verbose = T){

  ## check input arguments
  if(inherits(verbose, "logical")) options(moveVis.verbose = verbose)
  extras <- list(...)
  if(!inherits(m, "move2")) out("Argument 'm' must be of class 'move2'.", type = 3)
  
  ## check m time conformities
  out("Processing input data...")
  .time_conform(m)
  
  if(!is.null(extras$r_list)) out("Argument 'r_list' is deprecated. Use 'r' instead to supply raster objects.", type = 3)
  if(inherits(r, "SpatRaster")){
    r_list <- list(r)
    r_times <- list(time(r))
  }
  if(inherits(r, "SpatRasterDataset")){
    r_list <- lapply(r, function(x) x)
    r_times <- time(r)
  }
  if(!inherits(r, c("SpatRaster", "SpatRasterDataset"))) out("Argument 'r' must be a terra raster object of class SpatRaster or SpatRasterDataset.", type = 3)
  
  if(is.character(r_type)){
    if(!any(r_type == c("gradient", "discrete"))) out("Argument 'r_type' must be either 'gradient' or 'discrete'.", type = 3)
  } else{ out("Argument 'r_type' must be of type 'character'.", type = 3)}
  
  if(length(unique(sapply(r_list, nlyr))) > 1) out("Number of layers per raster object in 'r' differ.", type = 3)
  if(!all(sapply(r_times, inherits, "POSIXct"))) out("Times of r must be of class 'POSIXct' (see ?terra::time).", type = 3)
  
  if(nlyr(r_list[[1]]) != 1) out("frames_graph is expecting single-layer 'SpatRaster' objects per time steps. Multi-layer 'SpatRaster' objects are not supported by this function.", type = 3)
  if(length(unique(sapply(r_list, nlyr))) > 1) out("Number of layers per 'SpatRaster' object in differ.", type = 3)
  if(!all(sapply(r_times, inherits, "POSIXct"))) out("Raster times  must be of class 'POSIXct' (see ?terra::time).", type = 3)
  if(!is.logical(fade_raster)) out("Argument 'fade_raster' has to be either TRUE or FALSE.", type = 3)
  
  if(!is.numeric(path_size)) out("Argument 'path_size' must be of type 'numeric'.", type = 3)
  if(is.character(path_colours)) if(length(path_colours) != mt_n_tracks(m)) out("Argument 'path_colours' must be of same length as the number of individual tracks of 'm', if defined. Alternatively, use a column 'colour' for individual colouring per coordinate within 'm' (see details of ?frames_spatial).", type = 3)
  if(!is.logical(path_legend)) out("Argument 'path_legend' must be of type 'logical'.", type = 3)
  if(!is.character(path_legend_title)) out("Argument 'path_legend_title' must be of type 'character'.", type = 3)
  if(!is.logical(return_data)) out("Argument 'return_data' must be of type 'logical'.", type = 3)
  
  ## check graph_type and hist arguments
  if(!is.character(graph_type)){
    out("Argument 'graph_type' must be of type character.", type = 3)
  } else{
    if(!any(graph_type == c("flow", "hist"))) out("Argument 'graph_type' must be either 'flow' or 'hist'.", type = 3)
  }
  if(graph_type == "hist"){
    if(!is.null(val_min)) if(!is.numeric(val_min)) out("Argument 'val_min' must be of type 'numeric', if defined.", type = 3)
    if(!is.null(val_max)) if(!is.numeric(val_max)) out("Argument 'val_max' must be of type 'numeric', if defined.", type = 3)
    if(!is.numeric(val_by)) out("Argument 'val_by' must be of type 'numeric'.", type = 3)
  }
  
  ## warnings
  if(r_type == "discrete" & fade_raster == T) out("Argument 'fade_raster' is TRUE, while argument 'r_type' is set to 'discrete'. Interpolating discrete values will destroy discrete classes!", type = 2)
  if(r_type == "discrete" & !val_by%%1==0) out("Argument 'val_by' is fractional, while argument 'r_type' is set to 'discrete'. You may want to set 'val_by' to 1 or another integer for discrete classes.", type = 2)
  
  ## create data.frame from m with frame time and colour
  m <- .add_m_attributes(m, path_colours = path_colours)
  .stats(max(m$frame))
  
  ## create raster list
  out("Extracting raster values per frame...")
  r_list <- .rFrames(r_list = r_list, r_times = r_times, m =  m, gg.ext = .ext(m, st_crs(m)), fade_raster = fade_raster, crop_raster = crop_raster)
  m$value <- NA
  if(length(r_list) == 1){
    m$value <- terra::extract(r_list[[1]], terra::vect(m))[,2]
  } else{
    for(i in unique(m$frame)){
      m$value[m$frame == i] <- terra::extract(r_list[[i]], terra::vect(m[m$frame == i,]))[,2]
    }
  }
  
  ## create value sequence
  if(is.null(val_min)) val_min <- floor(min(sapply(r_list, function(x) minmax(x)[1,]), na.rm = T))
  if(is.null(val_max)) val_max <- ceiling(max(sapply(r_list, function(x) minmax(x)[2,]), na.rm = T))
  val_digits <- nchar(strsplit(as.character(val_by), "[.]")[[1]][2])
  if(is.na(val_digits)) val_digits <- 0
  val_seq <- seq(val_min, val_max, by = val_by)
  
  if(isTRUE(return_data)){
    return(m)
  } else{
    
    
    # if(graph_type == "flow"){
    #   #frames <- .gg_flow(m, path_legend, path_legend_title, path_size, val_seq)
    # }
    hist_data <- NULL
    if(graph_type == "hist"){
      
      dummy <- do.call(rbind, lapply(as.character(unique(mt_track_id(m))), function(name){
        cbind.data.frame(count = 0, value = val_seq, name = name,
                         colour = unique(m[m$name == name,]$colour))
      }))
      
      ## Calculating time-cumulative value histogram per individual and timestep
      #out("Calculating histogram...")
      hist_data <- lapply(1:max(m$frame), function(i, d = dummy){
        x <- m[unlist(lapply(1:i, function(x) which(m$frame == x))),]
        
        x <- do.call(rbind, lapply(unique(x$name), function(name){
          y <- x[x$name == name,]
          z <- table(round(y$value, digits = val_digits))
          
          d.name <- d[d$name == name,]
          d.name[match(names(z), as.character(d.name$value)), 1] <- z
          
          #d <- cbind(d, id = unique(y$id), name = unique(y$name), colour = unique(y$colour))
          return(d.name)
        }))

      })
      
      ## fusing histograms for plot scaling
      # all.hist <- do.call(rbind, hist_data)
      #frames <- .gg_hist(hist_data, all.hist, path_legend, path_legend_title, path_size, val_seq, r_type)
    }
  }
  
  # create frames object
  frames <- list(
    m = m,
    hist_data = hist_data,
    type = paste0("ggplot (", graph_type, " graph)"),
    graph_type = graph_type,
    aesthetics = list(
      path_size = path_size,
      path_legend = path_legend,
      path_legend_title = path_legend_title,
      val_seq = val_seq,
      r_type = r_type),
    additions = NULL
  )
  attr(frames, "class") <- c("moveVis", "frames_graph")
  
  return(frames)
}

