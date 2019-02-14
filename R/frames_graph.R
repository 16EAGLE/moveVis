#' Create frames of movement-environment interaction graphs for animation
#'
#' \code{frames_graph} creates a list of \code{ggplot2} graphs displaying movement-environment interaction. Each object represents a single frame. Each frame can be viewed or modified individually. The returned list of frames can be animated using \code{\link{animate_frames}}.
#'
#' @inheritParams frames_spatial
#' @param return_data logical, if \code{TRUE}, instead of a list of frames, a \code{data.frame} containing the values extracted from \code{r_list} per individual, location and time is returned. This \code{data.frame} can be used to create your own multi- or monotemporal \code{ggplot2} movement-environemnt interaction graphs.
#' @param graph_type character, defines the type of multi-temporal graph that should be drawn as frames. Currently supported graphs are:
#' \itemize{
#'    \item \code{"flow"}, a time flow graph with frame time on the x axis and values of the visited cell at x on the y axis per individual track
#'    \item \code{"hist"}, a cumulative histogram with cell values on the x axis and time-cumulative counts of visits on the y axis per individual track.
#' }
#' @param val_min numeric, minimum value of the value axis. If undefined, the minimum is collected automatically.
#' @param val_max numeric, maximum value of the value axis. If undefined, the maximum is collected automatically.
#' @param val_by numeric, increment of the value axis sequence. Default is 0.1. If \code{graph_type = "discrete"}, this value should be an integer of 1 or greater.
#' 
#' @details To later on side-by-side join spatial frames created using \code{\link{frames_spatial}} with frames created with \code{\link{frames_graph}} for animation,
#' equal inputs must have been used for both function calls for each of the arguments \code{m}, \code{r_list}, \code{r_times} and \code{fade_raster}.
#'
#' @return List of ggplot2 objects, each representing a single frame. If \code{return_data} is \code{TRUE}, a \code{data.frame} is returned (see \code{return_data}).
#' 
#' @author Jakob Schwalb-Willmann
#' 
#' @importFrom raster compareCRS nlayers minValue maxValue extract
#' @importFrom sf st_crs 
#' @importFrom sp proj4string
#' @importFrom move n.indiv
#' 
#' @examples
#' library(moveVis)
#' library(move)
#' library(ggplot2)
#' 
#' data("move_data", "basemap_data")
#' # align movement
#' m <- align_move(move_data, res = 4, unit = "mins")
#' 
#' # create spatial frames with frames_spatial:
#' r_list <- basemap_data[[1]]
#' r_times <- basemap_data[[2]]
#' 
#' frames.sp <- frames_spatial(m, r_list = r_list, r_times = r_times, r_type = "gradient",
#'                             fade_raster = TRUE)
#' 
#' # use the same inputs to create a non-spatial graph, e.g. a flow graph:
#' frames.gr <- frames_graph(m, r_list = r_list, r_times = r_times, r_type = "gradient",
#'                           fade_raster = TRUE, graph_type = "flow")
#' # take a look
#' frames.gr[[100]]
#' 
#' # make a histogram graph:
#' frames.gr <- frames_graph(m, r_list = r_list, r_times = r_times, r_type = "gradient",
#'                           fade_raster = TRUE, graph_type = "hist")
#' # change the value interval:
#' frames.gr <- frames_graph(m, r_list = r_list, r_times = r_times, r_type = "gradient",
#'                           fade_raster = TRUE, graph_type = "hist", val_by = 0.01)
#' frames.gr[[100]]
#' # manipulate the labels, since now they are very dense:
#' # just replace the current scale
#' frames.gr <- add_gg(frames.gr, expr(scale_x_continuous(breaks=seq(0,1,0.1),
#'                                     labels=seq(0,1,0.1), expand = c(0,0))))
#' frames.gr[[100]]
#' 
#' # the same can be done for discrete data, histogram will then be shown as bin plots
#' 
#' # to make your own graphs, use frames_graph to return data instead of frames
#' data.gr <- frames_graph(m, r_list = r_list, r_times = r_times, r_type = "gradient",
#'                         fade_raster = TRUE, return_data = TRUE)
#' 
#' \dontrun{
#' # animate the frames created with frames_graph;
#' animate_frames(frames, out_file = tempfile(fileext = ".gif"))
#' }
#' 
#' # see all add_ functions on how to customize your frames created with frames_spatial
#' # or frames_graph
#' 
#' # see ?animate_frames on how to animate your list of frames
#' 
#' @seealso \code{\link{frames_spatial}} \link{join_frames} \code{\link{animate_frames}}
#' @export

frames_graph <- function(m, r_list, r_times, r_type = "gradient", fade_raster = FALSE, return_data = FALSE, graph_type = "flow", path_size = 1, path_legend = TRUE, path_legend_title = "Names", 
                         val_min = NULL, val_max = NULL, val_by = 0.1, verbose = T){

  ## check input arguments
  if(inherits(verbose, "logical")) options(moveVis.verbose = verbose)
  if(all(!c(inherits(m, "MoveStack"), inherits(m, "Move")))) out("Argument 'm' must be of class 'Move' or 'MoveStack'.", type = 3)
  
  ## check m time conformities
  .time_conform(m)
  
  if(all(!is.list(r_list), inherits(r_list, "Raster"))) r_list <- list(r_list)
  if(is.character(r_type)){
    if(!any(r_type == c("gradient", "discrete"))) out("Argument 'r_type' must be either 'gradient' or 'discrete'.", type = 3)
  } else{ out("Argument 'r_type' must be of type 'character'.", type = 3)}
  if(!inherits(r_list[[1]], "RasterLayer")) out("Argument 'r_list' must contain single-layer 'RasterLayer' objects. Multi-layer 'RasterStack' objects are not supported by this function.", type = 3)
  if(any(!sapply(r_list, compareCRS, y = m))) out("Projections of 'm' and 'r_list' differ.", type = 3)
  if(length(unique(sapply(r_list, nlayers))) > 1) out("Number of layers per raster object in list 'r' differ.", type = 3)
  if(!inherits(r_times, "POSIXct")) out("Argument 'r_times' must be of type 'POSIXct' if 'r_list' is defined.", type = 3)
  if(!is.logical(fade_raster)) out("Argument 'fade_raster' has to be either TRUE or FALSE.", type = 3)
  
  if(!is.numeric(path_size)) out("Argument 'path_size' must be of type 'numeric'.", type = 3)
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
  out("Processing movement data...")
  m.df <- .m2df(m) 
  m.split <- .split(m.df)
  
  ## create raster list
  r_list <- .rFrames(r_list, r_times, m.split, .ext(m.df, st_crs(proj4string(m))), fade_raster = fade_raster)
  
  m.split <- mapply(x = r_list[[1]], y = m.split, function(x, y){
    y$value <- extract(x, y[c("x", "y")])
    return(y)
  }, SIMPLIFY = F)
  
  ## fuse framewise split to data.frame
  gg.df <- do.call(rbind, m.split)
  
  ## create value sequence
  if(is.null(val_min)) val_min <- floor(min(sapply(r_list[[1]], minValue), na.rm = T))
  if(is.null(val_max)) val_max <- ceiling(max(sapply(r_list[[1]], maxValue), na.rm = T))
  val_digits <- nchar(strsplit(as.character(val_by), "[.]")[[1]][2])
  if(is.na(val_digits)) val_digits <- 0
  val_seq <- seq(val_min, val_max, by = val_by)
  
  if(isTRUE(return_data)){
    return(gg.df)
  } else{
    
    ## create frames
    out("Creating frames...")
    if(graph_type == "flow"){
      return(.gg_flow(m.split, gg.df, path_legend, path_legend_title, path_size, val_seq))
    }
    if(graph_type == "hist"){
      
      dummy <- do.call(rbind, lapply(unique(gg.df$id), function(id){
        dummy <- cbind.data.frame(count = 0, value = val_seq, id = id, name = unique(gg.df[gg.df$id == id,]$name),
                                  colour = unique(gg.df[gg.df$id == id,]$colour))
      }))
      
      ## Calculating time-cumulative value histogram per individual and timestep
      #out("Calculating histogram...")
      l.hist <- lapply(1:max(gg.df$frame), function(i, d = dummy){
        x <- gg.df[unlist(lapply(1:i, function(x) which(gg.df$frame == x))),]
        
        do.call(rbind, lapply(unique(x$id), function(id){
          y <- x[x$id == id,]
          z <- table(round(y$value, digits = val_digits))
          
          d.id <- d[d$id == id,]
          d.id[match(names(z), as.character(d.id$value)), 1] <- z
          d[d$id == id,] <- d.id
          
          #d <- cbind(d, id = unique(y$id), name = unique(y$name), colour = unique(y$colour))
          return(d)
        }))
      })
      
      ## fusing histograms for plot scaling
      all.hist <- do.call(rbind, l.hist)
      return(.gg_hist(l.hist, all.hist, path_legend, path_legend_title, path_size, val_seq, r_type))
    }
  }
}

