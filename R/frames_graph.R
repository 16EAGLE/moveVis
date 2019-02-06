#' Create frames of movement-environemnt interaction graphs for animation
#'
#' \code{frames_graph} creates a list of \code{ggplot2} objects of which each represents a single frame. Each frame can be viewed or modified individually. The returned list of frames can be animated using \code{\link{animate_frames}}.
#'
#' @inheritParams frames_spatial
#' @param return_data logical, if \code{TRUE}, instead of a list of frames, a \code{data.frame} containing the values extracted from \code{r_list} per individual, location and time is returned. This \code{data.frame} can be used to create your own multi- or monotemporal \code{ggplot2} movement-environemnt interaction graphs.
#' @param graph_type character, defines the type of multi-temporal graph that should be drawn as frames. Currently supported graphs are:
#' \itemize{
#'    \item \code{"flow"}, a time flow graph with frame time on the x axis and values of the visited cell at x on the y axis per individual track
#'    \item \code{"hist"}, a cumulative histogram with cell values on the x axis and time-cumulative counts of visits on the y axis per individual track.
#' }
#' 
#' @details To later on side-by-side join spatial frames created using \code{\link{frames_spatial}} with statistical frames created with \code{\link{create_stats}} for animation,
#' equal inputs must have been used for both function calls for each of the arguments \code{m}, \code{r_list}, \code{r_times} and \code{fade_raster}.
#'
#' @return List of ggplot2 objects, each representing a single frame. If \code{return_data} is \code{TRUE}, a \code{data.frame} is returned (see \code{return_data}).
#' 
#' @author Jakob Schwalb-Willmann
#' @seealso \code{\link{frames_spatial}} \code{\link{animate_frames}}
#' 
#' @importFrom raster compareCRS nlayers minValue maxValue
#' @importFrom move n.indiv
#' 
#' @export

frames_graph <- function(m, r_list, r_times, fade_raster = TRUE, return_data = FALSE, graph_type = "flow", path_size = 1.5, path_legend = TRUE, path_legend_title = "Names", 
                         hist_min = NULL, hist_max = NULL, hist_by = 0.1, verbose = T){

  ## check input arguments
  if(inherits(verbose, "logical")) options(moveVis.verbose = verbose)
  if(all(!c(inherits(m, "MoveStack"), inherits(m, "Move")))) out("Argument 'm' must be of class 'Move' or 'MoveStack'.", type = 3)
  
  if(all(!is.list(r_list), inherits(r_list, "Raster"))) r_list <- list(r_list)
  if(!inherits(r_list[[1]], "RasterLayer")) out("Argument 'r_list' must contain single-layer 'RasterLayer' objects. Multi-layer 'RasterStack' objects are not supported by this function.", type = 3)
  if(any(!sapply(r_list, compareCRS, y = m))) out("Projections of 'm' and 'r_list' differ.", type = 3)
  if(length(unique(sapply(r_list, nlayers))) > 1) out("Number of layers per raster object in list 'r' differ.", type = 3)
  if(!inherits(r_times, "POSIXct")) out("Argument 'r_times' must be of type 'POSIXct' if 'r_list' is defined.", type = 3)
  if(!is.logical(fade_raster)) out("Argument 'fade_raster' has to be either TRUE or FALSE.", type = 3)
  
  if(!is.numeric(path_size)) out("Argument 'path_size' must be of type 'numeric'.", type = 3)
  if(!is.logical(path_legend)) out("Argument 'path_legend' must be of type 'logical'.", type = 3)
  if(!is.character(path_legend_title)) out("Argument 'path_legend_title' must be of type 'character'.", type = 3)
  if(!is.logical(return_data)) out("Argument 'return_data' must be of type 'logical'.", type = 3)
  
  ## check graph_type and hist arguments!
  
  ## create data.frame from m with frame time and colour
  out("Processing movement data...")
  m.df <- .m2df(m) 
  m.split <- .split(m.df)
  
  ## create raster list
  r_list <- .rFrames(r_list, r_times, m.split, .ext(m.df), fade_raster = fade_raster)
  
  m.split <- mapply(x = r_list[[1]], y = m.split, function(x, y){
    y$value <- extract(x, y[c("x", "y")])
    return(y)
  }, SIMPLIFY = F)
  
  ## fuse framewise split to data.frame
  gg.df <- do.call(rbind, m.split)
  
  if(isTRUE(return_data)){
    return(gg.df)
  } else{
    
    ## create frames
    out("Creating frames...")
    if(graph_type == "flow"){
      return(.gg_flow(m.split, gg.df, path_legend, path_legend_title, path_size))
    }
    if(graph_type == "hist"){
      
      if(is.null(hist_min)) hist_min <- floor(min(sapply(r_list[[1]], minValue)))
      if(is.null(hist_max)) hist_max <- ceiling(max(sapply(r_list[[1]], maxValue)))
      hist_digits <- nchar(strsplit(as.character(hist_by), "[.]")[[1]][1])
      
      dummy <- cbind.data.frame(count = 0, value = seq(hist_min, hist_max, by = hist_by))
      
      ## Calculating time-cumulative value histogram per individual and timestep
      l.hist <- lapply(1:max(gg.df$frame), function(i, d = dummy){
        x <- gg.df[match(1:i, gg.df$frame),]
        do.call(rbind, lapply(unique(x$id), function(j){
          y <- x[x$id == j,]
          z <- table(round(y$value, digits = hist_digits))
          d[match(names(z), as.character(d$value)), 1] <- z
          d <- cbind(d, id = unique(y$id), name = unique(y$name), colour = unique(y$colour))
          return(d)
        }))
      })
      
      ## fusing histograms for plot scaling
      all.hist <- do.call(rbind, l.hist)
      return(.gg_hist(l.hist, all.hist, path_legend, path_legend_title, path_size))
    }
  }
}

