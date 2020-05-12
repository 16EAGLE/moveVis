#' Convert a data.frame into a move or moveStack object
#'
#' This function is a simple wrapper that converts a \code{data.frame} into a \code{move} or \code{moveStack} object. Both can be used as inputs to \code{\link{frames_spatial}} or \code{\link{frames_graph}}.
#'
#' @param df data.frame, a data.frame with rows representing observations and columns representing x and y coordinates, time and optionally track IDs, if multiple tracks are contained.
#' @param proj projection, character (proj4string) or CRS object, indicating the projection that the coordinates of \code{df} represent.
#' @param x character, name of the column in \code{df} that represents x coordinates.
#' @param y character, name of the column in \code{df} that represents y coordinates.
#' @param time character, name of the column in \code{df} that represents timestamps. Timestamps need to be of class POSIXct.
#' @param track_id character, optional, name of the column in \code{df} that represents track names or IDs. If set, a \code{moveStack} is returned, otherwise, a \code{move} object is returned.
#' @param data data.frame, optional, to add additional data such as path colours (see \code{\link{move}}). Number of rows must equal number of rows of \code{df}.
#' @param ... additional arguments passed to \code{move}.
#'
#' @return A \code{move} or \code{moveStack} object.
#' 
#' @seealso \code{\link{frames_spatial}} \code{\link{frames_graph}} \code{\link{subset_move}}
#' 
#' @importFrom move move moveStack
#' 
#' @examples
#' library(moveVis)
#' library(move)
#' 
#' # load the example data and convert them into a data.frame
#' data("move_data")
#' move_df <- methods::as(move_data, "data.frame")
#' 
#' # use df2move to convert the data.frame into a moveStack
#' df2move(move_df,
#'         proj = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0", 
#'         x = "coords.x1", y = "coords.x2", time = "timestamps", track_id = "trackId")
#' @export

df2move <- function(df, proj, x, y, time, track_id = NULL, data = NULL, ...){
  
  # checks
  if(!inherits(df, "data.frame")) out("Argument 'df' must be of type 'data.frame'.", type = 3)
  df <- data.frame(df, check.names = F)
  df.names <- colnames(df)
  
  crs <- try(sf::st_crs(proj), silent = T)
  if(inherits(crs, "try-error")) out("Argument 'proj' seems not to represent a valid projection.", type = 3)
  
  catch <- sapply(c(x, y, time), function(x) if(!isTRUE(x %in% df.names)) out(paste0("Column named '", x, "' cannot be found in 'df'."), type = 3))
  if(!is.null(data)) if(nrow(data) != nrow(df)) out("Number of rows in 'data' must be equal to number of rows in 'df'.", type = 3)
  if(!inherits(df[,time], "POSIXct")) out("Time column must be of type POSIXct.", type = 3)
  
  if(!is.null(track_id)){
    
    # get ids per individual
    if(!isTRUE(track_id %in% df.names)) out(paste0("Column named '", track_id, "' cannot be found in 'df'."), type = 3)
    id.sub <- lapply(unique(df[, track_id]), function(x) which(df[, track_id] == x))
    df.split <- lapply(id.sub, function(x) df[x,])
    
    # make moveStack for multiple individuals
    m.split <- mapply(dfx = df.split, id = id.sub, function(dfx, id){
      dfx <- dfx[order(dfx[, time]),]
      if(is.null(data)){
        move(x = dfx[,x], y = dfx[,y], time = dfx[,time], proj = crs$proj4string, animal = dfx[, track_id], ...)
      } else{
        move(x = dfx[,x], y = dfx[,y], time = dfx[,time], proj = crs$proj4string, animal = dfx[, track_id], data = data[id,], ...)
      }
    })
    if(length(m.split) == 1) m.split[[1]] else moveStack(m.split)
  } else{
    
    # make move for one individual
    df <- df[order(df[, time]),]
    if(is.null(data)){
      move(x = df[,x], y = df[,y], time = df[,time], proj = crs$proj4string, ...)
    } else{
      move(x = df[,x], y = df[,y], time = df[,time], proj = crs$proj4string, data = data, ...)
    }
  }
}