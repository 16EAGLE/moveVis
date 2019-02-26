#' Subset a \code{move} or \code{moveStack} object by a given time span
#'
#' This function is a simple wrapper that subsets a \code{move} or \code{moveStack} by a given time span. A \code{move} or \code{moveStack} containing data only for the subset time span is returned.
#'
#' @param m a \code{move} or \code{moveStack} object (see \code{\link{df2move}} to convert a \code{data.frame} to a \code{move} object).
#' @param from character or \code{POSIXct}, representing the start time. If character, the format \code{"\%m-\%d-\%y \%H:\%M:\%S"} must be used (see \code{\link{strptime}}).
#' @param to character or \code{POSIXct}, representing the stop time. If character, the format  \code{"\%m-\%d-\%y \%H:\%M:\%S"} must be used  (see \code{\link{strptime}}).
#' @param tz character, time zone that should be used if \code{from} and/or \code{to} are of type \code{character}.
#'
#' @return A \code{move} or \code{moveStack} object.
#' 
#' @seealso \code{\link{df2move}}
#' 
#' @importFrom move moveStack move
#' @importFrom methods as
#' 
#' @examples
#' library(moveVis)
#' library(move)
#' 
#' # load the example data
#' data("move_data")
#' 
#' # check min and max of move_data timestamps
#' min(timestamps(move_data))
#' max(timestamps(move_data))
#' 
#' # subset by character times
#' m <- subset_move(move_data, from = "2018-05-15 07:00:00", to = "2018-05-15 18:00:00")
#' 
#' # check min and max of result
#' min(timestamps(m))
#' max(timestamps(m))
#' 
#' @export

subset_move <- function(m, from, to, tz = "UTC"){
  
  # checks
  if(!inherits(from, "POSIXct")) from <- as.POSIXct(from, tz)
  if(!inherits(to, "POSIXct")) to <- as.POSIXct(to, tz)
  
  # subset by time
  ts <- timestamps(m)
  if(from < min(ts)) out("Argument 'from' cannot be smaller than the minimum timestamp of m.")
  if(to > max(ts)) out("Argument 'to' cannot be greater than the maximum timestamp of m.")
  
  ts.order <- order(ts)
  m.df <- as(m, "data.frame")
  m.df <- m.df[ts.order[ts[ts.order] >= from & ts[ts.order] <= to],]
  
  df2move(m.df, proj = crs(m), x = "coords.x1", y = "coords.x2", time = "timestamps", data = m.df, 
          track_id = if(!is.null(m.df$trackId)) "trackId" else NULL)
}