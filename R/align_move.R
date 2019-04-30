#' Align movement data
#'
#' This function aligns movement data to a uniform time scale with a uniform temporal resolution throughout the complete movement sequence. 
#' This prepares the provided movement data to be interpretable by \code{\link{frames_spatial}}, which necessitates a uniform time scale and
#' a consistent, unique temporal resolution for all moving individuals to turn recording times into frame times.
#'
#' @param m \code{move} or \code{moveStack}, which is allowed to contain irregular timestamps and diverging temporal resolutions to be aligned (see \code{\link{df2move}} to convert a \code{data.frame} to a \code{move} object).
#' @param res either numeric, representing the temporal resolution, to which \code{m} should be aligned to (see argument \code{unit}), or character:
#' \itemize{
#'   \item \code{"min"} to use the smalles temporal resolution of \code{m} (default)
#'   \item \code{"max"} to use the largest temporal resolution of \code{m}
#'   \item \code{"mean"} to use the rounded average temporal resolution of \code{m}
#' }
#' @param digit either numeric, indicating to which digits of a specifc unit (see argument \code{unit}) the time scale of \code{m} should be aligned (e.g. 0 to align the time scale to second ":00", if \code{unit} is set to \code{secs}), or character:
#' \itemize{
#'   \item \code{"min"} to use the smallest digit of the defined \code{unit} (default)
#'   \item \code{"max"} to use the largest digit of the defined \code{unit}
#'   \item \code{"mean"} to use the rounded average digit of the defined \code{unit}
#' }
#' @param unit character, either \code{"secs"}, \code{"mins"}, \code{"hours"}, \code{"days"}, indicating the temporal unit, to which \code{res} and \code{digit} are referring.
#' @param spaceMethod character, either \code{"euclidean"}, \code{"greatcircle"} or \code{"rhumbline"}, indicating the interpolation function to be used to interpolate locations of \code{m} to the aligned time scale. Interpolation is performed using \code{move::interpolateTime}.
#'
#' @return Aligned \code{move} or \code{moveStack}, ready to be used with \code{\link{frames_spatial}}-
#' @author Jakob Schwalb-Willmann
#'
#' @importFrom move timestamps timeLag interpolateTime moveStack move split
#' @importFrom sp coordinates
#' @importFrom raster crs
#' @importFrom lubridate second<- minute<- hour<- day<- 
#'
#' @seealso \code{\link{df2move}} \code{\link{frames_spatial}} \code{\link{frames_graph}}
#' 
#' @examples
#' library(moveVis)
#' library(move)
#' data("move_data")
#' 
#' # the tracks in move_data have irregular timestamps and sampling rates.
#' # print unique timestamps and timeLag
#' unique(timestamps(move_data))
#' unique(unlist(timeLag(move_data, units = "secs")))
#' 
#' # use align_move to correct move_data to a uniform time scale and lag using interpolation.
#' # resolution of 4 minutes (240 seconds) at digit 0 (:00 seconds) per timestamp:
#' m <- align_move(move_data, res = 240, digit = 0, unit = "secs")
#' unique(unlist(timeLag(m, units = "secs")))
#' 
#' # resolution of 1 hour (3600 seconds) at digit 0 (:00 seconds) per timestamp:
#' m <- align_move(move_data, res = 3600, digit = 0, unit = "secs")
#' unique(unlist(timeLag(m, units = "secs")))
#' 
#' # resolution of 1 hour (15 seconds) at digit 0 (:00 seconds) per timestamp:
#' m <- align_move(move_data, res = 15, digit = 0, unit = "secs")
#' unique(unlist(timeLag(m, units = "secs")))
#' 
#' # resolution of 1 hour:
#' m <- align_move(move_data, res = 60, unit = "mins")
#' unique(unlist(timeLag(m, units = "secs")))
#' 
#' @export

align_move <- function(m, res = "min", digit = "min", unit = "secs", spaceMethod = "greatcircle"){
  
  ## check m and spaceMethod
  if(!inherits(m, c("Move", "MoveStack"))) out("Argument 'm' must be of class 'Move' or 'MoveStack'.", type = 3)
  m.length <- if(inherits(m, "MoveStack")) sapply(split(m), length) else length(m)
  if(any(m.length < 2)) out(paste0("Individual track(s) ", paste0(which(m.length < 2), collapse = ", "), " of 'm' consist(s) of less than 2 locations only. A minimum of 2 locations per indvidual track is required for alignment."), type = 3)
  
  ## check resolution and define resolution
  if(all(!c(inherits(res, "numeric"), inherits(res, "character")))) out("Argument 'res' must either be numeric or one of c('min', 'max', 'mean').", type = 3)
  if(res == "min") res <- min(unique(unlist(timeLag(m, unit))))
  if(res == "max") res <- max(unique(unlist(timeLag(m, unit))))
  if(res == "mean") res <- round(mean(unique(unlist(timeLag(m, unit)))))
  res <- as.difftime(res, units = unit)
  
  ## check time.digit
  ts <- timestamps(m)
  time.digits <- unique(as.numeric(format(ts, .convert_units(unit))))
  
  if(all(!c(inherits(digit, "numeric"), inherits(digit, "character")))) out("Argument 'digit' must either be numeric or one of c('min', 'max', 'mean').", type = 3)
  if(digit == "min") digit <- min(time.digits)
  if(digit == "max") digit <- max(time.digits)
  if(digit == "mean") digit <- round(mean(time.digits))
  
  ts.shoulder <- list(min(ts), max(ts))
  set.fun <- list("secs" = `second<-`, "mins" = `minute<-`, "hours" = `hour<-`, "days" = `day<-`)
  set.fun <- set.fun[[match(unit, names(set.fun))]]
  ts.shoulder <- lapply(ts.shoulder, set.fun, value = digit)
  ts.target <- seq.POSIXt(ts.shoulder[[1]], ts.shoulder[[2]], by = res)
  
  m.indi <- if(inherits(m, "MoveStack")) split(m) else list(m)
  m <- moveStack(lapply(m.indi, function(x){
    ts.m <- timestamps(x)
    ts.t <- ts.target[ts.target >= min(ts.m) & ts.target <= max(ts.m)]
    #tryCatch(interpolateTime(x, ts.t, spaceMethod), error = function(e) out("The selected interpolation method defined with argument 'spaceMethod' cannot be used with this projection. Please try a different interpolation method. See ?align_move for help.", type = 3))
    interpolateTime(x, ts.t, spaceMethod)
  }))
  
  m[,c("x", "y")] <- coordinates(m)
  m[,"time"] <- timestamps(m)
  return(m)
}