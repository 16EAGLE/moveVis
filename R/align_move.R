#' Align movement data
#'
#' This function aligns movement data to a uniform time scale with a uniform temporal resolution throughout the complete movement sequence. 
#' This prepares the provided movement data to be interpretable by \code{\link{create_frames}}, which necessitates a uniform time scale and
#' a consistent, unique temporal resolution for all moving individuals to turn recording times into frame times.
#'
#' @param m \code{move} or \code{moveStack}, which is allowed to contain irregular timestamps and diverging temporal resolutions to be aligned
#' @param res either numeric, representing the temporal resolution, to which \code{m} should be aligned to (see argument \code{unit}), or character:
#' \itemize{
#'   \item \code{"min"} to use the smalles temporal resolution of \code{m} (default)
#'   \item \code{"max"} to use the largest temporal resolution of \code{m}
#'   \item \code{"mean"} to use the rounded average temporal resolution of \code{m}
#' }
#' @param unit.digit either numeric, indicating to which digits of a specifc unit (see argument \code{unit}) the time scale of \code{m} should be aligned (e.g. 0 to align the time scale to second ":00", if \code{unit} is set to \code{secs}), or character:
#' \itemize{
#'   \item \code{"min"} to use the smallest digit of the defined \code{unit} (default)
#'   \item \code{"max"} to use the largest digit of the defined \code{unit}
#'   \item \code{"mean"} to use the rounded average digit of the defined \code{unit}
#' }
#' @param unit character, either \code{"secs"}, \code{"mins"}, \code{"hours"}, \code{"days"}, indicating the temporal unit, to which \code{res} and \code{unit.digit} are referring.
#' @param spaceMethod character, either \code{"euclidean"}, \code{"greatcircle"} or \code{"rhumbline"}, indicating the interpolation function to be used to interpolate locations of \code{m} to the aligned time scale. Interpolation is performed using \code{move::interpolateTime}.
#'
#' @return Aligned \code{move} or \code{moveStack}, ready to be used with \code{\link{create_frames}}-
#' @author Jakob Schwalb-Willmann
#'
#' @importFrom move timestamps timeLag interpolateTime moveStack move split
#' @importFrom lubridate second<- minute<- hour<- day<- 
#'
#' @seealso \link{create_frames}
#' @export

align_move <- function(m, res = "min", unit.digit = "min", unit = "secs", spaceMethod = "greatcircle"){
  
  ## check resolution and define resolution
  if(all(!c(inherits(res, "numeric"), inherits(res, "character")))) out("Argument 'res' must either be numeric or one of c('min', 'max', 'mean').", type = 3)
  if(res == "min") res <- min(unique(unlist(timeLag(m, unit))))
  if(res == "max") res <- max(unique(unlist(timeLag(m, unit))))
  if(res == "mean") res <- round(mean(unique(unlist(timeLag(m, unit)))))
  res <- as.difftime(res, units = unit)
  
  ## check time.digit
  ts <- timestamps(m)
  time.digits <- unique(as.numeric(format(ts, .convert_units(unit))))
  
  if(all(!c(inherits(unit.digit, "numeric"), inherits(unit.digit, "character")))) out("Argument 'unit.digit' must either be numeric or one of c('min', 'max', 'mean').", type = 3)
  if(unit.digit == "min") unit.digit <- min(time.digits)
  if(unit.digit == "max") unit.digit <- max(time.digits)
  if(unit.digit == "mean") unit.digit <- round(mean(time.digits))
  
  ts.shoulder <- list(min(ts), max(ts))
  set.fun <- list("secs" = `second<-`, "mins" = `minute<-`, "hours" = `hour<-`, "days" = `day<-`)
  set.fun <- set.fun[[match(unit, names(set.fun))]]
  ts.shoulder <- lapply(ts.shoulder, set.fun, value = unit.digit)
  ts.target <- seq.POSIXt(ts.shoulder[[1]], ts.shoulder[[2]], by = res)
  
  m.indi <- if(inherits(m, "MoveStack")) split(m) else list(m)
  moveStack(lapply(m.indi, function(x){
    ts.m <- timestamps(x)
    ts.t <- ts.target[ts.target >= min(ts.m) & ts.target <= max(ts.m)]
    interpolateTime(x, ts.t, spaceMethod)
  }))
}