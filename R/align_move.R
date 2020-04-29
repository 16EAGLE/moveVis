#' Align movement data
#'
#' This function aligns movement data to a uniform time scale with a uniform temporal resolution throughout the complete movement sequence. 
#' This prepares the provided movement data to be interpretable by \code{\link{frames_spatial}}, which necessitates a uniform time scale and
#' a consistent, unique temporal resolution for all moving individuals to turn recording times into frame times.
#' 
#' @inheritParams frames_spatial
#' @param m \code{move} or \code{moveStack}, which is allowed to contain irregular timestamps and diverging temporal resolutions to be aligned (see \code{\link{df2move}} to convert a \code{data.frame} to a \code{move} object).
#' @param res either numeric, representing the temporal resolution, to which \code{m} should be aligned to (see argument \code{unit}), or character:
#' \itemize{
#'   \item \code{"minimum"} to use the smallest temporal resolution of \code{m} (default)
#'   \item \code{"maximum"} to use the largest temporal resolution of \code{m}
#'   \item \code{"mean"} to use the rounded average temporal resolution of \code{m}
#' }
#' @param unit character, temporal unit of \code{res}. Either \code{"secs"}, \code{"mins"}, \code{"hours"}, \code{"days"}. No effect, if \code{res} is not defined.
#' @param spaceMethod character, either \code{"euclidean"}, \code{"greatcircle"} or \code{"rhumbline"}, indicating the interpolation function to be used to interpolate locations of \code{m} to the aligned time scale. Interpolation is performed using \code{move::interpolateTime}.
#' @param ... deprecated arguments including \code{digit}.
#'
#' @return Aligned \code{move} or \code{moveStack}, ready to be used with \code{\link{frames_spatial}}-
#' @author Jakob Schwalb-Willmann
#'
#' @importFrom move timestamps timeLag interpolateTime moveStack move split namesIndiv
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
#' # resolution of 4 minutes:
#' m <- align_move(m = move_data, res = 4, unit = "mins")
#' unique(unlist(timeLag(m, units = "mins")))
#' 
#' # resolution of 1 hour:
#' m <- align_move(move_data, res = 1, unit = "hours")
#' unique(unlist(timeLag(m, units = "hours")))
#' 
#' # resolution of 15 seconds:
#' m <- align_move(move_data, res = 15, unit = "secs")
#' unique(unlist(timeLag(m, units = "secs")))
#' 
#' @export

align_move <- function(m, res = "minimum", unit = NA, spaceMethod = "greatcircle", ..., verbose = TRUE){
  
  # deprecated arguments
  extras <- list(...)
  if("digit" %in% names(extras)) out("Argument 'digit' is deprecated. See ?align_move for details.", type = 2)
  
  ## check m and spaceMethod
  if(inherits(verbose, "logical")) options(moveVis.verbose = verbose)
  if(!inherits(m, c("Move", "MoveStack"))) out("Argument 'm' must be of class 'Move' or 'MoveStack'.", type = 3)
  m.length <- if(inherits(m, "MoveStack")) sapply(split(m), length) else length(m)
  if(any(m.length < 2)) out(paste0("Individual track(s) ", paste0(which(m.length < 2), collapse = ", "), " of 'm' consist(s) of less than 2 locations only. A minimum of 2 locations per indvidual track is required for alignment."), type = 3)
  
  ## check resolution and define resolution
  if(is.na(unit)) unit_ <- "secs" else unit_ <- unit
  if(all(!c(inherits(res, "numeric"), inherits(res, "character")))) out("Argument 'res' must either be numeric or one of c('min', 'max', 'mean').", type = 3)
  if(any(res == "min", res == "minimum")) res <- min(unique(unlist(timeLag(m, unit_))))
  if(any(res == "max", res == "maximum")) res <- max(unique(unlist(timeLag(m, unit_))))
  if(res == "mean") res <- round(mean(unique(unlist(timeLag(m, unit_)))))
  res <- as.difftime(res, units = unit_)
  
  # recalc unit
  if(is.na(unit)){
    if(all(res >= 60, res < 3600)) unit <- "mins"
    if(all(res >= 3600, res < 86400)) unit <- "hours"
    if(res >= 86400) unit <- "days"
    res <- `units<-`(res, unit)
  }
  out(paste0("Temporal resolution of ",  round(as.numeric(res), digits = 2), " [", units(res), "] is used to align trajectories."))
  
  # calculate time shoulders and full target timestamps
  ts <- timestamps(m)
  ts.shoulder <- list(min(ts), max(ts))
  if(unit != "secs"){
    set.fun <- list("mins" = `second<-`, "hours" = `minute<-`, "days" = `hour<-`)
    set.fun <- set.fun[1:match(unit, names(set.fun))]
    ts.shoulder <- lapply(ts.shoulder, function(x){
      for(i in 1:length(set.fun)) x <- set.fun[[i]](x, value = 0)
      return(x)
    })
  }
  ts.target <- seq.POSIXt(ts.shoulder[[1]], ts.shoulder[[2]], by = res)
  
  # calculate new timestamps per trajectory
  m.indi <- if(inherits(m, "MoveStack")) split(m) else list(m)
  ts.m <- lapply(m.indi, timestamps)
  ts.t <- lapply(ts.m, function(x) ts.target[ts.target >= min(x) & ts.target <= max(x)])
  
  # check whether resolution fits data
  i.finer <- which(sapply(ts.t, function(x) length(x) == 0))
  if(length(i.finer) > 0){
    if(length(i.finer) == length(m.indi)){
      out("The temporal coverage of all trajectories of 'm' is shorter than the specified resolution. You may want to choose a finer resolution.", type = 3)
    } else{
      out(paste0("The full temporal coverage of at least one trajectory is shorter than the specified resolution. You may want to choose a finer resolution.\nTrajectories that are excluded: '", paste0(namesIndiv(m)[i.finer], collapse = "', '"), "'"), type = 2)
    }
    m.indi <- m.indi[-i.finer]
    ts.t <- ts.t[-i.finer]
  }
  
  # interpolate
  m <- moveStack(mapply(x = m.indi, y = ts.t, function(x, y) interpolateTime(x, y, spaceMethod)))
  m[,c("x", "y")] <- m@coords
  m[,"time"] <- timestamps(m)
  return(m)
}