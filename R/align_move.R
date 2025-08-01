#' Align movement data
#'
#' This function aligns movement data to a uniform time scale with a uniform temporal resolution throughout the complete movement sequence. 
#' This prepares the provided movement data to be usable by \code{\link{frames_spatial}}, which requires a uniform time scale and
#' a consistent, unique temporal resolution for all supplied trajectories.
#' 
#' @inheritParams frames_spatial
#' @param m \code{move2} object, which is allowed to contain irregular timestamps and diverging temporal resolutions.
#' @param res either  a \code{units} object representing the temporal resolution \code{m} should be aligned to, or a character being one of 'min', 'max', 'mean' or 'median' to indicate how the target resolution should be derived from \code{m}.
#' @param start_end_time \code{NULL} (default) or a vector of two POSIXct times (one start time and one end time for alignment). If \code{NULL}, the start and end time are retrieved from \code{m} and used for alignment.
#' \itemize{
#'   \item \code{"minimum"} to use the smallest temporal resolution of \code{m} (default)
#'   \item \code{"maximum"} to use the largest temporal resolution of \code{m}
#'   \item \code{"mean"} to use the rounded average temporal resolution of \code{m}
#'   \item \code{"median"} to use the rounded median temporal resolution of \code{m}
#' }
#' @param ... deprecated arguments, including \code{digit}, \code{unit} and \code{spaceMethod}.
#'
#' @return \code{move2} object, with aligned positions at uniform temporal scale computed from \code{m}, ready to be used by \code{\link{frames_spatial}}.
#' @author Jakob Schwalb-Willmann
#'
#' @seealso \code{\link{frames_spatial}} \code{\link{frames_graph}}
#' 
#' @examples
#' library(moveVis)
#' library(move2)
#' library(lubridate)
#'
#' # example data
#' data("move_data")
#'
#' # the tracks in move_data have irregular timestamps and sampling rates.
#' # print unique timestamps and time lags
#' unique(mt_time(move_data))
#' unique(mt_time_lags(move_data, units = "s"))
#'
#' # use align_move to interpolate move_data to a uniform time scale and lag.
#' # e.g. setting a resolution of 4 minutes:
#' m <- align_move(m = move_data, res = units::set_units(4, "min"))
#' # check the result: records with attribute interpolate == TRUE have been added
#' # all trajectories are now aligend to a uniform 4-minute resolution:
#' unique(mt_time_lags(m, units = "min"))
#'
#' # same with resolution of 1 hour:
#' m <- align_move(move_data, res = units::set_units(1, "hour"))
#' unique(mt_time_lags(m, units = "hour"))
#'
#' # resolution of 15 seconds:
#' m <- align_move(move_data, res = units::set_units(15, "sec"))
#' unique(mt_time_lags(m, units = "sec"))
#'
#' # you can set the start/end times if needed:
#' # first, let us retrieve the start and end times
#' start_end_time <- range(mt_time(m))
#' start_end_time
#'
#' # I want the start time to be at 00 minutes and 00 seconds for the first track:
#' start_end_time <- round.POSIXt(start_end_time, units = "hours")
#'
#' m <- align_move(
#'   move_data, res = units::set_units(4, "min"), 
#'   start_end_time = start_end_time
#' )
#' mt_time(m)
#' @importFrom move2 mt_track_id mt_track_id<- mt_track_id mt_n_tracks mt_time_lags mt_time mt_track_id_column mt_time_column mt_as_move2 mt_set_track_data mt_track_data
#' @importFrom sf st_coordinates st_sf st_as_sf st_sfc st_linestring st_crs st_geometry st_line_interpolate st_is_longlat sf_use_s2
#' @importFrom s2 s2_interpolate_normalized
#' @importFrom units as_units ud_are_convertible deparse_unit set_units
#' @importFrom stats median
#' 
#' @export

align_move <- function(m, res = "minimum", start_end_time = NULL, ..., verbose = TRUE){
  if(inherits(verbose, "logical")) options(moveVis.verbose = verbose)
  
  extras <- list(...)
  if(!is.null(extras$digit)) out("Argument 'digit' is deprecated. See ?moveVis::align_move for details.", type = 2)
  if(!is.null(extras$unit)) out("Argument 'unit' is deprecated. See ?moveVis::align_move for details.", type = 2)
  if(!is.null(extras$spaceMethod)) out("Argument 'spaceMethod' is deprecated. See ?moveVis::align_move for details.", type = 2)
  
  # check inputs
  .check_move2(m)
  m_tracks <- split(m, mt_track_id(m))
  m_length <- if(mt_n_tracks(m) > 1) sapply(split(m, mt_track_id(m)), nrow) else nrow(m)
  if(any(m_length < 2)) out(paste0("Individual track(s) ", paste0(which(m_length < 2), collapse = ", "), " of 'm' consist(s) of less than 2 locations. A minimum of 2 locations per indvidual track is required for alignment."), type = 3)
  
  # check resolution and define resolution
  if(all(!c(inherits(res, "units"), inherits(res, "character")))) out("Argument 'res' must either be a 'units' object or one of c('min', 'max', 'mean', 'median').", type = 3)
  if(inherits(res, "units")){
    time_unit <- as_units("s")
    is_time_unit <- ud_are_convertible(deparse_unit(res), deparse_unit(time_unit))
    if(!is_time_unit){
      out(paste0("If argument 'res' is a 'units' object, a time unit must be used (such as seconds, minutes or hours). The provided unit '", units::deparse_unit(res), " cannot be used."), type = 3)
    }
  } else{
    if(any(res == "min", res == "minimum")){
      res <- min(mt_time_lags(m), na.rm = T)
    } else if(any(res == "max", res == "maximum")){
      res <- max(mt_time_lags(m), na.rm = T)
    } else if(res == "mean"){
      res <- round(mean(mt_time_lags(m), na.rm = T))
    } else if(res == "median"){
      res <- round(median(mt_time_lags(m), na.rm = T))
    }
  }
  out(paste0("Temporal resolution of ",  round(as.numeric(res), digits = 2), " [", units(res), "] is used to align trajectories."))
  
  if(is.null(start_end_time)){
    start_end_time <- range(mt_time(m), na.rm = T)
  } else{
    if(!all(length(start_end_time) == 2, inherits(start_end_time, "POSIXt"))) out("Argument 'start_end_time' must consist of two POSIXct times (one start time and one end time for alignment), if defined.", type = 3)
  }
  
  # move2 to points per track
  m$interpolated <- FALSE
  m_sf_points <- lapply(m_tracks, function(.m){
    class(.m) <- setdiff(class(.m), "move2")
    return(.m)
  })
  
  # points per track to single linestring per track (linear)
  coords <- lapply(m_sf_points, function(.m) st_coordinates(.m))
  m_sf_lines <- lapply(coords, function(x) st_sf(geometry = st_sfc(st_linestring(x), crs = st_crs(m))))
  
  # calculate target times for aligning, calculate normalized distance
  times_target <- seq.POSIXt(start_end_time[1], start_end_time[2], by = set_units(res, "s"))
  times_target <- lapply(m_tracks, function(x){
    ts <- mt_time(x)
    times_target[which.minpos(times_target - min(ts)):which.minpos(max(ts) - times_target)]
  })
  
  # check whether resolution fits data
  if(any(sapply(times_target, length) == 2)){
    out(paste0("Trajectory alignment using the chosen temporal resolution of ",  round(as.numeric(res), digits = 2), " [", units(res), "] results in only two positions for at least one track in 'm'. You may want to choose a finer resolution."), type = 2)
  }
  if(any(sapply(times_target, length) < 2)){
    out(paste0("The chosen temporal resolution of ",  round(as.numeric(res), digits = 2), " [", units(res), "] is to coarse for the provided data. You may want to choose a finer resolution."), type = 3)
  }
  
  # interpolate points on linestring by normalized distance (future: make use of ctmm?)
  nd <- lapply(1:length(m_tracks), function(i) as.numeric(difftime(times_target[[i]], min(mt_time(m_tracks[[i]])), units = "secs")) / as.numeric(diff(range(mt_time(m_tracks[[i]])), units = "s")))
  
  if(all(st_is_longlat(m), sf_use_s2())){
    m_aligned <- mapply(.m = m_sf_lines, .nd = nd, function(.m, .nd) st_as_sf(s2_interpolate_normalized(st_geometry(.m), .nd)), SIMPLIFY = T)
  } else{
    m_aligned <- mapply(.m = m_sf_lines, .nd = nd, function(.m, .nd) st_line_interpolate(st_geometry(.m), .nd), SIMPLIFY = T)
  }
  
  # assemble sf object
  m_aligned <- lapply(1:length(m_tracks), function(i) st_sf(
    interpolated = c(rep(FALSE, nrow(m_tracks[[i]])), rep(TRUE, length(m_aligned[[i]]))),
    track = names(m_tracks)[i],
    timestamp = c(mt_time(m_tracks[[i]]), times_target[[i]]),
    geometry = c(st_geometry(m_tracks[[i]]), m_aligned[[i]])
  ))
  m_aligned <- do.call(rbind, m_aligned)
  colnames(m_aligned) <- c("interpolated", mt_track_id_column(m), mt_time_column(m), attr(m, "sf_column"))
  
  # add attributes from user move2 object
  names_attr <- names(m)[!(names(m) %in% names(m_aligned))]
  df_attr <- data.frame(matrix(NA, nrow(m_aligned), length(names_attr)))
  colnames(df_attr) <- names_attr
  m_aligned <- cbind(m_aligned, df_attr)
  m_aligned <- m_aligned[,match(colnames(m), colnames(m_aligned))]
  
  # add aligned data to original data
  m_sf <- m
  class(m_sf) <- setdiff(class(m_sf), "move2")
  m_aligned <- rbind(m_sf, m_aligned)
  
  # coerce sf to move2
  m_aligned <- mt_as_move2(m_aligned, time_column = mt_time_column(m), track_id_column = mt_track_id_column(m))
  mt_track_id(m_aligned) <- as.factor(mt_track_id(m_aligned))
  m_aligned <- mt_set_track_data(m_aligned, mt_track_data(m))
  
  # reorder by time
  m_aligned <- m_aligned[order(m_aligned$timestamp),]
  m_aligned <- m_aligned[order(mt_track_id(m_aligned)),]
  
  # for now, we just return the aligned data
  m_aligned <- m_aligned[m_aligned$interpolated,]
  m_aligned$interpolated <- NULL
  
  return(m_aligned)
}