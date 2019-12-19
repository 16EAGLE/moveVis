#' moveVis settings
#'
#' These functions control session-wide settings that can increase processing speeds.
#' 
#' \code{use_multicore} enables multi-core usage of \code{moveVis} by setting the maximum number of cores to be used. This can strongly increase the speed of creating frames.
#' 
#' \code{use_disk4frames} enables the usage of disk space for creating frames. This can prevent memory overload when creating frames for very large animations.
#' 
#' @inheritParams frames_spatial
#' @param n_cores numeric, optional, number of cores to be used. If not defined, the number of cores will be detected automatically (\code{n-1} cores will be used with \code{n} being the number of cores available).
#' @param use_disk logical, whether to use disk space for creating frames or not. If \code{TRUE}, frames will be written to \code{dir_frames}, clearing memory.
#' @param dir_frames character, directory where to save frame during frames creating.
#' @param n_memory_frames numeric, maximum number of frames allowed to be hold in memory. This number defines after how many frames memory should be cleared by writing frames in memory to disk.
#' 
#' @return None. These functions are used for their side effects.
#' 
#' @details For most tasks, \code{moveVis} is able to use multiple cores to increase computational times through parallelization. By default, multi-core usage is disabled. This function saves the number of cores that \code{moveVis} should use to the global option \code{"moveVis.n_cores"} that can be printed using \code{getOption("moveVis.n_cores")}.
#' 
#' How much memory is needed to create frames depends on the frame resolution (number of pixels) and the number of frames. Depending on how much memory is available it can make sense to allow disk usage and set a maximum number of frames to be hold in memory that won't fill up the available memory completely.
#' 
#' \code{moveVis} uses the \code{parallel} package for parallelization.
#' 
#' 
#' @examples 
#' # enable multi-core usage automatically
#' use_multicore()
#' 
#' # define number of cores manually
#' use_multicore(n_cores = 2)
#' 
#' # allow disk use with default directory
#' # and maxiumum of 50 frames in memory
#' use_disk4frames(use_disk = TRUE, n_memory_frames = 50)
#' 
#' @name settings
#' @export

use_multicore <- function(n_cores = NULL, verbose = TRUE){
  
  # checks
  if(inherits(verbose, "logical")) options(moveVis.verbose = verbose)
  
  # cores
  cores <- parallel::detectCores()
  if(is.null(n_cores)) n_cores <- cores-1
  if(n_cores < 1) n_cores <- 1
  if(n_cores > cores) n_cores <- cores
  
  # set
  options(moveVis.n_cores = n_cores)
  
  # inform
  out(paste0("Number of cores set to be used by moveVis: ", as.character(getOption("moveVis.n_cores")), " out of ", as.character(cores)))
}

#' @rdname settings
#' @export
use_disk4frames <- function(use_disk = TRUE, dir_frames = paste0(tempdir(), "/moveVis"), n_memory_frames = NULL, verbose = TRUE){
  
  # checks
  if(inherits(verbose, "logical")) options(moveVis.verbose = verbose)
  if(!is.null(n_memory_frames)) if(n_memory_frames < 3){
    out("Minimum number of frames in memory is 3.", type = 2)
    n_memory_frames <- 3
  }
  
  # options
  if(isTRUE(use_disk)){
    options(moveVis.use_disk = TRUE, moveVis.dir_frames = dir_frames, moveVis.n_memory_frames = n_memory_frames)
    out(paste0("Disk usage for creating frames enabled.\nDirectory: '", dir_frames, "'\nMaximum number of frames which will be hold in memory: ", if(is.null(n_memory_frames)) "auto." else as.character(n_memory_frames)))
  } else{
    options(moveVis.use_disk = FALSE, moveVis.dir_frames = NULL, moveVis.n_memory_frames = NULL)
    out(paste0("Disk usage for creating frames disabled.\nAll frames will be hold in memory."))
  }
}