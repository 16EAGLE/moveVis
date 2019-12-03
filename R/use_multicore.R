#' Enable multi-core usage
#'
#' This function enables multi-core usage of \code{moveVis} by setting the maximum number of cores to be used.
#' 
#' @inheritParams frames_spatial
#' @param n_cores numeric, optional, number of cores to be used. If not defined, the number of cores will be detected automatically (\code{n-1} cores will be used with \code{n} being the number of cores available).
#' 
#' @return None. This function is used for its side effects.
#' 
#' @details For most tasks, \code{moveVis} is able to use multiple cores to increase computational times through parallelization. By default, multi-core usage is disabled. This function saves the number of cores that \code{moveVis} should use to the global option \code{"moveVis.n_cores"} that can be printed using \code{getOption("moveVis.n_cores")}.
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
  out(paste0("Number of cores set to be used by moveVis: ", as.character(getOption("moveVis.n_cores")), "/", as.character(cores)))
}
