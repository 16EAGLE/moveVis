#' Suggest known file formats
#'
#' This function returns a selection of suggested file formats that can be used with \code{out_file} of \code{\link{animate_frames}} on your system.
#'
#' @param suggested character, a vector of suggested file formats which are checked to be known by the available renderers on the running system. By default, these are \code{c("gif", "mov", "mp4", "flv", "avi", "mpeg", "3gp", "ogg")}.
#' 
#' @importFrom av av_muxers
#'
#' @return A subset of \code{suggested}, containing only those file formats which are known by the renderers on the running system.
#' 
#' @seealso \code{\link{animate_frames}}
#' 
#' @export

suggest_formats <- function(suggested = c("gif", "mov", "mp4", "flv", "avi", "mpeg", "3gp", "ogg")){
  
  mux <- as.character(na.omit(av_muxers()$extensions))
  mux <- unlist(sapply(mux, function(x) unlist(strsplit(x, ",")), simplify = F, USE.NAMES = F))
  return(mux[as.numeric(na.omit(match(suggested, mux)))])
}