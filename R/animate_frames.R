#' Animate frames
#'
#' \code{animate_frames} creates an animation from a list of frames computed with \code{\link{frames_spatial}}.
#'
#' @inheritParams add_gg
#' @param out_file character, the output file path, e.g. "/dir/to/file.mov". The file extension must correspond to a file format known by the available renderers of the running system. Use \code{\link{suggest_formats}} to get a vector of suggested known file formats.
#' @param fps numeric, the number of frames to be displayed per second. Default is 2.
#' @param width numeric, width of the output animation in pixels.
#' @param height numeric, height of the output animation in pixels.
#' @param res numeric, resolution of the output animation in ppi.
#' @param display logical, whether the animation should be displayed after rendering or not.
#' @param overwrite logical, wether to overwrite an existing file, if \code{out_file} is already present.
#' @param ... additional arguments to be passed to the render function.
#'
#' @details An appropriate render function is selected depending on the file extension in \code{out_file}: For \code{.gif} files, \code{gifski::save_gif} is used, for any other (video) format, \code{av::av_capture_graphics} is used.
#'
#' @return None or an R graphics window displaying the animation
#' 
#' @importFrom av av_capture_graphics
#' @importFrom gifski save_gif
#' @importFrom ggplot2 quo
#' 
#' @importFrom utils tail
#' 
#' @author Jakob Schwalb-Willmann
#' @seealso \code{\link{frames_spatial}}
#' 
#' @export

animate_frames <- function(frames, out_file, fps = 25, width = 700, height = 700, res = 100, display = TRUE, overwrite = FALSE, verbose = TRUE, ...){
  
  if(inherits(verbose, "logical")) options(moveVis.verbose = verbose)
  if(!inherits(frames, "list")) out("Argument 'frames' needs to be a list of ggplot objects. See frames_spatial()).", type = 3)
  if(!all(sapply(frames, function(x) inherits(x, "ggplot")))) out("At least one element of argument 'frames' is not a ggplot object.", type = 3)
  
  if(!is.character(out_file)) out("Argument 'out_file' must be of type 'character'.", type = 3)
  if(all(file.exists(out_file), !isTRUE(overwrite))) out("Defined output file already exists and overwriting is disabled.", type = 3)
  num.args <- c(fps = fps, width = width, height = height, res = res)
  catch <- sapply(1:length(num.args), function(i) if(!is.numeric(num.args[[i]])) out(paste0("Argument '", names(num.args)[[i]], "' must be of type 'numeric'."), type = 3))
  
  out_ext <- tolower(tail(unlist(strsplit(out_file, "[.]")), n=1))
  out("Rendering animation...")
  #frames_expr <- expression(moveVis:::.lapply(frames, function(x) quiet(print(x))))
  
  if(out_ext == "gif"){
    if(length(frames) > 800) out("The number of frames exceeds 800 and the GIF format is used. This format may not be suitable for animations with a high number of frames, since it causes large file sizes. Consider using a video file format instead.", type = 2)
    save_gif(.lapply(frames, function(x) quiet(print(x))), gif_file = out_file, width = width, height = height, delay = (1/fps), progress = T, res = res, ...)
  }else{
    av_capture_graphics(.lapply(frames, function(x) quiet(print(x))), output = out_file, width = width, height = height, res = res, framerate = fps, verbose = verbose, ...) #, vfilter =' framerate=fps=10') 
  }
  if(isTRUE(display)) utils::browseURL(out_file)
}