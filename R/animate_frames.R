#' Animate frames
#'
#' \code{animate_frames} creates an animation from a list of frames computed with \code{\link{create_frames}}.
#'
#' @inheritParams create_frames
#' @param out_file character, the output file path, e.g. "/dir/to/file.mov". The file extension must correspond to a known video/image format such as mp4, mkv, mov, flv or gif.
#' @param framerate numeric, the number of frames to be displayed per second. Default is 2.
#' @param width numeric, width of the output animation in pixels.
#' @param height numeric, height of the output animation in pixels.
#' @param display logical, whether the animation should be displayed after rendering or not.
#' @param render_function function, an optional render function. If undefined (default), an appropriate render function is selected automatically depending on the output file extension and the available system libraries.
#' @param ... additional arguments to be passed to the render function.
#'
#' @return None or an R graphics window displaying the animation
#' 
#' @importFrom av av_capture_graphics
#' 
#' @author Jakob Schwalb-Willmann
#' @seealso \code{\link{create_frames}}
#' 
#' @export

animate_frames <- function(frames, out_file, framerate = 2, width = 800, height = 800,  display = TRUE, render_function = NULL, ...){
  
  av_capture_graphics(print(frames), output = out_file, width = width, height = height, res = 144, framerate = 20, ...) #, vfilter =' framerate=fps=10')
  utils::browseURL(out_file)
}