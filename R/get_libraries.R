#' Detect extern system libraries needed by moveVis
#'
#' \code{get_libraries} trys to detect the libraries on your system that are needed by moveVis to be able to deal with different output file formats. get_libraries() searches for'convert' form the ImageMagick library (needed for .gif support), 'ffmpeg' from the FFmpeg library or 'avconv' from the 'libav-tools' library (both needed for video support).
#' You can execute get_libraries() to make sure, these libraries are correctly installed on your system. It is recommended to have ImageMagick including 'convert' and FFmpeg including 'ffmpeg' installed to gain support of all available output file formats. The function's return can serve as \code{conv_dir} input to the animate_move() function.
#'  
#' \code{get_imconvert} is an deprecated alias function of \code{get_libraries} inlcuded for compatibility reasons that does the same as \code{get_libraries}, but is only checking for the \code{convert} tool of ImageMagick. It is recommended to use \code{get_libraries} instead.
#'  
#' @param lib.tool character. Vector of libraries to look for. This can be either 'convert', 'ffmpeg', 'avconv' or a combination. Default is "all" to check for all possible libraries.
#' @param ... additional arguments. Currently not used.
#' 
#' @return A character vector including all found commands or directories to the needed tools of the requested libraries. The return can serve as \code{conv_dir} input to the \code{animate_move} function.
#' 
#' @details The following tools and libraries are needed by moveVis:
#' \itemize{
#'   \item the \code{convert} tool of ImageMagick to support the GIF format
#'   \item the \code{ffmpeg} tool of FFmpeg to support video formats
#'   \item alternatively to ffmpeg, the \code{avconv} tool of libav-tools to support video formats.
#' }
#' It is recommmended to have both ImageMagick and one of the mentioned video libraries installed to be able to create all output formats with the animate_move() function (see argument \code{out_format} of \link{animate_move})
#' 
#' If you are running Windows or macOS, use the download links below to install the required software:
#' \itemize{
#'   \item ImageMagick: \url{https://www.imagemagick.org/script/download.php})
#'   \item FFmpeg from \url{https://www.ffmpeg.org/download.html}
#'   \item libav from \url{https://libav.org/download/}  
#'  }
#' If you are running macOS, an installation via a package manager such as 'brew' is recommended.
#' 
#' If you are running Linux, execute the commands below to install the required software:
#' \itemize{
#'   \item ImageMagick: \code{sudo apt-get install imagemagick}
#'   \item ffmpeg: \code{sudo apt-get install ffmpeg}
#'   \item libav: \code{sudo apt-get install libav-tools}
#' }
#' @examples
#' #conv_dir of the animate_move() function
#' conv_dir <- get_libraries()
#' 
#' @author Jakob Schwalb-Willmann
#' @seealso \code{\link{animate_move}}
#' 
#' @export

get_libraries <- function(lib.tool = "all", ...){
  
  lib.avail <- c("convert", "avconv", "ffmpeg")
  if(lib.tool[1] == "all"){lib.tool <- lib.avail
  }else{if(is.na(match(TRUE,is.na(match(lib.tool, lib.avail)))) == FALSE){
    out(paste0("'",paste(lib.tool, collapse = ", "),"' is not supported by moveVis. Argument 'lib.tool' can be either 'all' or 'convert', 'ffmpeg', 'avconv' or a combination of the latter three."),type=3)}
  }
  
  #lib.check <- check.cmd(paste0("'", lib.tool, "' -version"))
  lib.check <- check.cmd(paste0(lib.tool, " -version"))
  names(lib.check) <- lib.tool
  catch <- sapply(names(lib.check), function(x){
    eval(parse(text = paste0("options(moveVis.", x, "_avail = T)")))
    eval(parse(text = paste0("options(moveVis.", x, "_cmd = '", x, "')")))
  })
  
  return(names(lib.check)[lib.check])
}

#' @rdname get_libraries
#' @export

get_imconvert <- function(){
  get_libraries(lib.tool = "convert")
}
