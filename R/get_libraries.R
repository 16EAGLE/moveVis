#' Detect and/or download and install extern system libraries needed by moveVis
#'
#' \code{get_libraries} trys to detect the libraries on your system that are needed by moveVis to be able to deal with different output file formats. get_libraries() searches for'convert' form the ImageMagick library (needed for .gif support), 'ffmpeg' from the FFmpeg library or 'avconv' from the 'libav-tools' library (both needed for video support).
#' You can execute get_libraries() to make sure, these libraries are correctly installed on your system. It is recommended to have ImageMagick including 'convert' and FFmpeg including 'ffmpeg' installed to gain support of all available output file formats. The function's return can serve as \code{conv_dir} input to the animate_move() function.
#'  
#' \code{get_imconvert} is an alias function of \code{get_libraries} inlcuded for compatibility reasons that does the same as \code{get_libraries}, but is only checking for the \code{convert} tool of ImageMagick. It is recommended to use \code{get_libraries} instead.
#'  
#' @param lib.tool character. Vector of libraries to look for. This can be either 'convert', 'ffmpeg', 'avconv' or a combination. Default is "all" to check for all possible libraries.
#' @param dir character. Directory were to download, unzip and install ImageMagick. If set to "auto", a temporary directory is used. Default is "none", which will cause no automatic installation.
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
#' If you are running Windows, the function can download and either temporarily or permanently install ImageMagick
#' on your Windows system in case that no existing installation can be found. For FFmpeg or libav, a download link will be provided.
#' 
#' If you are running Linux, the function provides a root permission requiring command to be executed once by the user in the terminal
#' to install ImageMagick, FFmpeg or libav, if not installed. On standard Ubuntu distributions, ImageMagick and FFmpeg belong to the preinstalled packages by default.
#' If you are running macOS (former OSX), manual installation is necessary or installation via a package manager such as 'brew'.
#' 
#' You can install the necessary libraries manually and then run get_libraries to just identify the needed commands:
#' \itemize{
#'   \item ImageMagick: \url{https://www.imagemagick.org/script/download.php})
#'   \item FFmpeg from \url{https://www.ffmpeg.org/download.html}
#'   \item libav from \url{https://libav.org/download/}  
#' }
#' @examples
#' #conv_dir of the animate_move() function
#' conv_dir <- get_libraries()
#' 
#' @author Jakob Schwalb-Willmann
#' @seealso \code{\link{animate_move}}
#' 
#' @importFrom utils download.file unzip
#' @importFrom RCurl getURL
#' @export


get_libraries <- function(lib.tool = "all", dir = "none", ...){
  
  arg <- list(...)
  s_try <- try(arg$nodownload)
  if(class(s_try) == "NULL"){
    if(dir == "none"){
      nodownload <- TRUE
    }else{
      nodownload <-  FALSE
    }
  }else{
    nodownload <- arg$nodownload
  }
  s_try <- try(arg$log_level)
  if(class(s_try) == "NULL"){log_level <-  1}else{log_level <- arg$log_level}

  #Define output handling
  out <- function(input,type = 1, ll = log_level, msg = FALSE){
    signs <- c("", "")
    if(type == 2 & ll <= 2){warning(paste0(signs[2],input), call. = FALSE, immediate. = TRUE)}
    else{if(type == 3){stop(input,call. = FALSE)}else{if(ll == 1){
      if(msg == FALSE){cat(paste0(signs[1],input),sep="\n")
      }else{message(paste0(signs[1],input))}}}}
  }
  
  #if(.Platform$OS.type == 'windows'){cmd.fun <- shell}else{cmd.fun <- system}
  cmd.fun <- system
  if(dir == "auto"){dir <- tempdir()}
  
  lib.avail <- c("convert", "avconv", "ffmpeg")
  if(lib.tool[1] == "all"){lib.tool <- lib.avail
  }else{if(is.na(match(TRUE,is.na(match(lib.tool, lib.avail)))) == FALSE){
    out(paste0("'",paste(lib.tool, collapse = ", "),"' is not supported by moveVis. Argument 'lib.tool' can be either 'all' or 'convert', 'ffmpeg', 'avconv' or a combination of the latter three."),type=3)}
  }
  
  try.lib <- sapply(lib.tool, function(x){quiet(try(cmd.fun(x,ignore.stdout = TRUE,ignore.stderr = TRUE)))})
  try.lib.which <- which(try.lib > 4)
  
  if(length(try.lib.which) == 0){lib.found <- lib.tool
  }else{lib.found <- lib.tool[-try.lib.which]}
  lib.notfound <- lib.tool[try.lib.which]
  
  if(is.na(match("convert", lib.notfound)) == FALSE){
    if(.Platform$OS.type == 'windows'){
      if(length(grep("convert.exe",list.files(paste0("C:/Program Files/",grep("ImageMagick", list.files("C:/Program Files/"),value = TRUE))))) != 0){
        conv_dir <- paste0("C:/Program Files/", list.files("C:/Program Files/")[grep("ImageMagick",list.files("C:/Program Files/"))], "/convert.exe")
        lib.found <- c(lib.found, paste0(dir,"\\imagick\\convert.exe"))
        lib.notfound <- lib.notfound[-match("convert",lib.notfound)]
      }else{
        if(!file.exists(paste0(dir,"/imagick/convert.exe"))){
          if(nodownload == TRUE){
            out("No ImageMagick installation could be found. Please install manually.",type=2)
            out("On Linux, open the terminal, enter 'sudo apt-get install imagemagick'.")
            out("On other systems, install manually from 'https://www.imagemagick.org/script/download.php'.")
          }else{
            out("Downloading portable ImageMagick binary...")
            ftp.dir <- "ftp://ftp.imagemagick.org/pub/ImageMagick/binaries/"
            zip.dir <- grep(".zip$",grep('portable', unlist(strsplit(getURL(ftp.dir,dirlistonly = TRUE),"[\\\\]|[^[:print:]]",fixed=FALSE)), value=TRUE),value = TRUE)
            f.dir <- grep(unlist(strsplit(Sys.getenv("R_ARCH"),"/"))[2],zip.dir,value = TRUE)
            f.dir <- paste0(ftp.dir,f.dir[length(f.dir)])
            download.file(url = f.dir, destfile = paste0(dir,"/imagick.zip"),method="auto")
            unzip(paste0(dir,"/imagick.zip"),exdir = paste0(dir,"/imagick"))
            file.remove(paste0(dir,"/imagick.zip"))
            lib.found <- c(lib.found, paste0(dir,"\\imagick\\convert.exe"))
            lib.notfound <- lib.notfound[-match("convert",lib.notfound)]
          }
        }
      }
    }else{
      out("No ImageMagick installation could be found. Please install manually.",type=2)
      out("On Linux, open the terminal, enter 'sudo apt-get install imagemagick'.")
      out("On other systems, install manually from 'https://www.imagemagick.org/script/download.php'.")
    }  
  }
  if(is.na(match("ffmpeg", lib.notfound)) == FALSE){
    if(is.na(match("avconv", lib.notfound)) == FALSE){
      #libav install process
      out("No FFmpeg or libav installation could be found. Please install manually.",type=2)
      out("On Linux, open the terminal, enter 'sudo apt-get install ffmpeg' for ffmpeg or 'sudo apt-get install libav-tools' for avconv.",type=1)
      out("On other systems, install manually from 'https://www.ffmpeg.org/download.html' or 'https://libav.org/download/'.",type=1)
    }
  }
  
  if(length(lib.found) != 0){out(paste0("Detected library commands on this system: ",paste0(lib.found, collapse = ", ")))}
  if(length(grep("convert",lib.notfound)) == 1 & length(lib.found) <= 1){
    out(paste0("Could not detect or install the following library commands: ",paste0(lib.notfound, collapse = ", ")), type = 2)
    out(paste0("Please follow installation instructions and then run get_libraries() again."))
  }
  if(length(lib.found) != 0){return(lib.found)}else{return("")}
}


#' @rdname get_libraries
#' @export

get_imconvert <- function(dir = "auto"){
  get_libraries(lib.tool = "convert", dir = dir)
}
