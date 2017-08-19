#' Search/download and install ImageMagick
#'
#' \code{get_imconvert} searches for the convert tool being part of the ImageMagick software on your system (either Windows or Linux) or installs it.
#' The convert tool system command or directory is needed by the \code{animate_move()} function of the \code{moveVis} package.
#' If you are running Windows, the function can download and either temporarily or permanently install ImageMagick
#' on your Windows system in case that no existing installation can be found. If you are running Linux,
#' the function provides a root permission requiring command to be executed once by the user in the terminal
#' to install ImageMagick (Linux), if ImageMagick is not installed. On standard Ubuntu distributions, ImageMagick belongs to the preinstalled packages by default.
#' 
#' @param dir character. Directory were to download, unzip and install ImageMagick. If set to "auto", a temporary directory is used. Default is "auto".
#' 
#' @return The convert tool command line command or directory. The output can serve as \code{conv_dir} input to the \code{animate_move} function.
#' 
#' @examples
#' conv_dir <- get_imconvert()
#' 
#' @author Jakob Schwalb-Willmann
#' @seealso \code{\link{animate_move}}
#' 
#' @importFrom utils download.file unzip
#' @importFrom RCurl getURL
#' @export

get_imconvert <- function(dir = "auto"){
  if(dir == "auto"){dir <- tempdir()}
  if(.Platform$OS.type == 'windows'){
    if(length(grep("convert.exe",list.files(paste0("C:/Program Files/",grep("ImageMagick", list.files("C:/Program Files/"),value = TRUE))))) != 0){
      conv_dir <- paste0("C:/Program Files/", list.files("C:/Program Files/")[grep("ImageMagick",list.files("C:/Program Files/"))], "/convert.exe")
    }else{
      if(!file.exists(paste0(dir,"/imagick/convert.exe"))){
        print("Downloading portable ImageMagick...")
        ftp.dir <- "ftp://ftp.imagemagick.org/pub/ImageMagick/binaries/"
        zip.dir <- grep(".zip$",grep('portable', unlist(strsplit(getURL(ftp.dir,dirlistonly = TRUE),"[\\\\]|[^[:print:]]",fixed=FALSE)), value=TRUE),value = TRUE)
        f.dir <- grep(unlist(strsplit(Sys.getenv("R_ARCH"),"/"))[2],zip.dir,value = TRUE)
        f.dir <- paste0(ftp.dir,f.dir[length(f.dir)])
        download.file(url = f.dir, destfile = paste0(dir,"/imagick.zip"),method="auto")
        unzip(paste0(dir,"/imagick.zip"),exdir = paste0(dir,"/imagick"))
        file.remove(paste0(dir,"/imagick.zip"))
      }
      conv_dir <- paste0(dir,"\\imagick\\convert.exe")
    }
    return(conv_dir)
  }else{
    tryit <- try(system("convert",ignore.stdout = TRUE,ignore.stderr = TRUE))
    if(tryit != 1){
      print("Installing ImageMagick on this system requires root permissions by the user.")
      print("Please open the terminal, enter 'sudo apt-get install imagemagick' and then rerun get_imconvert().")
      print("This operation will not be necessary again in the future unless ImageMagick gets deinstalled.")
    }else{conv_dir <- "convert"; return(conv_dir)}
  }
}
