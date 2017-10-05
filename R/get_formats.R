#' Get all available output file formats
#'
#' \code{get_formats} returns all available file formats that can be used with \code{\link{animate_move}} (\code{out_format}). The available output formats depend on the additional libraries that are installed on your system (see \code{\link{get_libraries}}).
#'  
#' @param tool character. Default is "auto" to return all available formats. If set to either 'convert', 'ffmpeg' or 'avconv', the output formats made available by one of these specific library tools are returned. 
#' 
#' @return A character vector containing available output formats. Each vector element can serve as input to the \code{out_format} argument of \code{\link{animate_move}}.
#' 
#' @examples
#' \dontrun{
#' #Get all formats available from the currently installed libraries
#' formats <- get_formats()
#' print(formats)
#' 
#' #For example, use the output as input to animate_move() out_format argument
#' out_format <- formats[1]
#' }
#' 
#' #Get formats provided by specifc tool
#' formats <- get_formats(tool = "convert")
#' 
#' @author Jakob Schwalb-Willmann
#' @seealso \code{\link{get_libraries}}
#' 
#' @export


get_formats <- function(tool = "auto"){
  
  detect_formats <- function(call){
    formats.supp <- sapply(lapply(cmd.fun(paste0(call," -formats"),intern = TRUE, ignore.stdout = FALSE, ignore.stderr = TRUE),
                                function(x){substring(x,5,20)}), function(x){unlist(strsplit(x, " "))[1]})
    formats.supp <- formats.supp[5:length(formats.supp)]
     return(formats.supp)
  }
  
  log_level <- 1
  out <- function(input,type = 1, ll = log_level, msg = FALSE){
    signs <- c("", "")
    if(type == 2 & ll <= 2){warning(paste0(signs[2],input), call. = FALSE, immediate. = TRUE)}
    else{if(type == 3){stop(input,call. = FALSE)}else{if(ll == 1){
      if(msg == FALSE){cat(paste0(signs[1],input),sep="\n")
      }else{message(paste0(signs[1],input))}}}}
  }
  
  if(.Platform$OS.type == 'windows'){cmd.fun <- shell}else{cmd.fun <- system}
  
  if(tool == "auto"){
    command <- NULL
    if(is.na(match("convert",get_libraries(log_level = 3))) == FALSE){command <- "convert"}
    if(is.na(match("avconv",get_libraries(log_level = 3))) == FALSE){command <- "avconv"}
    if(is.na(match("ffmpeg",get_libraries(log_level = 3))) == FALSE){command <- "ffmpeg"}
    if(is.null(command)){
      out("None of the necessary libraries could be detected on your system. Please run get_libraries() or install needed libraries manually (see manual).", type=3)
    }else{
      if(command == "convert"){df <- "gif"}else{df <- detect_formats(command)}
    }
  }else{
    if(is.na(match(tool,c("convert", "ffmpeg", "avconv"))) == FALSE){
      if(tool == "convert"){df <- "gif"}else{df <- detect_formats(tool)}
    }else{
      out(paste0("'",toString(tool),"' not recognized. 'tool' must either be 'convert', 'ffmpeg' or 'avconv'."), type=3)
    }
  }
  vid <- c("gif", "mov", "mp4", "flv", "avi", "mpeg", "3gp", "ogg")
  return(vid[sort(as.numeric(na.omit(match(df,vid))))])
}