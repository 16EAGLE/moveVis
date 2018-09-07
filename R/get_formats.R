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
    return(grep("gif", formats.supp, value = T, invert = T))
  }
  
  if(.Platform$OS.type == 'windows'){cmd.fun <- shell}else{cmd.fun <- system}
  
  if(tool[1] == "auto"){
    tool <- NULL
    if(is.na(match("convert",get_libraries(log_level = 3))) == FALSE){tool <- c(tool, "convert")}
    if(is.na(match("avconv",get_libraries(log_level = 3))) == FALSE){tool <-  c(tool, "avconv")}
    if(is.na(match("ffmpeg",get_libraries(log_level = 3))) == FALSE){tool <-  c(tool, "ffmpeg")}
    if(is.null(tool)){
      out("None of the necessary libraries could be detected on your system. Please run get_libraries() or install needed libraries manually (see manual).", type=3)
    }
  }else{
    if(length(unlist(sapply(c("convert", "ffmpeg", "avconv"), function(x, t = tool) grep(x, t)))) == 0){
      out(paste0("'",toString(tool),"' not recognized. 'tool' must eithr link to 'convert', 'ffmpeg' or 'avconv'."), type=3)
    } else{
      tool <- paste0('"', tool, '"')
    }
  }
  
  sub.conv <- grep("convert", tool)
  if(length(sub.conv) > 0){
    df <- "gif"
    tool <- tool[-sub.conv]
  }else{
    df <- NULL
  }
  df <- c(df, sapply(tool, detect_formats, USE.NAMES = F))
    
  vid <- c("gif", "mov", "mp4", "flv", "avi", "mpeg", "3gp", "ogg")
  return(vid[sort(as.numeric(na.omit(match(df,vid))))])
}

}
