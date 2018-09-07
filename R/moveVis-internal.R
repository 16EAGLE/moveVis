#' Suppress messages and warnings
#' @noRd 
quiet <- function(expr){
  #return(expr)
  return(suppressWarnings(suppressMessages(expr)))
}


#' Suppress messages and warnings
#' @noRd 
out <- function(input,type = 1, ll = getOption("moveVis.log_level"), msg = getOption("moveVis.msg")){
  signs <- c("", "")
  if(type == 2 & ll <= 2){warning(paste0(signs[2],input), call. = FALSE, immediate. = TRUE)}
  else{if(type == 3){stop(input,call. = FALSE)}else{if(ll == 1){
    if(msg == FALSE){cat(paste0(signs[1],input),sep="\n")
    }else{message(paste0(signs[1],input))}}}}
}


#' check a command
#' @noRd 
check.cmd <- function(cmd){
  if(.Platform$OS.type == 'windows'){
    run <- quiet(try(shell(cmd, intern = T), silent = T))
  }else{
    run <- quiet(try(system(cmd, intern = T, ignore.stdout = T, ignore.stderr = T), silent = T))
  }
  if(!is.null(attributes(run))) F else T
  
  #sapply(cmd, function(x, cf = cmd.fun){
  #  run <- try(cf(x, intern = T, ignore.stdout = T, ignore.stderr = T), silent = TRUE)
  #  if(inherits(run, "try-error")) FALSE else TRUE
  #  #if(length(grep("Error", as.character(run[1]))) != 0) FALSE else TRUE
  #})
}


#' package startup
#' @noRd 
.onLoad <- function(libname, pkgname){
  if(is.null(getOption("moveVis.log_level")))  options(moveVis.log_level = 1)
  if(is.null(getOption("moveVis.msg")))  options(moveVis.msg = FALSE)
  
  if(is.null(getOption("moveVis.convert_avail"))) options(moveVis.convert_avail = F)
  if(is.null(getOption("moveVis.ffmpeg_avail"))) options(moveVis.ffmpeg_avail = F)
  if(is.null(getOption("moveVis.avconv_avail"))) options(moveVis.avconv_avail = F)
  
  if(is.null(getOption("moveVis.convert_cmd"))) options(moveVis.convert_cmd = "")
  if(is.null(getOption("moveVis.ffmpeg_cmd"))) options(moveVis.ffmpeg_cmd = "")
  if(is.null(getOption("moveVis.avconv_cmd"))) options(moveVis.avconv_cmd = "")
  
  get_libraries()
}
