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

#' get base map
#' @importFrom rosm bmaps.plot osm.raster set_default_cachedir
#' @importFrom raster stack crs crs<- extent extent<- projectRaster
#' @importFrom sp bbox
#' @importFrom graphics par
#' @noRd 
.get_bm <- function(global.ext, global.crs, map_type, api_key, frames_pixres, frames_height){
  
  e.bb <- bbox(global.ext)
  rownames(e.bb) <- c("x", "y")
  e.diff <- e.bb[,2]-e.bb[,1]
  e.add <- (e.diff[which.max(e.diff)]-e.diff[which.min(e.diff)])/2
  e.bb[which.min(e.diff),] <- e.bb[which.min(e.diff),] + c(-e.add,e.add)
  
  cache.dir <- paste0(tempdir(), "/moveVis/rosm.cache")
  if(!dir.exists(cache.dir)) dir.create(cache.dir)
  set_default_cachedir(cache.dir)
  
  if(map_type == "satellite" | map_type == "hybrid"){
    png.file <- paste0(tempdir(), "/moveVis/bm.png")
    png(png.file, width = frames_height, height = frames_height)
    par(mar=c(0,0,0,0))
    bmaps.plot(e.bb, type = if(map_type == "satellite"){"Aerial"}else{"AerialWithLabels"}, key = api_key, res = frames_pixres, stoponlargerequest = F, project = T)
    e.file <- par("usr")
    dev.off()
    
    bm <- stack(png.file)
    crs(bm) <- crs("+init=epsg:3857")
    extent(bm) <- extent(e.file)
    bm <- projectRaster(bm, crs = global.crs)
  }else{
    if(map_type == "roadmap") type <- "osm"
    if(map_type == "roadmap_dark") type <- "cartodark"
    if(map_type == "roadmap_bw") type <- "stamenbw"
    if(map_type == "roadmap_watercolor") type <- "stamenwatercolor"
    
    bm <- quiet(osm.raster(e.bb, projection = global.crs, crop=TRUE, type = type, zoomin = -1))
  }
  
  names(bm) <- c("red", "green", "blue")
  unlink(cache.dir, recursive = T, force = T)
  return(bm)
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
  
  #get_libraries()
}
