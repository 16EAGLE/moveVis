#' square it
#' @noRd 
.squared <- function(ext, margin_factor = 1){
  
  # calculate corner coordinates
  corn <- rbind(c(ext[1], ext[2]), c(ext[1], ext[4]), c(ext[3], ext[2]), c(ext[3], ext[4]))
  colnames(corn) <- c("x", "y")
  
  # calculate difference and distance
  ax.dist <- c(distGeo(corn[1,], corn[3,]), distGeo(corn[1,], corn[2,]))
  ax.diff <- c(ext[3]-ext[1],ext[4]-ext[2])
  
  # add difference to match equal distances
  if(ax.dist[1] < ax.dist[2]){
    x.devi <- (ax.diff[1]/ax.dist[1])*((ax.dist[2]-ax.dist[1])*margin_factor)/2
    y.devi <- ((ax.diff[2]/ax.dist[2])*(ax.dist[2]*margin_factor))-ax.diff[2]
  } else{
    x.devi <- ((ax.diff[1]/ax.dist[1])*(ax.dist[1]*margin_factor))-ax.diff[2]
    y.devi <- (ax.diff[2]/ax.dist[2])*((ax.dist[1]-ax.dist[2])*margin_factor)/2
  }
  return(st_bbox(c(ext[1]-x.devi, ext[3]+x.devi, ext[2]-y.devi, ext[4]+y.devi)))
}

#' split movement by tail length
#' @noRd 
.split <- function(m, n_tail, cols){
  lapply(1:(max(m$frame)-n_tail), function(i){
    
    # extract all rows of frame time range
    y <- m[!is.na(match(m$frame,i:(i+n_tail))),]
    y <- y[order(y$id),]
    
    # compute colour ramp from id count
    y$colour <- unlist(mapply(x = cols[unique(y$id)], y = table(y$id), function(x, y){
      f <- colorRampPalette(c(x, "white"))
      rev(f(y+4)[1:y])
    }, SIMPLIFY = F))
    return(y)
  })
}


#' plot function
#' @noRd 
.gg <- function(l, ggbmap, print_plot = T){
  lapply(l, function(x){
    p <- ggbmap + geom_path(aes(x = lon, y = lat, group = id), data = x, size = 3,
                            lineend = "round", linejoin = "round", colour = x$colour) +
      theme(aspect.ratio = 1) + scale_fill_identity() + scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0))
    if(isTRUE(print_plot)) print(p) else return(p)
  })
}