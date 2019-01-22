align_move <- function(m, res = "min", unit = "secs", use.digit = "min", method = "euclidean"){
  
  ## check resolution and define resolution
  if(all(!c(inherits(res, "numeric"), inherits(res, "character")))) out("Argument 'res' must either be numeric or one of c('min', 'max', 'mean').", type = 3)
  if(res == "min") res <- min(unique(unlist(timeLag(m, unit))))
  if(res == "max") res <- max(unique(unlist(timeLag(m, unit))))
  if(res == "mean") res <- mean(unique(unlist(timeLag(m, unit))))
  res <- as.difftime(res, units = unit)
  
  ## check time.digit
  ts <- timestamps(m)
  time.digits <- unique(as.numeric(format(ts, .convert_units(unit))))
  
  if(all(!c(inherits(use.digit, "numeric"), inherits(use.digit, "character")))) out("Argument 'use.digit' must either be numeric or one of c('min', 'max', 'mean').", type = 3)
  if(use.digit == "min") use.digit <- min(time.digits)
  if(use.digit == "max") use.digit <- max(time.digits)
  if(use.digit == "mean") use.digit <- round(mean(time.digits))
  
  ts.shoulder <- list(min(ts), max(ts))
  set.fun <- list("secs" = `second<-`, "mins" = `minute<-`, "hours" = `hour<-`, "days" = `day<-`)
  set.fun <- set.fun[[match(unit, names(set.fun))]]
  ts.shoulder <- lapply(ts.shoulder, set.fun, value = use.digit)
  ts.target <- seq.POSIXt(ts.shoulder[[1]], ts.shoulder[[2]], by = res)
  
  m.indi <- if(inherits(m, "MoveStack")) split(m) else list(m)
  
  
  
  m.ts <- timestamps(m)
  
  
  test <- interpolateTime(m, time = res, spaceMethod = method)
  
  
}