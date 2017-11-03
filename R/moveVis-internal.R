#Suppress messages and warnings
quiet <- function(expr){
  return(suppressWarnings(suppressMessages(expr)))
}