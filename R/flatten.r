flatten <- function(x) {
  y <- list()
  rapply(x, function(x) y <<- c(y,x))
  return(y)
}

depth <- function(this, thisdepth = 0) {
  if(!is.list(this)) {
    return(thisdepth)
  } else{
    return(max(unlist(lapply(this, depth, thisdepth = thisdepth + 1))))   
  }
}
