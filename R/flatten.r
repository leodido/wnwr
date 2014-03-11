flatten <- function(x) {
  y <- list()
  rapply(x, function(x) y <<- c(y,x))
  return(y)
}