exec <- function(cmd) {
  assert_that(is.string(cmd))
  output <- suppressWarnings(tryCatch(system(cmd, intern = TRUE), error = I))
  if (length(output) > 0) {
#   TODO: process output
  attrs <- attributes(output)
  found_senses <- attrs$status    
  return(list(res = paste(output, collpase = ''), found_senses = found_senses))
  } else {
    return(NULL)
  }
}
