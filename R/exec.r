exec <- function(cmd) {
  assert_that(is_command(cmd))
  output <- suppressWarnings(tryCatch(system(cmd, intern = TRUE), error = I))
  if (length(output) > 0) {
    attrs <- attributes(output)
    found_senses <- attrs$status    
    return(list(res = paste(output, collpase = ''), num_senses = found_senses))
  } else {
    return(NULL)
  }
}
