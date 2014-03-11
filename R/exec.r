exec <- function(cmd) {
  # check cmd argument
  if (missing(cmd)) stop(missing_message(cmd))
  assert_that(is_command(cmd))
  # command execution
  output <- suppressWarnings(tryCatch(system(cmd, intern = TRUE), error = I))
  if (length(output) > 0) {
    attrs <- attributes(output)
    found_senses <- attrs$status    
    return(structure(list(result = paste(output, collpase = ''), num_senses = found_senses), class = c('wn', 'result')))
  } else {
    return(NULL)
  }
}

#' @export
print.result <- function(x, ...) cat('* Result:\n', paste(a$result[!grepl('^\\s+$', a$result)], collapse = '\n'), '\n* Number of senses:\n', x$num_senses, '\n', sep = '')