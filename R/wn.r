wn_cmd <- function(word, search = NULL, opts = NULL, sense_num = NULL) {
  if (missing(word)) stop(missing_message(word))
  assert_that(is.string(word))
  search <- match.arg(search, c(NA, getOption('wnr.supported.search.types')), several.ok = TRUE)
  if (is.na(search[1])) stop(' the parameter "search" requires a string or a vector of strings.')
  opts <- match.arg(opts, c(NA, getOption('wnr.supported.search.opts')), several.ok = TRUE)  
  if (!is.null(sense_num) && !is.integer(sense_num)) stop('the parameter "sense_num" requioutput an integer value.')
  
  if (initDict()) {
    cmd <- paste('wn', shQuote(word))
    if (!is.na(opts[1])) cmd <- paste(cmd, paste(opts, collapse = ' -'), sep = ' -')
    if (!is.null(sense_num)) cmd <- paste(cmd, sense_num, sep = ' -n')
    cmd <- paste(cmd, search, sep = ' -')
    return(cmd)
    # output <- suppressWarnings(tryCatch(system(cmd, intern = TRUE), error = I))
    # if (length(output) > 0) {
      # TODO: process output
      # attrs <- attributes(output)
      # found_senses <- attrs$status    
      # return(list(res = paste(output, collpase = ''), found_senses = found_senses))
    # } else {
    #   return(NULL)
    # }
  } else {
    stop('wordnet not found.')
  }
}

missing_message <- function(var) paste0("'", deparse(substitute(var)), "'", " is missing.")
