wn_cmd <- function(word, search, opt = NULL, sense_num = NULL) {
  # check word arg
  if (missing(word)) stop(missing_message(word))
  assert_that(not_empty_string(word))
  # check search arg
  if (missing(search)) stop(missing_message(search))
  assert_that(not_empty_character_vector(search))
  search <- match.arg(search, getOption('wnr.supported.search.types'), several.ok = TRUE)
  # check opt arg
  current_opt <- as.list(environment())$opt
  opt <- match.arg(opt, getOption('wnr.supported.search.opts'))
  
  if (initDict()) {
    cmd <- paste('wn', shQuote(word))
    if (identical(current_opt, opt)) cmd <- paste(cmd, opt, sep = ' -')
    if (!is.null(sense_num)) {
      assert_that(is.count(sense_num))
      cmd <- paste(cmd, sense_num, sep = ' -n')
    }
    cmd <- paste(
      cmd,
      substring(paste(' -', search, collapse = '', sep = ''), 2),
      sep = ' '
    )
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
