wn_cmd <- function(word, search, opts = NULL, sense_num = NULL) {
  # check word arg
  if (missing(word)) stop(missing_message(word))
  assert_that(not_empty_string(word))
  # check search arg
  if (missing(search)) stop(missing_message(search))
  assert_that(not_empty_character_vector(search))
  search <- match.arg(search, getOption('wnr.supported.search.types'), several.ok = TRUE)
  # check opts arg
  opts <- match.arg(opts, c(NA, getOption('wnr.supported.search.opts')), several.ok = TRUE)
  
  if (initDict()) {
    cmd <- paste('wn', shQuote(word))
    if (!is.na(opts[1])) cmd <- paste(cmd, paste(opts, collapse = ' -'), sep = ' -')
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
