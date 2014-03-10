wn_cmd <- function(word, search, opt = NULL, sense_num = NULL, info = FALSE) {
  # check word arg
  if (missing(word)) stop(missing_message(word))
  assert_that(not_empty_string(word))
  # check if we are in word information mode or not
  if (!info) {
    # check search arg
    if (missing(search)) stop(missing_message(search))
    assert_that(not_empty_character_vector(search))
    search <- match.arg(unique(search), getOption('wnwr.supported.search.types'), several.ok = TRUE)
    # check opt arg
    current_opt <- as.list(environment())$opt
    opt <- match.arg(opt, getOption('wnwr.supported.search.opts'))
  }
  
  if (initDict()) {
    cmd <- paste(getOption('wnwr.wn.command'), shQuote(word))
    if (info) return(cmd)
    if (identical(current_opt, opt)) cmd <- paste(cmd, opt, sep = ' -')
    if (!is.null(sense_num)) {
      assert_that(is.count(sense_num))
      cmd <- paste(cmd, sense_num, sep = ' -n')
    }
    return(paste(cmd, substring(paste(' -', search, collapse = '', sep = ''), 2)))
  } else {
    stop('wordnet not found.')
  }
}
