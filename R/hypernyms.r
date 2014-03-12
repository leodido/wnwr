#' @export
hypernyms <- function(word, opt = NULL, sense_num = NULL) {
  this <- as.character(match.call()[[1]])
  types <- getOption('wnwr.supported.search.types')[this][[1]]
  exec(wn_cmd(word, types, opt, sense_num))
}

# TODO: extract_tree
# sense => .. =>

extract_tree <- function() {
  
}