word_info_cmd <- function(word) {
  wn_cmd(word, info = TRUE)
}

#' @export
word_info <- function(word) {
  exec(word_info_cmd(word))
}

get_info_res <- function(output) {
  
}