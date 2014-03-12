#' Extracts lexicographer information from a wn string
#' 
#' It is vectorized.
extract_lexinfo <- function(str) {
  
}

#' Extracts gloss from a wn string
#'  
#' It is vectorized.
extract_gloss <- function(str) {
  
}

#' Extracts synset offset from a wn string
#' 
#' It is vectorized.
extract_offset <- function(str) {
  regex <- '\\{(\\d+)\\}\\s'
  locat <- str_locate(str, regex)
  return(list(
    offset = str_trim(substr(str, locat[, 'start'], locat[, 'end']), side = 'right'),
    string = substr(str, locat[, 'end'] + 1, str_length(str))
  ))
}

# extract_arrow # =>
