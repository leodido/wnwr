#' Extracts lexicographer information from a wn string
#' 
#' It is vectorized.
extract_lexinfo <- function(str) {
  locat <- str_locate(str, '<\\w+\\.\\w+>\\s')
  lexinfo <- str_trim(substr(str, locat[, 'start'], locat[, 'end']), side = 'right')
  return(list(
    lexinfo = substring(lexinfo, 2, str_length(lexinfo) - 1),
    sense = substr(str, locat[, 'end'] + 1, str_length(str))
  ))
}

#' Extracts gloss from a wn string
#'  
#' It is vectorized.
extract_gloss <- function(str) {
  data <- strsplit(str, ' -- ')
  glosses <- str_replace_all(sapply(data, `[[`, 2), '\\(|\\)|\\\\|\\"', '')
  glosses <- str_split_fixed(glosses, '; ', 2)
  return(list(
    gloss = glosses[, 1],
    example = str_split(glosses[, 2], '; '),
    sense = sapply(data, `[[`, 1)
  ))
}

#' Extracts synset offset from a wn string
#' 
#' It is vectorized.
extract_offset <- function(str) {
  locat <- str_locate(str, '\\{(\\d+)\\}\\s')
  offset <- str_trim(substr(str, locat[, 'start'], locat[, 'end']), side = 'right')
  return(list(
    offset = substring(offset, 2, str_length(offset) - 1),
    sense = substr(str, locat[, 'end'] + 1, str_length(str))
  ))
}

#' Removes synset number from a wn string
#' 
#' It is vectorized.
delete_sensenum <- function(str) {
  return(gsub('\\#\\d+', '', str))
}
