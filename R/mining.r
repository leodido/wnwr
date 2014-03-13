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

#' Removes elements that matches regexp
#'
#' It is vectorized.
delete_whit <- function(str, regexp) {
  str[!grepl(paste0('^', regexp , '$'), str)]
}

#' Indexes a character vector identifying the synset which its elements belong
#' 
#' It returns an index.
identify_synsets <- function(data, synsets = unlist(unname(getOption('wnwr.supported.synset.types')))) {
  # identify and index synset
  synset_regex <- paste(synsets, collapse = '|')
  flags <- grepl(synset_regex, data)
  nms <- sub(paste0('.*?(', synset_regex, ').*'), '\\1', data) # non greedy match
  assert_that(length(flags) == length(nms))
  nms <- nms[flags]
  index <- cumsum(flags)
  index <- flatten(tapply(index, index, function(i) setNames(i, rep(nms[i[1]], length(i))), simplify = FALSE))
  assert_that(length(unique(index)) == length(synsets), !any(unlist(index)) == 0)
  return(index)
}
