word_info_cmd <- function(word) {
  wn_cmd(word, info = TRUE)
}

#' @export
word_info <- function(word) {
  out <- exec(word_info_cmd(word))
  return(get_info_res(out$res))
}

get_info_res <- function(output) {
  if (missing(output)) stop(missing_message(output))
  assert_that(not_null(output))
  # remove empty string elements
  output <- output[output != ' ']
  # retrieve supported synset types
  synsets <- getOption('wnwr.supported.synset.types')
  # retrieve supported search types
  types <- getOption('wnwr.supported.search.types')
  # identify and index synset
  index <- cumsum(grepl(paste0('for ', synsets, collapse = '|'), output))
  # subset by index as factor
  output <- setNames(
    lapply(split(output, index), function(i) {
      i <- i[-1]
      if (length(i) > 0) i
      else NULL
    }),
    synsets
  )
  # matching search types only in not null children
  res <- lapply(Filter(Negate(is.null), output), function(i) {
    sub(paste0('.*(', paste(types, collapse = '|'), ').*'), '\\1', i)
  })
  # return
  if (length(res) > 0) return(res)
  else return(NULL)
}

has_info <- function(word) {
  
}
