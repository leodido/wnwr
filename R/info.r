word_info_cmd <- function(word) {
  wn_cmd(word, info = TRUE)
}

process_info <- function(info) {
  if (missing(info)) stop(missing_message(info))
  if (is.null(info)) return(info)
  assert_that(not_string(info), is.character(info))
  # remove empty string elements
  info <- gsub('\\s+', '', info)
  info <- info[info != '']
  # retrieve supported synset types
  synsets <- getOption('wnwr.supported.synset.types')
  # retrieve supported search types
  types <- getOption('wnwr.supported.search.types')
  # identify and index synset
  flags <- grepl(paste(synsets, collapse = '|'), info)
  nms <- sub(paste0('.*?(', paste(synsets, collapse = '|'), ').*'), '\\1', info) # non greedy match
  assert_that(length(flags) == length(nms))
  nms <- nms[flags]
  index <- cumsum(flags)
  index <- flatten(tapply(index, index, function(i) setNames(i, rep(nms[i[1]], length(i))), simplify = FALSE))
  assert_that(length(unique(index)) == length(synsets), !any(unlist(index)) == 0)
  # subset by index as factor
  info <- setNames(
    lapply(split(info, unlist(index)), function(i) {
      i <- i[-1]
      if (length(i) > 0) i
      else NULL
    }),
    nms
  )
  # no information detected for any synsets ?
  if (length(Filter(is.null, info)) == length(synsets)) return(info)
  # matching search types only in children with information (not null children)
  results <- lapply(Filter(Negate(is.null), info), function(i) {
    sub(paste0('.*(', paste(types, collapse = '|'), ').*'), '\\1', i)
  })
  return(c(results, Filter(is.null, info))[synsets])
}

#' @export
word_info <- function(word) {
  out <- exec(word_info_cmd(word))
  return(process_info(out$result))
}

# TODO
# #' @export
# can_search <- function(word, search, synset = NULL) {
#   info <- word_info(word)
#   
# }
