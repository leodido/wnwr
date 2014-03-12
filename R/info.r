info_cmd <- function(word) {
  wn_cmd(word, info = TRUE)
}

extract_info <- function(info) {
  if (missing(info)) stop(missing_message(info))
  if (is.null(info)) return(info)
  assert_that(is_result(info))
  # remove empty string elements
  info <- gsub('^\\s+$', '', info$result)
  info <- info[info != '']
  # retrieve supported synset types
  synsets <- unlist(unname(getOption('wnwr.supported.synset.types')))
  # retrieve supported search types
  types <- unlist(flatten(getOption('wnwr.supported.search.types')))
  # identify and index synset
  synset_regex <- paste(synsets, collapse = '|')
  flags <- grepl(synset_regex, info)
  nms <- sub(paste0('.*?(', synset_regex, ').*'), '\\1', info) # non greedy match
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
    i <- sub(paste0('.*(', paste(types, collapse = '|'), ').*'), '\\1', i)
    i[i %in% types]
  })
  return(c(results, Filter(is.null, info))[synsets])
}

#' @export
info <- function(word) {
  out <- exec(info_cmd(word))
  return(extract_info(out))
}

#' @export
has <- function(word, search, synset = NULL) {
  info <- info(word)
  if (!is.null(synset)) {
    assert_that(not_empty_character_vector(synset))
    synset <- match.arg(synset, unlist(unname(getOption('wnwr.supported.synset.types'))), several.ok = TRUE)
    info <- info[synset]
  }
  return(lapply(info, function(x) search %in% x))
}
