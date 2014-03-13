info_cmd <- function(word) {
  wn_cmd(word, info = TRUE)
}

extract_info <- function(info) {
  if (missing(info)) stop(missing_message(info))
  if (is.null(info)) return(info)
  assert_that(is_result(info))
  # remove empty string elements
  info <- delete_whit(info$result, '\\s*')
  # retrieve supported search types
  types <- unlist(flatten(getOption('wnwr.supported.search.types')))
  # retrieve supported synset types
  synsets <- unlist(unname(getOption('wnwr.supported.synset.types')))
  # identify and index synset
  index <- identify_synsets(info)
  # subset by index as factor
  info <- setNames(
    lapply(split(info, unlist(index)), function(i) {
      i <- i[-1]
      if (length(i) > 0) i
      else NULL
    }),
    unique(names(index))
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
has <- function(word, search, synset = NULL, details = FALSE) {
  assert_that(is.logical(details))
  info <- info(word)
  if (!is.null(synset)) {
    assert_that(not_empty_character_vector(synset))
    synset <- match.arg(synset, unlist(unname(getOption('wnwr.supported.synset.types'))), several.ok = TRUE)
    info <- info[synset]
  }
  result <- vapply(info, function(x) search %in% x, logical(1))
  if (!details) return(any(result))
  else return(result)
}
