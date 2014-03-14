#' @export
hypernyms <- function(word, opt = NULL, sense_num = NULL) {
  this <- as.character(match.call()[[1]])
  types <- getOption('wnwr.supported.search.types')[this][[1]]
  return(extract_tree(exec(wn_cmd(word, types, opt, sense_num))))
}

# TODO: extract options
# TODO: filter by sense number

extract_tree <- function(data) {
  if (missing(data)) stop(missing_message(data))
  if (is.null(data)) return(data)
  assert_that(is_result(data))
  # remove empty string elements
  data <- delete_with(data$result, '\\s*')
  data <- delete_with(data, '\\d+\\ssense(s)?\\sof\\s.*')
  # retrieve supported synset types
  synsets <- unlist(unname(getOption('wnwr.supported.synset.types')))
  # identify and index synset
  index <- identify_synsets(data, synsets) # FIXME: see (1) @ mining.r
  # subset by index as factor
  data <- setNames(
    lapply(split(data, unlist(index)), function(i) {
      i <- i[-1]
      if (length(i) > 0) i
      else NULL
    }),
    unique(names(index))
  )
  # identify senses of each synset and process results
  lapply(data, function(sset) {
    unname(lapply(split(sset, identify_senses(sset)), function(sense) {
      sense <- sense[-1]
      if (length(sense) > 0) {
        build_list(str_trim(sense, side = 'right'))
      } else {
        NULL
      }
    }))
  })
}
