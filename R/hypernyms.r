#' @export
hypernyms <- function(word, opt = NULL, sense_num = NULL) {
  this <- as.character(match.call()[[1]])
  types <- getOption('wnwr.supported.search.types')[this][[1]]
  exec(wn_cmd(word, types, opt, sense_num))
}

# TODO: extract_tree

extract_tree <- function() {
  
}

# e.g wn 'beef' -hypen -hypev
# list(
#   noun = list(
#     c()
#   ),
#   verb = list(
#     list(
#       sense = c('gripe', 'bitch', 'grouse', 'crab', 'beef', 'squawk', 'bellyache', 'holler'),
#       gloss = 'complain',
#       examples = c('What was he hollering about?')
#       hypernym = list(
#         sense = c('complain', 'kick', 'plain', 'sound off', 'quetch', 'kvetch'),
#         gloss = 'express complaints, discontent, displeasure, or unhappiness',
#         examples = c('My mother complains all day', 'She has a lot to kick about')
#       )
#     ),
#   )
# )