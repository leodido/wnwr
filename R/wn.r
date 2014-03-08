library(wordnet)

search_type <- c(
  'antsn', 'antsv', 'antsa', 'antsr',   # antonyms
  'hypen', 'hypev',                     # hypernyms
  'hypon', 'hypov', 'treen', 'treev',   # hyponyms & hyponym tree
  'entav',  		                        # verb entailment
  'synsn', 'sysnv', 'sysna', 'sysnr',		# synonyms (ordered by estimated frequency)
  'smemn',            			            # member of holonyms
  'ssubn',			                        # substance of holonyms
  'sprtn',			                        # part of holonyms
  'membn',			                        # has member meronyms
  'subsn',			                        # has substance meronyms
  'partn',			                        # has part meronyms
  'meron',		                        	# all meronyms
  'holon',			                        # all holonyms
  'causv',			                        # cause to
  'perta', 'pertar',		                # pertainyms
  'attrn', 'attrna',      		          # attributes
  'derin', 'deriv',	                    # derived forms
  'domnn', 'domnv' , 'domna', 'domnr',	# domain
  'domtn', 'domtv', 'domta', 'domtr',	  # domain terms
  'famln', 'famlv', 'famla', 'famlr',   # familiarity & polysemy count
  'framv',			                        # verb frames
  'coorn', 'coorv',	                    # coordinate terms (sisters)
  'simsv',			                        # synonyms (grouped by similarity of meaning)
  'hmern',			                        # hierarchical meronyms
  'hholn',		                          # hierarchical holonyms
  'grepn', 'grepv', 'grepa' ,'grepr',	  # list of compound words
  'over'                                # overview of senses
)

search_opts <- c('h', 'g', 'l', 'a', 'o', 's')

# FIXME: anche varie search a chiamata ..

wn <- function(word, search = NULL, opts = NULL, sense_num = NULL) {
  if (!is.character(word)) stop('the parameter "word" requires a string value.')
  search <- match.arg(search, c(NA, search_type), several.ok = TRUE)
  if (is.na(search[1])) stop(' the parameter "search" requires a string or a vector of strings.')
  opts <- match.arg(opts, c(NA, search_opts), several.ok = TRUE)  
  if (!is.null(sense_num) && !is.integer(sense_num)) stop('the parameter "sense_num" requioutput an integer value.')
  
  if (initDict()) {
    cmd <- paste('wn', shQuote(word))
    if (!is.na(opts[1])) cmd <- paste(cmd, paste(opts, collapse = ' -'), sep = ' -')
    if (!is.null(sense_num)) cmd <- paste(cmd, sense_num, sep = ' -n')
    cmd <- paste(cmd, search, sep = ' -')
    output <- suppressWarnings(tryCatch(system(cmd, intern = TRUE), error = I))
    if (length(output) > 0) {
      # TODO: process output
      attrs <- attributes(output)
      found_senses <- attrs$status
      
      
      return(list(res = paste(output, collpase = ''), found_senses = found_senses))
    } else {
      return(NULL)
    }
  } else {
    stop('wordnet not found.')
  }
}

ants <- function(word, pos = c('n', 'v', 'a', 'r')) {
  
}