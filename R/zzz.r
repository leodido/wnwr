#' wnwr provides ...
#'
#' @section Package options:
#'
#' It uses the following \code{options} to configure behaviour:
#' \itemize{
#'   \item \code{wnwr.supported.search.types}: vector of supported search types
#'   \item \code{wnwr.supported.search.opts}: vector of supported search options
#' }
#' @docType package
#' @name wnwr
NULL

search_type <- c(
  'antsn', 'antsv', 'antsa', 'antsr',   # antonyms
  'hypen', 'hypev',                     # hypernyms
  'hypon', 'hypov', 'treen', 'treev',   # hyponyms & hyponym tree
  'entav',    	                        # verb entailment
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

.onLoad <- function(libname, pkgname) {
  ops <- options()
  wnwr_opts <- list(
    wnwr.supported.search.types = search_type,
    wnwr.supported.search.opts = search_opts
  )
  to_set <- !(names(wnwr_opts) %in% names(ops))
  if (any(to_set)) options(wnwr_opts[to_set])
  invisible()
}
