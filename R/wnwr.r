#' wnwr provides ...
#'
#' @section Package options:
#'
#' It uses the following \code{options} to configure behaviour:
#' \itemize{
#'   \item \code{wnwr.supported.search.types}: vector of supported search types
#'   \item \code{wnwr.supported.search.opts}: vector of supported search options
#'   \item \code{wnwr.supported.synset.types}: vector of supported synset types
#'   \item \code{wnwr.wn.command}: wordnet shell command
#' }
#' @docType package
#' @name wnwr
NULL

search_type <- list(
  antonyms        = c('antsn', 'antsv', 'antsa', 'antsr'),
  hypernyms       = c('hypen', 'hypev'),                     
  hyponyms        = c('treen', 'treev'), # tree
  verb_entailment = c('entav'), 
  synonyms        = c('synsn', 'synsv', 'synsa', 'synsr'), # ordered by estimated frequency
  holonyms        = c('hholn'), # hierarchical holonyms # c('holon', 'smemn', 'ssubn', 'sprtn') => all holonyms, member, substance, part of holonyms
  meronyms        = c('hmern'), # hierarchical meronyms # c('meron', membn', 'subsn', 'partn') => all meronyms, has member meronyms, has substance meronyms, has part meronyms
	cause_to        = c('causv'), 
  pertainyms      = c('perta', 'pertar'),	
  attributes      = c('attrn', 'attrna'),  
  derived_forms   = c('derin', 'deriv'),
  domain          = c('domnn', 'domnv' , 'domna', 'domnr'),
  domain_terms    = c('domtn', 'domtv', 'domta', 'domtr'),
  polysemy_count  = c('famln', 'famlv', 'famla', 'famlr'), # familiarity
  verb_frames     = c('framv'),	 
  sisters         = c('coorn', 'coorv'), # coordinate terms
  synonym_groups  = c('simsv'), # grouped by similarity of meaning
  compound_words  = c('grepn', 'grepv', 'grepa' ,'grepr'),
  sense_overview  = c('over')
)
search_opt <- c('h', 'g', 'l', 'a', 'o', 's')
synset_type <- list(n = 'noun', v = 'verb', a = 'adj', r = 'adv')
wn_command <- 'wn'

.onLoad <- function(libname, pkgname) {
  ops <- options()
  wnwr_opts <- list(
    wnwr.supported.search.types = search_type,
    wnwr.supported.search.opts = search_opt,
    wnwr.supported.synset.types = synset_type,
    wnwr.wn.command = wn_command
  )
  to_set <- !(names(wnwr_opts) %in% names(ops))
  if (any(to_set)) options(wnwr_opts[to_set])
  invisible()
}
