\name{wn}
\alias{wn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
wn(word, search = NULL, opts = NULL, sense_num = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{word}{
%%     ~~Describe \code{word} here~~
}
  \item{search}{
%%     ~~Describe \code{search} here~~
}
  \item{opts}{
%%     ~~Describe \code{opts} here~~
}
  \item{sense_num}{
%%     ~~Describe \code{sense_num} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (word, search = NULL, opts = NULL, sense_num = NULL) 
{
    if (!is.character(word)) 
        stop("the parameter \"word\" requires a string value.")
    search <- match.arg(search, c(NA, search_type), several.ok = TRUE)
    if (is.na(search[1])) 
        stop(" the parameter \"search\" requires a string or a vector of strings.")
    opts <- match.arg(opts, c(NA, search_opts), several.ok = TRUE)
    if (!is.null(sense_num) && !is.integer(sense_num)) 
        stop("the parameter \"sense_num\" requioutput an integer value.")
    if (initDict()) {
        cmd <- paste("wn", shQuote(word))
        if (!is.na(opts[1])) 
            cmd <- paste(cmd, paste(opts, collapse = " -"), sep = " -")
        if (!is.null(sense_num)) 
            cmd <- paste(cmd, sense_num, sep = " -n")
        cmd <- paste(cmd, search, sep = " -")
        output <- suppressWarnings(tryCatch(system(cmd, intern = TRUE), 
            error = I))
        if (length(output) > 0) {
            attrs <- attributes(output)
            found_senses <- attrs$status
            return(list(res = paste(output, collpase = ""), found_senses = found_senses))
        }
        else {
            return(NULL)
        }
    }
    else {
        stop("wordnet not found.")
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
