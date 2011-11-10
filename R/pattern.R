##' Patterns to match and extract R code in a document
##'
##' Patterns are regular expressions and will be used in functions
##' like \code{\link[base]{grep}} to extract R code and chunk
##' options. The object \code{kpat} controls the patterns currently
##' used; see the references and examples for usage.
##'
##' @references Usage: \url{http://yihui.github.com/knitr/objects}
##'
##' Components in \code{kpat}: \url{http://yihui.github.com/knitr/patterns}
##' @export
##' @examples library(knitr)
##' opat = kpat$get() # old pattern list (to restore later)
##'
##' apats = optk$get('all.patterns')  # a list of all built-in patterns
##' str(apats)
##' kpat$set(apats[['rnw']]) # set pattern list from apats
##'
##' kpat$get(c('chunk.begin', 'chunk.end', 'inline.code'))
##'
##' ## a customized pattern list; has to empty the original patterns first!
##' kpat$restore()
##' ## we may want to use this in an HTML document
##' kpat$set(list(chunk.begin = '<!--helloR\\s+(.*)', chunk.end = '^byeR-->'))
##' str(kpat$get())
##'
##' kpat$set(opat)  # put the old patterns back
kpat =
    new_defaults(list(chunk.begin = NULL, chunk.end = NULL, chunk.code = NULL,
                      inline.code = NULL, global.options = NULL,
                      header.begin = NULL, document.begin = NULL, ref.label = NULL))

knit_patterns = kpat  # I feel safter to use this longer name internally

## TODO: use inline.sep to alternate between R code and text, like <?php for begin ?>text<?php for end ?> (text will be printed as-is for several times)
## in LaTeX, may need to put this \newcommand{\rinline}[1]{R output}


## helper functions

## is it a group pattern?
group_pattern = function(pattern) {
    !is.null(pattern) && str_detect(pattern, '\\(.+\\)')
}
