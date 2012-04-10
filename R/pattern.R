## initial pattern list
.pat.init = list(chunk.begin = NULL, chunk.end = NULL, chunk.code = NULL,
                inline.code = NULL, global.options = NULL, input.doc = NULL,
                ref.chunk = NULL, header.begin = NULL, document.begin = NULL, 
                ref.label = NULL)

#' Patterns to match and extract R code in a document
#' 
#' Patterns are regular expressions and will be used in functions like
#' \code{\link[base]{grep}} to extract R code and chunk options. The object
#' \code{knit_patterns} controls the patterns currently used; see the references
#' and examples for usage.
#' 
#' @references Usage: \url{http://yihui.name/knitr/objects}
#'   
#' Components in \code{knit_patterns}: \url{http://yihui.name/knitr/patterns}
#' @export
#' @examples library(knitr)
#' opat = knit_patterns$get() # old pattern list (to restore later)
#'
#' apats = opts_knit$get('all.patterns')  # a list of all built-in patterns
#' str(apats)
#' knit_patterns$set(apats[['rnw']]) # set pattern list from apats
#'
#' knit_patterns$get(c('chunk.begin', 'chunk.end', 'inline.code'))
#'
#' ## a customized pattern list; has to empty the original patterns first!
#' knit_patterns$restore()
#' ## we may want to use this in an HTML document
#' knit_patterns$set(list(chunk.begin = '<!--helloR\\s+(.*)', chunk.end = '^byeR-->'))
#' str(knit_patterns$get())
#'
#' knit_patterns$set(opat)  # put the old patterns back
knit_patterns = new_defaults(.pat.init)

## in LaTeX, may need to put this \newcommand{\rinline}[1]{R output}

## convenience functions
set_pattern = function(type) {
  knit_patterns$restore()
  knit_patterns$set(all_patterns[[type]])
}

#' Set regular expressions to read input documents
#' 
#' These are convenience functions to set pre-defined pattern lists (the syntax
#' to read input documents). The function names are built from corresponding
#' file extensions, e.g. \code{pat_rnw()} can set the Sweave syntax to read Rnw
#' documents.
#' @rdname pat_fun
#' @return The patterns object \code{\link{knit_patterns}} is modified as a side
#'   effect.
#' @export
#' @examples ## see how knit_patterns is modified
#' knit_patterns$get(); pat_rnw(); knit_patterns$get()
#'
#' knit_patterns$restore()  # empty the list
pat_rnw = function() set_pattern('rnw')
#' @rdname pat_fun
#' @export
pat_brew = function() set_pattern('brew')
#' @rdname pat_fun
#' @export
pat_tex = function() set_pattern('tex')
#' @rdname pat_fun
#' @export
pat_html = function() set_pattern('html')
#' @rdname pat_fun
#' @export
pat_gfm = function() set_pattern('gfm')

## helper functions

## is it a group pattern?
group_pattern = function(pattern) {
  !is.null(pattern) && str_detect(pattern, '\\(.+\\)')
}

## automatically detect the chunk patterns
detect_pattern = function(text) {
  for (p in names(all_patterns)) {
    pat = all_patterns[[p]][['chunk.begin']]
    if (length(pat) && length(grep(pat, text))) return(p)
  }
  NULL
}
