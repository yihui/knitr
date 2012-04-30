#' All built-in patterns
#'
#' This object is a named list of all built-in patterns.
#' @references Usage: \url{http://yihui.name/knitr/patterns}
#' @export
#' @examples all_patterns$rnw; all_patterns$html
#'
#' str(all_patterns)
all_patterns = list(
  `rnw` = list(chunk.begin = '^<<(.*)>>=', chunk.end = '^@\\s*%*',
               inline.code = '\\\\Sexpr\\{([^}]*)\\}',
               input.doc = '(^|\n) *\\\\SweaveInput\\{([^}]*)\\}',
               ref.chunk = '^\\s*<<(.*)>>\\s*$',
               global.options = '\\\\SweaveOpts\\{([^}]*)\\}',
               header.begin = '\n*\\s*\\\\documentclass[^}]+\\}',
               document.begin = '\n*\\s*\\\\begin\\{document\\}',
               ref.label = '^## @knitr (.*)$'),

  `brew` = list(inline.code = '<%[=]{0,1}\\s+([^%]*)\\s+[-]*%>'),

  `tex` = list(chunk.begin = '^%+\\s*begin.rcode\\s*(.*)',
               chunk.end = '^%+\\s*end.rcode', chunk.code = '^%+',
               ref.chunk = '^%+\\s*<<(.*)>>\\s*$',
               global.options = '%+\\s*roptions\\s*([^\n]*)',
               inline.code = '\\\\rinline\\{([^}]*)\\}',
               header.begin = '\n*\\s*\\\\documentclass[^}]+\\}',
               document.begin = '\n*\\s*\\\\begin\\{document\\}',
               ref.label = '^## @knitr (.*)$'),

  `html` = list(chunk.begin = '^<!--\\s*begin.rcode\\s*(.*)',
                chunk.end = '^\\s*end.rcode\\s*-->',
                ref.chunk = '^\\s*<<(.*)>>\\s*$',
                inline.code = '<!--\\s*rinline\\s*([^>]*)\\s*-->',
                global.options = '<!--\\s*roptions\\s*([^>]*)\\s*-->',
                header.begin = '\n*\\s*<head>',
                ref.label = '^## @knitr (.*)$'),

  `md` = list(chunk.begin = '^`{3,}\\s*\\{r(.*)\\}\\s*$',
              chunk.end = '^`{3,}\\s*$',
              ref.chunk = '^\\s*<<(.*)>>\\s*$',
              inline.code = '`r +([^`\n]+)\\s*`',
              global.options = '`ro\\s+([^`]*)\\s+or`',
              ref.label = '^## @knitr (.*)$'),

  `rst` = list(chunk.begin = "^\\.{2}\\s+\\{r(.*)\\}\\s*$",
               chunk.end = "^\\.{2}\\s+\\.{2,}\\s*$",
               chunk.code = "^\\.{2}",
               ref.chunk = "^\\.*\\s*<<(.*)>>\\s*$",
               inline.code = ":r:`([^`]*)`",
               global.options = ":roptions:`([^`]*)`",
               ref.label = "^## @knitr (.*)$")
)

## initial pattern list
.pat.init = list(
  chunk.begin = NULL, chunk.end = NULL, chunk.code = NULL, inline.code = NULL,
  global.options = NULL, input.doc = NULL, ref.chunk = NULL,
  header.begin = NULL, document.begin = NULL, ref.label = NULL
)

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
#' apats = all_patterns  # a list of all built-in patterns
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
#' @export pat_rnw pat_brew pat_tex pat_html pat_md pat_rst
#' @examples ## see how knit_patterns is modified
#' knit_patterns$get(); pat_rnw(); knit_patterns$get()
#'
#' knit_patterns$restore()  # empty the list
pat_rnw = function() set_pattern('rnw')
#' @rdname pat_fun
pat_brew = function() set_pattern('brew')
#' @rdname pat_fun
pat_tex = function() set_pattern('tex')
#' @rdname pat_fun
pat_html = function() set_pattern('html')
#' @rdname pat_fun
pat_md = function() set_pattern('md')
#' @rdname pat_fun
pat_rst = function() set_pattern('rst')

## helper functions

## is it a group pattern?
group_pattern = function(pattern) {
  !is.null(pattern) && str_detect(pattern, '\\(.+\\)')
}

## automatically detect the chunk patterns
detect_pattern = function(text) {
  for (p in names(all_patterns)) {
    for (i in c('chunk.begin', 'inline.code')) {
      pat = all_patterns[[p]][[i]]
      if (length(pat) && length(grep(pat, text))) return(p)
    }
  }
  NULL
}
