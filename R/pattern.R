#' All built-in patterns
#'
#' This object is a named list of all built-in patterns.
#' @references Usage: \url{https://yihui.name/knitr/patterns/}
#' @export
#' @seealso \code{\link{knit_patterns}}
#' @examples all_patterns$rnw; all_patterns$html
#'
#' str(all_patterns)
all_patterns = list(
  `rnw` = list(
    chunk.begin = '^\\s*<<(.*)>>=.*$', chunk.end = '^\\s*@\\s*(%+.*|)$',
    inline.code = '\\\\Sexpr\\{([^}]+)\\}', inline.comment = '^\\s*%.*',
    ref.chunk = '^\\s*<<(.+)>>\\s*$', header.begin = '(^|\n)\\s*\\\\documentclass[^}]+\\}',
    document.begin = '\\s*\\\\begin\\{document\\}',
    ext = list('rnw' = '.tex', 'snw' = '.tex', 'stex' = '.tex'),
    out.format = 'latex'),

  `brew` = list(inline.code = '<%[=]{0,1}\\s+([^%]+)\\s+[-]*%>', ext = list('brew' = NULL),
                out.format = 'brew'),

  `tex` = list(
    chunk.begin = '^\\s*%+\\s*begin.rcode\\s*(.*)', chunk.end = '^\\s*%+\\s*end.rcode',
    chunk.code = '^\\s*%+', ref.chunk = '^%+\\s*<<(.+)>>\\s*$',
    inline.comment = '^\\s*%.*', inline.code = '\\\\rinline\\{([^}]+)\\}',
    header.begin = '(^|\n)\\s*\\\\documentclass[^}]+\\}',
    document.begin = '\\s*\\\\begin\\{document\\}',
    out.format = 'latex', ext = list('rtex' = '.tex', 'tex' = NULL)),

  `html` = list(
    chunk.begin = '^\\s*<!--\\s*begin.rcode\\s*(.*)',
    chunk.end = '^\\s*end.rcode\\s*-->', ref.chunk = '^\\s*<<(.+)>>\\s*$',
    inline.code = '<!--\\s*rinline(.+?)-->', header.begin = '\\s*<head>',
    ext = list('htm' = NULL, 'html' = NULL, 'rhtm' = '.htm', 'rhtml' = '.html'),
    out.format = 'html'),

  `md` = list(
    chunk.begin = '^[\t >]*```+\\s*\\{([a-zA-Z0-9_]+.*)\\}\\s*$',
    chunk.end = '^[\t >]*```+\\s*$',
    ref.chunk = '^\\s*<<(.+)>>\\s*$', inline.code = '(?<!(^|\n)``)`r[ #]([^`]+)\\s*`',
    ext = list('rmd' = '.md', 'rmarkdown' = '.markdown', 'markdown' = NULL, 'md' = NULL),
    out.format = 'markdown'),

  `rst` = list(
    chunk.begin = '^\\s*[.][.]\\s+\\{r(.*)\\}\\s*$',
    chunk.end = '^\\s*[.][.]\\s+[.][.]\\s*$', chunk.code = '^\\s*[.][.]',
    ref.chunk = '^\\.*\\s*<<(.+)>>\\s*$', inline.code = ':r:`([^`]+)`',
    ext = list('rst' = NULL, 'rrst' = '.rst'), out.format = 'rst'),

  `asciidoc` = list(
    chunk.begin = '^//\\s*begin[.]rcode(.*)$', chunk.end = '^//\\s*end[.]rcode\\s*$',
    chunk.code = '^//+', ref.chunk = '^\\s*<<(.+)>>\\s*$',
    inline.code = '`r +([^`]+)\\s*`|[+]r +([^+]+)\\s*[+]',
    inline.comment = '^//.*',
    ext = list('asciidoc' = NULL, 'rasciidoc' = '.asciidoc', 'adoc' = NULL, 'radoc' = '.adoc'),
    out.format = 'asciidoc'),

  `textile` = list(
    chunk.begin = '^###[.]\\s+begin[.]rcode(.*)$',
    chunk.end = '^###[.]\\s+end[.]rcode\\s*$',
    ref.chunk = '^\\s*<<(.+)>>\\s*$',
    inline.code = '@r +([^@]+)\\s*@',
    inline.comment = '^###[.].*',
    ext = list('textile' = NULL, 'rtextile' = '.textile'), out.format = 'textile')
)

.sep.label = '^(#|--)+\\s*(@knitr|----+)(.*?)-*\\s*$'  # pattern for code chunks in an R script

# initial pattern list
.pat.init = list(
  chunk.begin = NULL, chunk.end = NULL, chunk.code = NULL, inline.code = NULL,
  global.options = NULL, input.doc = NULL, ref.chunk = NULL,
  header.begin = NULL, document.begin = NULL, out.format = 'unknown',
  ext = list()
)

#' Patterns to match and extract R code in a document
#'
#' Patterns are regular expressions and will be used in functions like
#' \code{\link[base]{grep}} to extract R code and chunk options. The object
#' \code{knit_patterns} controls the patterns currently used; see the references
#' and examples for usage.  All built-in patterns are available in the list
#' \link{all_patterns}.
#'
#' @seealso \code{\link{all_patterns}}
#' @references Usage: \url{https://yihui.name/knitr/objects/}
#'
#' Components in \code{knit_patterns}: \url{https://yihui.name/knitr/patterns/}
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
#' # a customized pattern list; has to empty the original patterns first!
#' knit_patterns$restore()
#' # we may want to use this in an HTML document
#' knit_patterns$set(list(chunk.begin = '<!--helloR\\s+(.*)', chunk.end = '^byeR-->'))
#' str(knit_patterns$get())
#'
#' knit_patterns$set(opat)  # put the old patterns back
knit_patterns = new_defaults(.pat.init)

# in LaTeX, may need to put this \newcommand{\rinline}[1]{R output}

# convenience functions
set_pattern = function(type) {
  knit_patterns$restore(all_patterns[[type]])
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
#' @export pat_rnw pat_brew pat_tex pat_html pat_md pat_rst pat_asciidoc pat_textile
#' @examples # see how knit_patterns is modified
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
#' @rdname pat_fun
pat_asciidoc = function() set_pattern('asciidoc')
#' @rdname pat_fun
pat_textile = function() set_pattern('textile')


# helper functions

# is it a group pattern?
group_pattern = function(pattern) {
  !is.null(pattern) && grepl('\\(.+\\)', pattern)
}

contains_pattern = function(text, pat) {
  for (i in c('chunk.begin', 'inline.code')) {
    if (length(pat[[i]]) && any(stringr::str_detect(text, pat[[i]]))) {
      return(TRUE)
    }
  }
  # didn't find any patterns
  FALSE
}

# automatically detect the chunk patterns
# returns the name of the pattern in `all_patterns`
detect_pattern = function(text = NULL, ext = NULL) {
  # check extensions
  if (!is.null(ext)) {
    ext = tolower(ext)
    for (p in names(all_patterns)) {
      pat = all_patterns[[p]]
      if (!is.null(pat[['ext']]) && (ext %in% tolower(names(pat[['ext']])))) {
        # *.Rtex indicates the tex syntax in knitr, but Rnw syntax in traditional
        # Sweave. Disambiguate by checking whether it contains Rnw patterns
        if ((ext == 'rtex') && !is.null(text) &&
            contains_pattern(text, all_patterns[['rnw']]))
            return('rnw')
        else return(p)
      }
    }
  }
  if (!is.null(text)) {
    # check for existence of patterns inside doc
    for (p in names(all_patterns)) {
      pat = all_patterns[[p]]
      if (contains_pattern(text, pat)) return(p)
    }
  }
  NULL
}
