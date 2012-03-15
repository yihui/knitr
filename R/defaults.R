new_defaults = function(value = list()) {
  defaults = value
  
  get = function(name, default = FALSE) {
    if (default) defaults = value  # this is only a local version
    if (missing(name)) defaults else {
      if (length(name) == 1) defaults[[name]] else defaults[name]
    }
  }
  set = function(...) {
    dots = list(...)
    if (length(dots) == 0) return()
    if (length(dots) == 1 && is.list(dots[[1]]))
      dots = dots[[1]]
    defaults <<- merge(dots)
    invisible(NULL)
  }
  merge = function(values) {
    defaults[names(values)] = values
    defaults
  }
  restore = function() defaults <<- value
  
  list(get = get, set = set, merge = merge, restore = restore)
}

#' Default and current chunk options
#' 
#' Options for R code chunks. When running R code, the object \code{opts_chunk}
#' (default options) is not modified by chunks (local chunk options are merged
#' with default options), whereas \code{opts_current} (current options) changes
#' with different chunks.
#' @references Usage: \url{http://yihui.name/knitr/objects}
#'   
#' A list of available options:
#' \url{http://yihui.name/knitr/options#chunk_options}
#' @export
#' @examples opts_chunk$get('prompt'); opts_chunk$get('fig.keep')
opts_chunk = 
  new_defaults(list(eval = TRUE, echo = TRUE, results = 'markup', tidy = TRUE,
                    cache = FALSE, dependson = NULL, cache.path = 'cache/',
                    ref.label = NULL, child = NULL,
                    prompt = FALSE, comment = '##', autodep = FALSE,
                    fig.keep = 'high', fig.show = 'asis', fig.align = 'default',
                    fig.path = '', fig.ext = NULL, dev = 'pdf', dpi = 72,
                    fig.width = 7, fig.height = 7,
                    fig.cap = NULL, fig.scap = NULL, fig.lp = 'fig:', fig.pos = '',
                    out.width = NULL, out.height = NULL,
                    resize.width = NULL, resize.height = NULL,
                    external = TRUE, sanitize = FALSE,
                    highlight = TRUE, size = 'normalsize',
                    warning = TRUE, error = TRUE, message = TRUE,
                    background = '#F7F7F7', split = FALSE, include = TRUE,
                    interval = 1, aniopts = 'controls;loop'))

#' @rdname opts_chunk
#' @export
opts_current = new_defaults()

#' All built-in patterns
#'
#' This object is a named list of all built-in patterns.
#' @references Usage: \url{http://yihui.name/knitr/patterns}
#' @export
#' @examples all_patterns$rnw; all_patterns$html
#'
#' str(all_patterns)
all_patterns =
  
  list(`rnw` = list(chunk.begin = '^<<(.*)>>=', chunk.end = '^@\\s*%*',
                    inline.code = '\\\\Sexpr\\{([^}]*)\\}',
                    input.doc = '(^|\n) *\\\\SweaveInput\\{([^}]*)\\}',
                    ref.chunk = '^<<(.*)>>\\s*$',
                    global.options = '\\\\SweaveOpts\\{([^}]*)\\}',
                    header.begin = '\n*\\s*\\\\documentclass[^}]+\\}',
                    document.begin = '\n*\\s*\\\\begin\\{document\\}',
                    ref.label = '^## @knitr (.*)$'),
       
       `brew` = list(inline.code = '<%[=]{0,1}\\s+([^%]*)\\s+[-]*%>'),
       
       `tex` = list(chunk.begin = '^%+\\s*begin.rcode\\s*(.*)',
                    chunk.end = '^%+\\s*end.rcode', chunk.code = '^%+',
                    global.options = '%+\\s*roptions\\s*([^\n]*)',
                    inline.code = '\\\\rinline\\{([^}]*)\\}',
                    header.begin = '\n*\\s*\\\\documentclass[^}]+\\}',
                    document.begin = '\n*\\s*\\\\begin\\{document\\}',
                    ref.label = '^## @knitr (.*)$'),
       
       `html` = list(chunk.begin = '^<!--\\s*begin.rcode\\s*(.*)',
                     chunk.end = '^\\s*end.rcode\\s*-->',
                     inline.code = '<!--\\s*rinline\\s*([^>]*)\\s*-->',
                     global.options = '<!--\\s*roptions\\s*([^>]*)\\s*-->',
                     header.begin = '\n*\\s*<head>',
                     ref.label = '^## @knitr (.*)$'))

#' Options for the knitr package
#' 
#' Options including whether to use a progress bar when knitting a document, and
#' the base directory of images, etc.
#' @references Usage: \url{http://yihui.name/knitr/objects}
#'   
#' A list of available options:
#' \url{http://yihui.name/knitr/options#package_options}
#' @export
#' @examples opts_knit$get('verbose'); opts_knit$set(verbose = TRUE)  # change it
opts_knit =
  new_defaults(list(progress = TRUE, verbose = FALSE,
                    out.format = NULL, child.command = 'input',
                    base.dir = NULL, base.url = NULL, child.path = '',
                    upload.fun = identity,
                    eval.after = NULL, concordance = FALSE,
                    all.patterns = all_patterns, tangle = FALSE,
                    child = FALSE, parent = FALSE,
                    cache.extra = NULL, aliases = NULL,
                    
                    header = c(highlight = '', tikz = '', framed = ''))
               )
## header should not be set by hand unless you know what you are doing

## tangle: whether I'm in tangle mode; child: whether I'm in child
## document mode; parent: whether I need to add parent preamble to the
## child output
