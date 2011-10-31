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
    }
    merge = function(values) modifyList(defaults, values)
    restore = function() defaults <<- value

    list(get = get, set = set, merge = merge, restore = restore)
}

##' Chunk options
##'
##' Options for R code chunks.
##' @references Usage: \url{http://yihui.github.com/knitr/objects}
##'
##' A list of available options: \url{http://yihui.github.com/knitr/options#chunk_options}
##' @export
##' @examples optc$get('prompt'); optc$get('fig.hold')
optc =
    new_defaults(list(eval = TRUE, echo = TRUE, results = TRUE, tidy = TRUE,
                      cache = FALSE, purge = FALSE, ref = NULL, prefix.string = '',
                      prompt = FALSE, comment = '##', fig = FALSE, fig.ext = NULL,
                      fig.low = FALSE, dev = 'pdf', dpi = 72, width = 7, height = 7,
                      out.width = NULL, out.height = NULL,
                      resize.width = NULL, resize.height = NULL, align = 'default',
                      external = FALSE, sanitize = FALSE,
                      fig.hold = FALSE, fig.last = FALSE, highlight = TRUE,
                      warning = TRUE, error = TRUE, message = TRUE,
                      background = '.97;.97;.97'))

opts_chunk = optc

## all built-in patterns
.all.patterns =

    list(`rnw` = list(chunk.begin = '^<<(.*)>>=', chunk.end = '^@\\s*$',
         inline.code = '\\\\Sexpr\\{([^\\}]*)\\}',
         global.options = '\\\\SweaveOpts\\{([^\\}]*)\\}',
         header.begin = '\n*\\s*\\\\documentclass[^\\}]+\\}',
         document.begin = '\n*\\s*\\\\begin\\{document\\}'),

         `brew` = list(inline.code = '<%[=]{0,1}\\s+(.*)[\\s-]+%>'),

         `tex` = list(chunk.begin = '^%+\\s*begin.rcode\\s*(.*)',
         chunk.end = '^%+\\s*end.rcode', chunk.code = '^%+',
         global.options = '%+\\s*roptions\\s*([^\n]*)',
         inline.code = '\\\\rinline\\{([^\\}]*)\\}',
         header.begin = '\n*\\s*\\\\documentclass[^\\}]+\\}',
         document.begin = '\n*\\s*\\\\begin\\{document\\}'),

         `html` = list(chunk.begin = '^<!--\\s*begin.rcode\\s*(.*)',
         chunk.end = '^\\s*end.rcode\\s*-->',
         inline.code = '<!--\\s*rinline\\s*(.*)\\s*-->',
         global.options = '<!--\\s*roptions\\s*(.*)\\s*-->',
         header.begin = '\n*\\s*<head>'))

##' Options for the knitr package
##'
##' Options including whether to use a progress bar when knitting a
##' document, and the cache directory, etc.
##' @references Usage: \url{http://yihui.github.com/knitr/objects}
##'
##' A list of available options: \url{http://yihui.github.com/knitr/options#package_options}
##' @export
##' @examples optk$get('verbose'); optk$set(verbose = TRUE)  # change it
optk =
    new_defaults(list(progress = TRUE, verbose = FALSE,
                      render.to = NULL,
                      cache.dir = 'cache', base.dir = NULL, base.url = NULL,

                      all.patterns = .all.patterns,

                      header = c(highlight = '', tikz = '', framed = ''))
)
## header should not be set by hand unless you know what you are doing

opts_knit = optk  # I feel safter to use this longer name internally

