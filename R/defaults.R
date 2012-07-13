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
    if (is.null(names(dots)) && length(dots) == 1 && is.list(dots[[1]]))
      dots = dots[[1]]
    defaults <<- merge(dots)
    invisible(NULL)
  }
  merge = function(values) merge_list(defaults, values)
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
opts_chunk = new_defaults(list(
  eval = TRUE, echo = TRUE, results = 'markup', tidy = TRUE,
  cache = FALSE, dependson = NULL, cache.path = 'cache/',
  ref.label = NULL, child = NULL, engine = 'R',
  prompt = FALSE, comment = '##', autodep = FALSE,
  fig.keep = 'high', fig.show = 'asis', fig.align = 'default',
  fig.path = 'figure/', fig.ext = NULL, dev = 'pdf', dpi = 72,
  dev.args = NULL, fig.width = 7, fig.height = 7,
  fig.cap = NULL, fig.scap = NULL, fig.lp = 'fig:', fig.pos = '',
  out.width = NULL, out.height = NULL, out.extra = NULL,
  resize.width = NULL, resize.height = NULL,
  external = TRUE, sanitize = FALSE,
  highlight = TRUE, size = 'normalsize',
  warning = TRUE, error = TRUE, message = TRUE,
  background = '#F7F7F7', split = FALSE, include = TRUE,
  interval = 1, aniopts = 'controls,loop'
))

#' @rdname opts_chunk
#' @export
opts_current = new_defaults()

## a list of options attributes for RStudio
opts_chunk_attr = (function() {
  opts = lapply(opts_chunk$get(), class)
  opts[opts == 'NULL'] = 'character'
  opts$results = list('markup', 'asis', 'hide')
  opts$fig.show = list('asis', 'hold', 'animate')
  opts$fig.keep = list('high', 'none', 'all', 'first', 'last')
  opts$fig.align = list('default', 'left', 'right', 'center')
  opts$dev = as.list(names(auto_exts))
  opts$fig.ext = as.list(unique(auto_exts))
  opts
})()


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
opts_knit = new_defaults(list(
  progress = TRUE, verbose = FALSE, out.format = NULL,
  child.command = 'input', base.dir = NULL, base.url = NULL, child.path = '',
  upload.fun = identity, animation.fun = NULL,
  eval.after = NULL, concordance = FALSE, sweave.penalty = 10,
  tangle = FALSE, child = FALSE, parent = FALSE,
  cache.extra = NULL, aliases = NULL, root.dir = NULL,
  self.contained = TRUE, filter.chunk.end = TRUE,
  header = c(highlight = '', tikz = '', framed = '')
))
## header should not be set by hand unless you know what you are doing

## tangle: whether I'm in tangle mode; child: whether I'm in child
## document mode; parent: whether I need to add parent preamble to the
## child output

#' Template for creating reusable chunk options
#'
#' Creates a template binding a label to a set of chunk options. Every chunk
#' that references the template label will have the specificed set of options
#' applied to it.
#' @export
#' @examples opts_template$set(myfigures = list(fig.height = 4, fig.width = 4))
#' # later you can reuse these chunk options by 'opts.label', e.g.
#'
#' # <<foo, opts.label='myfigures'>>=
#'
#' # the above is equivalent to <<foo, fig.height=4, fig.width=4>>=
opts_template = new_defaults()
