#' @include utils.R

new_defaults = function(value = list()) {
  defaults = value

  get = function(name, default = FALSE, drop = TRUE) {
    if (default) defaults = value  # this is only a local version
    if (missing(name)) defaults else {
      if (drop && length(name) == 1) defaults[[name]] else {
        setNames(defaults[name], name)
      }
    }
  }
  resolve = function(...) {
    dots = list(...)
    if (length(dots) == 0) return()
    if (is.null(names(dots)) && length(dots) == 1 && is.list(dots[[1]]))
      if (length(dots <- dots[[1]]) == 0) return()
    dots
  }
  set = function(...) {
    dots = resolve(...)
    if (length(dots)) defaults <<- merge(dots)
    invisible(NULL)
  }
  merge = function(values) merge_list(defaults, values)
  restore = function(target = value) defaults <<- target
  append = function(...) {
    dots = resolve(...)
    for (i in names(dots)) dots[[i]] <- c(defaults[[i]], dots[[i]])
    if (length(dots)) defaults <<- merge(dots)
    invisible(NULL)
  }

  list(get = get, set = set, append = append, merge = merge, restore = restore)
}

#' Default and current chunk options
#'
#' Options for R code chunks. When running R code, the object \code{opts_chunk}
#' (default options) is not modified by chunk headers (local chunk options are
#' merged with default options), whereas \code{opts_current} (current options)
#' changes with different chunk headers and it always reflects the options for
#' the current chunk.
#'
#' Normally we set up the global options once in the first code chunk in a
#' document using \code{opts_chunk$set()}, so that all \emph{latter} chunks will
#' use these options. Note the global options set in one chunk will not affect
#' the options in this chunk itself, and that is why we often need to set global
#' options in a separate chunk.
#'
#' See \code{str(knitr::opts_chunk$get())} for a list of default chunk options.
#' @references Usage: \url{https://yihui.name/knitr/objects/}
#'
#'   A list of available options:
#'   \url{https://yihui.name/knitr/options/#chunk_options}
#' @note \code{opts_current} is read-only in the sense that it does nothing if
#'   you call \code{opts_current$set()}; you can only query the options via
#'   \code{opts_current$get()}.
#' @export
#' @examples opts_chunk$get('prompt'); opts_chunk$get('fig.keep')
opts_chunk = new_defaults(list(

  eval = TRUE, echo = TRUE, results = 'markup', tidy = FALSE, tidy.opts = NULL,
  collapse = FALSE, prompt = FALSE, comment = '##', highlight = TRUE,
  strip.white = TRUE, size = 'normalsize', background = '#F7F7F7',

  cache = FALSE, cache.path = 'cache/', cache.vars = NULL, cache.lazy = TRUE,
  dependson = NULL, autodep = FALSE, cache.rebuild = FALSE,

  fig.keep = 'high', fig.show = 'asis', fig.align = 'default', fig.path = 'figure/',
  dev = NULL, dev.args = NULL, dpi = 72, fig.ext = NULL, fig.width = 7, fig.height = 7,
  fig.env = 'figure', fig.cap = NULL, fig.scap = NULL, fig.lp = 'fig:', fig.subcap = NULL,
  fig.pos = '', out.width = NULL, out.height = NULL, out.extra = NULL, fig.retina = 1,
  external = TRUE, sanitize = FALSE, interval = 1, aniopts = 'controls,loop',

  warning = TRUE, error = TRUE, message = TRUE, package.startup.message = FALSE,

  render = NULL,

  ref.label = NULL, child = NULL, engine = 'R', split = FALSE, include = TRUE, purl = TRUE

))

#' @rdname opts_chunk
#' @export
opts_current = new_defaults()

#' @include plot.R

# a list of options attributes for RStudio
opts_chunk_attr = local({
  opts = lapply(opts_chunk$get(), class)
  opts[opts == 'NULL'] = 'character'
  opts$results = list('markup', 'asis', 'hold', 'hide')
  opts$fig.show = list('asis', 'hold', 'animate', 'hide')
  opts$fig.keep = list('high', 'none', 'all', 'first', 'last')
  opts$fig.align = list('default', 'left', 'right', 'center')
  opts$fig.showtext = 'logical'
  # quartz_ devices are for OS X only
  opts$dev = names(auto_exts)
  if (Sys.info()[['sysname']] != 'Darwin')
    opts$dev = grep('^quartz_', opts$dev, value = TRUE, invert = TRUE)
  if (.Platform$OS.type != 'windows')
    opts$dev = setdiff(opts$dev, 'win.metafile')
  opts$dev = as.list(opts$dev)
  opts$fig.ext = as.list(unique(auto_exts))
  opts$external = opts$sanitize = NULL  # hide these two rare options
  opts$fig.process = 'function'
  opts$fig.asp = 'numeric'
  opts$R.options = 'list'
  opts$cache.comments = 'logical'
  opts
})

#' Set aliases for chunk options
#'
#' We do not have to use the chunk option names given in \pkg{knitr}; we can set
#' aliases for them. The aliases are a named character vector; the names are
#' aliases and the elements in this vector are the real option names.
#' @param ... Named arguments. Argument names are aliases, and argument values
#'   are real option names.
#' @return \code{NULL}. \code{opts_knit$get('aliases')} is modified as the side effect.
#' @export
#' @examples set_alias(w = 'fig.width', h = 'fig.height')
#' # then we can use options w and h in chunk headers instead of fig.width and fig.height
set_alias = function(...) {
  opts_knit$set(aliases = c(...))
}

#' Options for the knitr package
#'
#' Options including whether to use a progress bar when knitting a document, and
#' the base directory of images, etc.
#'
#' Besides the standard usage (\code{opts_knit$set()}), we can also set package
#' options prior to loading \code{knitr} or calling \code{knit()} using
#' \code{\link{options}()} in base R. A global option \code{knitr.package.foo}
#' in \code{options()} will be set as an option \code{foo} in \code{opts_knit},
#' i.e. global options in base R with the prefix \code{knitr.package.}
#' correspond to options in \code{opts_knit}. This can be useful to set package
#' options in \file{~/.Rprofile} without loading \pkg{knitr}.
#'
#' See \code{str(knitr::opts_knit$get())} for a list of default package options.
#' @references Usage: \url{https://yihui.name/knitr/objects/}
#'
#'   A list of available options:
#'   \url{https://yihui.name/knitr/options/#package_options}
#' @export
#' @examples opts_knit$get('verbose'); opts_knit$set(verbose = TRUE)  # change it
#' if (interactive()) {
#' # for unnamed chunks, use 'fig' as the figure prefix
#' opts_knit$set(unnamed.chunk.label='fig')
#' knit('001-minimal.Rmd') # from https://github.com/yihui/knitr-examples
#' }
#' @include hooks-html.R
opts_knit = new_defaults(list(
  progress = TRUE, verbose = FALSE, width = 75L, eval.after = 'fig.cap',
  base.dir = NULL, base.url = NULL, root.dir = NULL, child.path = '',
  upload.fun = identity, animation.fun = hook_ffmpeg_html,
  global.device = FALSE, global.par = FALSE,
  concordance = FALSE, documentation = 1L, self.contained = TRUE,
  unnamed.chunk.label = 'unnamed-chunk', highr.opts = NULL,

  # internal options; users should not touch them
  out.format = NULL, child = FALSE, parent = FALSE, tangle = FALSE, aliases = NULL,
  header = c(highlight = '', tikz = '', framed = ''), global.pars = NULL
))
# tangle: whether I'm in tangle mode; child: whether I'm in child document mode;
# parent: whether I need to add parent preamble to the child output

# you may modify these options in options(knitr.package.foo)
opts_knit_names = c(
  'progress', 'verbose', 'width', 'upload.fun', 'animation.fun', 'global.device',
  'eval.after', 'concordance', 'documentation', 'aliases', 'self.contained',
  'unnamed.chunk.label'
)
# adjust opts_chunk and opts_knit according to options(), e.g.
# options(knitr.package.progress = FALSE) --> opts_knit$set(progress = FALSE),
# and options(knitr.chunk.tidy) --> opts_chunk$set(tidy = TRUE); this makes it
# possible to set options in ~/.Rprofile without loading knitr
adjust_opts_knit = function() {
  # begin_hack: R CMD build does not evaluate .Rprofile, but I need a way to
  # modify opts_chunk just for myself
  if (nzchar(opts <- Sys.getenv('R_KNITR_OPTIONS')))
    eval(parse_only(sprintf('base::options(%s)', opts)), envir = globalenv())
  # end_hack
  opts = options()
  nms = names(opts)
  if (length(nms <- grep('^knitr[.]', nms, value = TRUE)) == 0) return()
  opts = opts[nms]
  # for backward compatibility
  i = grep('^knitr[.](package|chunk)[.]', nms, invert = TRUE)
  i = intersect(i, which(nms[i] %in% paste('knitr', opts_knit_names, sep = '.')))
  if (length(i)) {
    nms.pkg = sub('^knitr.', 'knitr.package.', nms[i])
    warning2(
      'These options must be renamed (from left to right):\n',
      formatUL(sprintf('%s => %s', nms[i], nms.pkg)), immediate. = TRUE
    )
    Sys.sleep(10)
    names(opts)[i] = nms[i] = nms.pkg
  }
  # strip off knitr.chunk from option names and set chunk options
  i = grep('^knitr[.]chunk[.]', nms)
  opts_chunk$set(setNames(opts[i], sub('^knitr[.]chunk[.]', '', nms[i])))
  # similarly for knitr.package.options
  i = grep('^knitr[.]package[.]', nms)
  opts_knit$set(setNames(opts[i], sub('^knitr[.]package[.]', '', nms[i])))
}

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
