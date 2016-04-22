# format a single inline object
.inline.hook = function(x) {
  if (is.numeric(x)) x = round(x, getOption('digits'))
  paste(as.character(x), collapse = ', ')
}
.out.hook = function(x, options) x
.plot.hook = function(x, options) paste(x, collapse = '.')

.default.hooks = list(
  source = .out.hook, output = .out.hook, warning = .out.hook,
  message = .out.hook, error = .out.hook, plot = .plot.hook,
  inline = .inline.hook, chunk = .out.hook, text = identity,
  evaluate = evaluate::evaluate, document = identity
)

#' Hooks for R code chunks, inline R code and output
#'
#' A hook is a function of a pre-defined form (arguments) that takes values of
#' arguments and returns desired output. The object \code{knit_hooks} is used to
#' access or set hooks in this package.
#' @export
#' @references Usage: \url{http://yihui.name/knitr/objects}
#'
#' Components in \code{knit_hooks}: \url{http://yihui.name/knitr/hooks}
#' @examples knit_hooks$get('source'); knit_hooks$get('inline')
knit_hooks = new_defaults(.default.hooks)

render_brew = function() NULL

# the chunk option out.lines = n (first n rows), -n (last n rows), or c(n1, n2)
# (first n1 and last n2 rows)
hook_suppress = function(x, options) {
  n = options$out.lines
  if (length(n) == 0 || !is.numeric(n) || length(n) > 2) return(x)
  x = split_lines(x)
  m = length(x)
  if (length(n) == 1) {
    if (m > abs(n)) {
      x = if (n >= 0) c(head(x, n), '....') else c('....', tail(x, -n))
    }
  } else {
    if (m > sum(n)) x = c(head(x, n[1]), '....', tail(x, n[2]))
  }
  paste(x, collapse = '\n')
}

#' Hooks for code chunk options
#'
#' Like \code{\link{knit_hooks}}, this object can be used to set hook functions
#' to manipulate chunk options.
#'
#' For every code chunk, if the chunk option named, say, \code{FOO}, is not
#' \code{NULL}, and a hook function with the same name has been set via
#' \code{opts_hooks$set(FOO = function(options) { options })} (you can manipuate
#' the \code{options} argument in the function and return it), the hook function
#' will be called to update the chunk options.
#' @references \url{http://yihui.name/knitr/hooks}
#' @export
#' @examples # make sure the figure width is no smaller than fig.height
#' opts_hooks$set(fig.width = function(options) {
#'   if (options$fig.width < options$fig.height) {
#'     options$fig.width = options$fig.height
#'   }
#'   options
#' })
#' # remove all hooks
#' opts_hooks$restore()
opts_hooks = new_defaults(list())
