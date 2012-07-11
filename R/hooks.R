## format a single inline object
.inline.hook = function(x) {
  if (is.numeric(x)) x = round(x, getOption('digits'))
  paste(as.character(x), collapse = ', ')
}
.out.hook = function(x, options) x
.plot.hook = function(x, options) paste(x, collapse = '.')

.default.hooks = list(
  source = .out.hook, output = .out.hook, warning = .out.hook,
  message = .out.hook, error = .out.hook, plot = .plot.hook,
  inline = .inline.hook, chunk = .out.hook, document = identity
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
