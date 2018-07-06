#' A general-purpose tool for dynamic report generation in R
#'
#' This is an alternative tool to Sweave with a more flexible design and new
#' features like caching and finer control of graphics. It is not limited to LaTeX
#' and is ready to be customized to process other file formats. See the package
#' website in the references for more information and examples.
#' @docType package
#' @name knitr-package
#' @aliases knitr
#' @import graphics grDevices methods stats utils
#' @author Yihui Xie <\url{https://yihui.name}>
#' @seealso The core function in this package: \code{\link{knit}}. If you are an
#'   Sweave user, see \code{\link{Sweave2knitr}} on how to convert Sweave files
#'   to \pkg{knitr}.
#' @note The pronunciation of \pkg{knitr} is similar to \emph{neater} (neater
#'   than what?) or you can think of \emph{knitter} (but it is \emph{single t}).
#'   The name comes from \code{knit} + \code{R} (while \code{Sweave} = \code{S}
#'   + \code{weave}).
#' @references Full documentation and demos: \url{https://yihui.name/knitr/};
#'   FAQ's: \url{https://yihui.name/knitr/faq/}
#' @importFrom xfun file_ext isFALSE parse_only sans_ext with_ext
NULL

.knitEnv = new.env()

.knitEnv$meta = list()

# no partial matching for lists!!
#' @export
`$.knitr_strict_list` = function(x, name) x[[name]]

as.strict_list = function(x) {
  if (!is.list(x)) stop("'x' is not a list")
  class(x) = 'knitr_strict_list'
  x
}
