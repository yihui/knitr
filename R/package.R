#' A general-purpose tool for dynamic report generation in R
#'
#' This is an alternative tool to Sweave with more flexible design and new
#' features like cache and fine control of graphics. It is not limited to LaTeX
#' and is ready to be customized to process other file formats. See the package
#' website in the references for more information and examples.
#' @docType package
#' @name knitr-package
#' @aliases knitr
#' @author Yihui Xie <\url{http://yihui.name}>
#' @seealso The core function in this package: \code{\link{knit}}. If you are an
#'   Sweave user, see \code{\link{Sweave2knitr}} on how to convert Sweave files
#'   to \pkg{knitr}.
#' @note The pronunciation of \pkg{knitr} is similar to \emph{neater} (neater
#'   than what?) or you can think of \emph{knitter} (but it is \emph{single t}).
#'   The name comes from \code{knit} + \code{R} (while \code{Sweave} = \code{S}
#'   + \code{weave}).
#' @references Full documentation and demos: \url{http://yihui.name/knitr/};
#'   FAQ's: \url{https://github.com/yihui/knitr/blob/master/FAQ.md}
#' @importFrom evaluate evaluate is.recordedplot is.source is.warning is.error
#'   is.message
#' @importFrom formatR tidy.source
#' @import stringr
NULL

.knitEnv = new.env()
