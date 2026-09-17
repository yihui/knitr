#' A general-purpose tool for dynamic report generation in R
#'
#' The \pkg{knitr} package is an implementation of Literate Programming, a
#' programming paradigm that intermingle code chunks (for computing) with prose
#' (for documentation) in the same document.
#'
#' When the document is compiled, the code chunks can be executed, and the
#' results from computing (text or graphics) are automatically written to the
#' output along with the prose.
#'
#' This package is an alternative tool to Sweave with a more flexible design and
#' new features like caching and finer control of graphics. It is not limited to
#' LaTeX and is ready to be customized to process other file formats. See the
#' package website in the references for more information and examples.
#' @name knitr-package
#' @aliases knitr
#' @import graphics grDevices stats utils
#' @author Yihui Xie <<https://yihui.org>>
#' @seealso The core function in this package: [knit()]. If you are an
#'   Sweave user, see [Sweave2knitr()] on how to convert Sweave files
#'   to \pkg{knitr}.
#' @note The pronunciation of \pkg{knitr} is similar to *neater* or you can
#'   think of *knitter* (but it is *single t*). The name comes from
#'   `knit` + `R` (while `Sweave` = `S` + `weave`).
#' @references Full documentation and demos: <https://yihui.org/knitr/>;
#'   FAQ's: <https://yihui.org/knitr/faq/>
#' @importFrom xfun file_ext html_escape is_windows loadable parse_only
#'   sans_ext try_silent with_ext read_utf8 write_utf8 file_string
#'   is_R_CMD_check is_abs_path file_exists strip_html
'_PACKAGE'

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
