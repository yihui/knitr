##' A general-purpose tool for dynamic report generation in R
##'
##' This is an alternative tool to Sweave with more flexible design
##' and new features like cache and fine control of graphics. It is
##' not limited to LaTeX and is ready to be customized to process
##' other file formats. See the package website in the references for
##' more information and examples.
##' @docType package
##' @name knitr-package
##' @aliases knitr
##' @author Yihui Xie <\url{http://yihui.name}>
##' @references Full documentation and demos:
##' \url{http://yihui.name/knitr/}; FAQ's:
##' \url{https://github.com/yihui/knitr/blob/master/FAQ.md}
##' @importFrom evaluate evaluate is.recordedplot is.source is.warning is.error is.message
##' @importFrom formatR tidy.source
##' @importFrom digest digest
##' @import stringr
##' @importFrom tools file_ext file_path_sans_ext
##' @importFrom highlight highlight renderer_latex renderer_html
##' styler boxes_latex css.parser styler_assistant_latex
NULL

.knitEnv = new.env()
