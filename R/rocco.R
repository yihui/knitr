#' Knit R Markdown using the classic Docco style
#'
#' The classic Docco style is a two-column layout, with text in the left and
#' code in the right column.
#'
#' The output HTML page supports resizing and hiding/showing the two columns.
#' Move the cursor to the center of the page, and it will change to a
#' bidirectional resize cursor; drag the cursor to resize the two columns. Press
#' the key \code{t} to hide the code column (show the text column only), and
#' press again to hide the text column (show code).
#' @param input Path of the input R Markdown file.
#' @param ... Arguments to be passed to \code{\link{knit2html}}
#' @return An HTML file is written, and its name is returned.
#' @author Weicheng Zhu and Yihui Xie
#' @references The Docco package by Jeremy Ashkenas:
#'   \url{https://github.com/jashkenas/docco}
#' @export
#' @examples rocco_view=function(input) {
#' owd = setwd(tempdir()); on.exit(setwd(owd))
#' if (!file.exists(input)) return()
#' o=rocco(input, quiet=TRUE)
#' if (interactive()) browseURL(o)}
#' # knit these two vignettes using the docco style
#' rocco_view(system.file('doc', 'docco-classic.Rmd', package = 'knitr'))
#' rocco_view(system.file('doc', 'knit_expand.Rmd', package = 'knitr'))
rocco = function(input, ...) {
  knit2html(
    input, ...,
    meta = list(
      css = c('@npm/@xiee/utils/css/docco-classic.min.css', '@prism-xcode'),
      js = c(
        '@npm/jquery@3.7.1/dist/jquery.min.js',
        '@npm/@xiee/utils/js/docco-classic.min.js,docco-resize.js',
        '@npm/@xiee/utils/js/center-img.min.js'
      )
    )
  )
}
