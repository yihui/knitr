#' A wrapper for rst2pdf
#'
#' Convert reST to PDF using \command{rst2pdf} (which converts from rst to PDF
#' using the ReportLab open-source library).
#' @param input the input rst file
#' @param command a character string which gives the path of the
#'   \command{rst2pdf} program (if it is not in PATH, the full path has to be
#'   given)
#' @param options extra command line options, e.g. \code{'-o foo.pdf -v'}
#' @author Alex Zvoleff
#' @export
#' @seealso \code{\link{knit2pdf}}
#' @references \url{http://rst2pdf.ralsina.com.ar/}
rst2pdf = function(input, command = 'rst2pdf', options = '') {
  system2(command, paste(input, options))
}

#' Convert Rnw or Rrst files to PDF using knit() and texi2pdf() or rst2pdf()
#'
#' Knit the input Rnw or Rrst document, and compile to PDF using \code{texi2pdf}
#' or \code{rst2pdf}.
#' @inheritParams knit
#' @param compiler a character string which gives the LaTeX program used to
#'   compile the tex document to PDF (by default it uses the default setting of
#'   \code{\link[tools]{texi2pdf}}, which is often PDFLaTeX); this argument will
#'   be used to temporarily set the environmental variable \samp{PDFLATEX}. For
#'   an Rrst file, setting compiler to \code{'rst2pdf'} will use
#'   \code{\link{rst2pdf}} to compiles the rst file to PDF using the ReportLab
#'   open-source library.
#' @param ... options to be passed to \code{\link[tools]{texi2pdf}} or
#'   \code{\link{rst2pdf}}
#' @author Ramnath Vaidyanathan, Alex Zvoleff and Yihui Xie
#' @export
#' @importFrom tools texi2pdf
#' @seealso \code{\link{knit}}, \code{\link[tools]{texi2pdf}},
#'   \code{\link{rst2pdf}}
#' @examples ## compile with xelatex
#'
#' ## knit2pdf(..., compiler = 'xelatex')
#'
#' ## compile a reST file with rst2pdf
#'
#' ## knit2pdf(..., compiler = 'rst2pdf')
knit2pdf = function(input, output = NULL, compiler = NULL, ..., envir = parent.frame()) {
  out = knit(input, output, envir = envir)
  owd = setwd(dirname(out)); on.exit(setwd(owd))
  if (!is.null(compiler)) {
    if (compiler == 'rst2pdf') {
      if (tolower(file_ext(out)) != 'rst') stop('for rst2pdf compiler input must be a .rst file')
      return(rst2pdf(basename(out), ...))
    } else {
      # use the specified PDFLATEX command
      oc = Sys.getenv('PDFLATEX')
      on.exit(Sys.setenv(PDFLATEX = oc), add = TRUE)
      Sys.setenv(PDFLATEX = compiler)
    }
  }
  texi2pdf(basename(out), ...)
}

#' Convert markdown to HTML using knit() and markdownToHTML()
#'
#' This is a convenience function to knit the input markdown source and call
#' \code{\link[markdown]{markdownToHTML}()} in the \pkg{markdown} package to
#' convert the result to HTML.
#' @inheritParams knit
#' @param ... options passed to \code{\link[markdown]{markdownToHTML}}
#' @export
#' @seealso \code{\link{knit}}, \code{\link[markdown]{markdownToHTML}}
#' @return If the argument \code{text} is NULL, a character string (HTML code)
#'   is returned; otherwise the result is written into a file and \code{NULL} is
#'   returned.
#' @examples # a minimal example
#' writeLines(c("# hello markdown", '```{r hello-random, echo=TRUE}', 'rnorm(5)', '```'), 'test.Rmd')
#' if (require('markdown')) {knit2html('test.Rmd')
#' if (interactive()) browseURL('test.html')}
knit2html = function(input, ..., text = NULL, envir = parent.frame()){
  if (!has_package('markdown'))
    return(warning('the package markdown is not available'))
  if (is.null(text)) {
    out = knit(input, envir = envir)
    markdown::markdownToHTML(out, str_c(file_path_sans_ext(out), '.html'), ...)
  } else {
    out = knit(text = text, envir = envir)
    markdown::markdownToHTML(text = out, ...)
  }
}
