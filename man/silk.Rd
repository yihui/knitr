\name{silk}
\alias{silk}
\title{Convert an R script to a literate programming document}
\usage{
silk(input, purse = TRUE, format = c("Rmd", "Rnw", "Rhtml", "Rtex", "Rrst"), 
    doc = "^#+\\s*'")
}
\arguments{
  \item{input}{the path to the R script}

  \item{purse}{logical: whether to compile the document
  after conversion}

  \item{format}{character: the output format (it takes five
  possible values); the default is R Markdown}

  \item{doc}{a regular expression to identify the
  documentation lines; by default it follows the roxygen
  convention, but it can be customized, e.g.  if you want
  to use \code{##} to denote documentation, you can use
  \code{'^##\\s*'}}
}
\value{
  The path of the literate programming document.
}
\description{
  This function takes a specially formatted R script and
  converts it to a literate programming document. By
  default normal text (documentation) should be written
  after the roxygen comment (\code{#'}) and code chunk
  options are written after \code{#+} or \code{#-}.
}
\note{
  The function name came from this idiom: make a silk purse
  out of a sow's ear (R script).
}
\examples{
#' write normal text like this and chunk options like below + label, opt=value

(s = system.file("examples", "knitr-silk.R", package = "knitr"))
silk(s)  # default markdown
o = silk(s, purse = FALSE)  # convert only; do not make a purse yet
knit2html(o)  # compile to HTML

# other formats
silk(s, FALSE, format = "Rnw")  # you need to write documentclass after #'
silk(s, FALSE, format = "Rhtml")
silk(s, FALSE, format = "Rtex")
silk(s, FALSE, format = "Rrst")
}
\author{
  Yihui Xie, with the original idea from Richard FitzJohn
  (who named it as \code{sowsear()})
}
\references{
  \url{http://yihui.name/knitr/demo/stitch/}
}
\seealso{
  \code{\link{stitch}} (feed a template with an R script)
}
