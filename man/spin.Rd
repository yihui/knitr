\name{spin}
\alias{spin}
\title{Spin goat's hair into wool}
\usage{
spin(hair, knit = TRUE, report = TRUE, format = c("Rmd", "Rnw", "Rhtml", "Rtex", 
    "Rrst"), doc = "^#+'[ ]?", envir = parent.frame())
}
\arguments{
  \item{hair}{the path to the R script}

  \item{knit}{logical: whether to compile the document
  after conversion}

  \item{report}{logical: whether to generate report for
  \file{Rmd}, \file{Rnw} and \file{Rtex} output (ignored if
  \code{knit = FALSE})}

  \item{format}{character: the output format (it takes five
  possible values); the default is R Markdown}

  \item{doc}{a regular expression to identify the
  documentation lines; by default it follows the roxygen
  convention, but it can be customized, e.g.  if you want
  to use \code{##} to denote documentation, you can use
  \code{'^##\\\\s*'}}

  \item{envir}{the environment in which the code chunks are
  to be evaluated (can use \code{\link{new.env}()} to
  guarantee an empty new environment)}
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
\details{
  Obviously the goat's hair is the original R script, and
  the wool is the literate programming document (ready to
  be knitted).
}
\note{
  If the output format is Rnw and no document class is
  specified in roxygen comments, this function will
  automatically add the \code{article} class to the LaTeX
  document so that it is complete and can be compiled. You
  can always specify the document class and other LaTeX
  settings in roxygen comments manually.
}
\examples{
#' write normal text like this and chunk options like below

# + label, opt=value

(s = system.file("examples", "knitr-spin.R", package = "knitr"))
spin(s)  # default markdown
o = spin(s, knit = FALSE)  # convert only; do not make a purse yet
knit2html(o)  # compile to HTML

# other formats
spin(s, FALSE, format = "Rnw")  # you need to write documentclass after #'
spin(s, FALSE, format = "Rhtml")
spin(s, FALSE, format = "Rtex")
spin(s, FALSE, format = "Rrst")
}
\author{
  Yihui Xie, with the original idea from Richard FitzJohn
  (who named it as \code{sowsear()} which meant to make a
  silk purse out of a sow's ear)
}
\references{
  \url{http://yihui.name/knitr/demo/stitch/}
}
\seealso{
  \code{\link{stitch}} (feed a template with an R script)
}
