\name{knit_child}
\alias{knit_child}
\title{Knit a child document}
\usage{
knit_child(..., eval = TRUE)
}
\arguments{
  \item{...}{arguments passed to \code{\link{knit}}}

  \item{eval}{logical: whether to evaluate the child
  document}
}
\value{
  A character string of the form
  \samp{\command{child-doc.tex}} or
  \code{source("child-doc.R")}, depending on the argument
  \code{tangle} passed in.
}
\description{
  This function is for LaTeX only except when it is used to
  extract R code from the document; by default it knits a
  child document and returns the command to input the
  result into the main document. It is designed to be used
  in the inline R code and serves as the alternative to the
  \command{SweaveInput} command in Sweave.
}
\details{
  The LaTeX command used to input the child document
  (usually \samp{input} or \samp{include}) is from the
  package option \code{child.command}
  (\code{opts_knit$get('child.command')}).

  When we call \code{purl()} to extract R code, the code in
  the child document is extracted and saved into an R
  script.

  The path of the child document is relative to the parent
  document.
}
\note{
  This function is not supposed be called directly like
  \code{\link{knit}()}; instead it must be placed in a
  parent document to let \code{\link{knit}()} call it
  indirectly.
}
\examples{
## you can write \\Sexpr{knit_child('child-doc.Rnw')} in
#   an Rnw file 'main.Rnw' to input child-doc.tex in
#   main.tex

## comment out the child doc by
#   \\Sexpr{knit_child('child-doc.Rnw', eval = FALSE)}

## use \\include: opts_knit$set(child.command =
#   'include')
}
\references{
  \url{http://yihui.name/knitr/demo/child/}
}
