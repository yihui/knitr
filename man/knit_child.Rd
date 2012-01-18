\name{knit_child}
\alias{knit_child}
\title{Knit a child document}
\usage{
knit_child(..., command = "input")
}
\arguments{
  \item{...}{arguments passed to \code{\link{knit}}}

  \item{command}{the LaTeX command to use (usually
  \samp{input} or \samp{include})}
}
\value{
  A character string of the form
  \samp{\command{child-doc.tex}}, of which the class is
  \samp{AsIs}, so the inline hook will not wrap it in
  \command{texttt}.
}
\description{
  This function is for LaTeX only; it knits a child
  document and returns the command to input the result into
  the main document. It is designed to be used in the
  inline R code and serves as the alternative to the
  \command{SweaveInput} command in Sweave.
}
\examples{
## you can write \\Sexpr{knit_child('child-doc.Rnw')} in
#   an Rnw file 'main.Rnw' to input child-doc.tex in
#   main.tex
}
