\name{set_parent}
\alias{set_parent}
\title{Specify the parent document of child documents}
\usage{
set_parent(parent)
}
\arguments{
  \item{parent}{path to the parent document (relative to
  the current child document)}
}
\value{
  The preamble is extracted and stored to be used later
  when the complete output is written.
}
\description{
  This function extract the LaTeX preamble of the parent
  document to use for the child document, so that the child
  document can be compiled as an individual document.
}
\note{
  Obviously this function is only useful when the output
  format is LaTeX. This function only works when the child
  document is compiled in a standalone mode using
  \code{\link{knit}()} (instead of being called in
  \code{\link{knit_child}()}); when the parent document is
  compiled, this function in the child document will be
  ignored.
}
\examples{
## can use, e.g. \\Sexpr{set_parent('parent_doc.Rnw')}
## or use it in a chunk with echo=FALSE
}
\references{
  \url{http://yihui.name/knitr/demo/child/}
}
