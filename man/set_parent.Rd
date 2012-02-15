\name{set_parent}
\alias{set_parent}
\title{Specify the parent document of child documents}
\usage{
set_parent(parent)
}
\arguments{
  \item{parent}{path to the parent document (relative to
  the current working directory)}
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
  format is LaTeX.
}
\examples{
## can use, e.g. \\Sexpr{set_parent('parent_doc.Rnw')}
## or use it in a chunk with echo=FALSE
}
