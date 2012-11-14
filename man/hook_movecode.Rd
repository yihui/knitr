\name{hook_movecode}
\alias{hook_movecode}
\title{A document hook function to move code out of floating environments}
\usage{
hook_movecode(x)
}
\arguments{
  \item{x}{a character string (the content of the whole
  document output)}
}
\value{
  The post-processed document as a character string.
}
\description{
  This is a document hook to move code chunks out of LaTeX
  floating environments like \samp{figure} and \samp{table}
  when the chunks were actually written inside the floats.
}
\details{
  This function is primarily designed for LyX: we often
  insert code chunks into floats to generate figures or
  tables, but in the final output we do not want the code
  to float with the environments, so we use regular
  expressions to find out the floating environments,
  extract the code chunks and move them out.
}
\note{
  This function is hackish. It assumes you to use the
  default output hooks for LaTeX (not Sweave or listings),
  and every figure/table environment must have a label.
}
\examples{
\dontrun{
knit_hooks$set(document = hook_movecode)
}
}
