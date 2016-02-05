% Please edit documentation in R/defaults.R
\docType{data}
\name{opts_chunk}
\alias{opts_chunk}
\alias{opts_current}
\title{Default and current chunk options}
\format{An object of class \code{list} of length 4.}
\usage{
opts_chunk

opts_current
}
\description{
Options for R code chunks. When running R code, the object \code{opts_chunk}
(default options) is not modified by chunk headers (local chunk options are
merged with default options), whereas \code{opts_current} (current options)
changes with different chunk headers and it always reflects the options for
the current chunk.
}
\details{
Normally we set up the global options once in the first code chunk in a
document using \code{opts_chunk$set()}, so that all \emph{latter} chunks will
use these options. Note the global options set in one chunk will not affect
the options in this chunk itself, and that is why we often need to set global
options in a separate chunk.

Below is a list of default chunk options, retrieved via
\code{opts_chunk$get()}:

\Sexpr[results=verbatim]{str(knitr::opts_chunk$get())}
}
\note{
\code{opts_current} is read-only in the sense that it does nothing if
  you call \code{opts_current$set()}; you can only query the options via
  \code{opts_current$get()}.
}
\examples{
opts_chunk$get("prompt")
opts_chunk$get("fig.keep")
}
\references{
Usage: \url{http://yihui.name/knitr/objects}

  A list of available options:
  \url{http://yihui.name/knitr/options#chunk_options}
}
\keyword{datasets}
