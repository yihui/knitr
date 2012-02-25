\name{run_chunk}
\alias{run_chunk}
\title{Run the code in a specified chunk}
\usage{
run_chunk(label, envir = parent.frame())
}
\arguments{
  \item{label}{the chunk label}

  \item{envir}{the environment in which to evaluate the
  code}
}
\value{
  Values returned by the code in the chunk.
}
\description{
  We can specify a chunk label and use this function to
  evaluate the code in this chunk. It is an alternative to
  the chunk reference in Sweave.
}
\details{
  The difference between this type of chunk reference and
  the chunk option \code{ref.label} is that the latter can
  only be used for a chunk so that it has exactly the same
  code as the reference chunk, whereas this function makes
  it possible to collect several little chunks and run them
  inside another big chunk.
}
\note{
  Recursion (must be finite, of course) of reference is
  allowed, e.g. we may run the code of \samp{chunk2} in
  \samp{chunk1}, and \samp{chunk2} also contains a
  reference to \samp{chunk3}, then if we run \samp{chunk1},
  both the code in \samp{chunk2} and \samp{chunk3} will be
  evaluated.
}
\examples{
## In Sweave we use chunk reference like this
# <<a>>=
# 1+1
# @
# <<b>>=
# <<a>>
# @

## In knitr, we can use the same, or
# <<b>>=
# run_chunk('a')
# @
}
