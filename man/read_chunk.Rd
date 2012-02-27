\name{read_chunk}
\alias{read_chunk}
\title{Read chunks from an external R script}
\usage{
read_chunk(path)
}
\arguments{
  \item{path}{the path to the R script}
}
\value{
  Code chunks are read into the current session so that
  future chunks can use the R code.
}
\description{
  Chunks can be put in an external R script, and this
  function reads chunks into the current \pkg{knitr}
  session.
}
\details{
  The \code{ref.label} component in the pattern list
  (\code{knit_patterns$get('ref.label')}) defines the
  format of code chunks.
}
\note{
  This function can only be used in a chunk which is
  \emph{not} cached (chunk option \code{cache = FALSE}),
  and the code is read and stored in the current session
  \emph{without} being executed (to actually run the code,
  you have to use a chunk with a corresponding label).
}
\examples{
## the default format

## @knitr my-label
1 + 1
lm(y ~ x, data = data.frame(x = 1:10, y = rnorm(10)))

## later you can use <<my-label>>= to reference this chunk
}
\references{
  \url{http://yihui.name/knitr/demo/reference/}
}
