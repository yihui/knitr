\name{knit_env}
\alias{knit_env}
\title{The environment in which a code chunk is evaluated}
\usage{
knit_env()
}
\description{
  This function makes the environment of a code chunk
  accessible inside a chunk.
}
\details{
  In some special cases, we need access to the environment
  of the current chunk; a typical example is when we use
  \code{source()} in a cached chunk, we have to make sure
  the script is executed in the correct environment (should
  not use the default local environment). See references
  for an example.
}
\references{
  \url{http://yihui.name/knitr/demo/cache/}
}
