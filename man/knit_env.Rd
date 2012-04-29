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
  of the current chunk, e.g., to make sure the code is
  executed in the correct environment.
}
\references{
  \url{http://yihui.name/knitr/demo/cache/}
}
