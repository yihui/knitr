\docType{data}
\name{opts_chunk}
\alias{opts_chunk}
\alias{opts_current}
\title{Default and current chunk options}
\format{List of 4
 $ get    :function (name, default = FALSE)  
 $ set    :function (...)  
 $ merge  :function (values)  
 $ restore:function ()}
\usage{
opts_chunk

opts_current
}
\description{
  Options for R code chunks. When running R code, the
  object \code{opts_chunk} (default options) is not
  modified by chunks (local chunk options are merged with
  default options), whereas \code{opts_current} (current
  options) changes with different chunks.
}
\examples{
opts_chunk$get("prompt")
opts_chunk$get("fig.keep")
}
\references{
  Usage: \url{http://yihui.github.com/knitr/objects}

  A list of available options:
  \url{http://yihui.github.com/knitr/options#chunk_options}
}
\keyword{datasets}
