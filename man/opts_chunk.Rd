\docType{data}
\name{opts_chunk}
\alias{opts_chunk}
\title{Chunk options}
\format{List of 4
 $ get    :function (name, default = FALSE)  
 $ set    :function (...)  
 $ merge  :function (values)  
 $ restore:function ()}
\usage{
opts_chunk
}
\description{
  Options for R code chunks.
}
\examples{
opts_chunk$get("prompt")
opts_chunk$get("fig.hold")
}
\references{
  Usage: \url{http://yihui.github.com/knitr/objects}

  A list of available options:
  \url{http://yihui.github.com/knitr/options#chunk_options}
}
\keyword{datasets}
