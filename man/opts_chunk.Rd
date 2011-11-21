\docType{data}
\name{optc}
\alias{optc}
\title{Chunk options}
\format{List of 4
 $ get    :function (name, default = FALSE)  
 $ set    :function (...)  
 $ merge  :function (values)  
 $ restore:function ()}
\usage{
optc
}
\description{
  Options for R code chunks.
}
\examples{
optc$get("prompt")
optc$get("fig.hold")
}
\references{
  Usage: \url{http://yihui.github.com/knitr/objects}

  A list of available options:
  \url{http://yihui.github.com/knitr/options#chunk_options}
}
\keyword{dataset}
