\docType{data}
\name{knit_engines}
\alias{knit_engines}
\title{Engines of other languages}
\format{List of 4
 $ get    :function (name, default = FALSE)  
 $ set    :function (...)  
 $ merge  :function (values)  
 $ restore:function ()}
\usage{
knit_engines
}
\description{
  This object controls how to execute the code from
  languages other than R (when the chunk option
  \code{engine} is not \code{'R'}). Each component in this
  object is a function that takes a list of current chunk
  options (including the source code) and returns a
  character string to be written into the output.
}
\examples{
knit_engines$get("python")
knit_engines$get("awk")
}
\references{
  Usage: \url{http://yihui.name/knitr/objects}
}
\keyword{datasets}
