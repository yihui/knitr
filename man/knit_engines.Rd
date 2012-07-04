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
\details{
  The engine function has one argument \code{options}: the
  source code of the current chunk is in
  \code{options$code}. Usually we can call external
  programs to run the code via \code{\link[base]{system}}.
  Other chunk options are also contained in this argument,
  e.g. \code{options$echo} and \code{options$eval}, etc.
}
\examples{
knit_engines$get("python")
knit_engines$get("awk")
}
\references{
  Usage: \url{http://yihui.name/knitr/objects}
}
\keyword{datasets}
