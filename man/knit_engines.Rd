\docType{data}
\name{knit_engines}
\alias{knit_engines}
\title{Engines of other languages}
\format{List of 4
 $ get    :function (name, default = FALSE, drop = TRUE)  
 $ set    :function (...)  
 $ merge  :function (values)  
 $ restore:function (target = value)}
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

  In most cases, \code{options$engine} can be directly used
  in command line to execute the code, e.g. \code{python}
  or \code{ruby}, but sometimes we may want to specify the
  path of the engine program, in which case we can pass it
  through the \code{engine.path} option. For example,
  \code{engine='ruby', engine.path='/usr/bin/ruby1.9.1'}.
  Additional command line arguments can be passed through
  \code{options$engine.opts}, e.g. \code{engine='ruby',
  engine.opts='-v'}.
}
\examples{
knit_engines$get("python")
knit_engines$get("awk")
names(knit_engines$get())
}
\references{
  Usage: \url{http://yihui.name/knitr/objects}
}
\keyword{datasets}
