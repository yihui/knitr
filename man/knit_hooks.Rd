\docType{data}
\name{hooks}
\alias{hooks}
\title{Hooks for R code chunks, inline R code and output}
\format{List of 4
 $ get    :function (name, default = FALSE)  
 $ set    :function (...)  
 $ merge  :function (values)  
 $ restore:function ()}
\usage{
hooks
}
\description{
  A hook is a function of a pre-defined form (arguments)
  that takes values of arguments and returns desired
  output. The object \code{hooks} is used to access or set
  hooks in this package.
}
\examples{
hooks$get("source")
hooks$get("inline")
}
\references{
  Usage: \url{http://yihui.github.com/knitr/objects}

  Components in \code{kpat}:
  \url{http://yihui.github.com/knitr/hooks}
}
\keyword{dataset}
