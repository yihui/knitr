\docType{data}
\name{knit_hooks}
\alias{knit_hooks}
\title{Hooks for R code chunks, inline R code and output}
\format{List of 4
 $ get    :function (name, default = FALSE)  
 $ set    :function (...)  
 $ merge  :function (values)  
 $ restore:function ()}
\usage{
knit_hooks
}
\description{
  A hook is a function of a pre-defined form (arguments)
  that takes values of arguments and returns desired
  output. The object \code{knit_hooks} is used to access or
  set hooks in this package.
}
\examples{
knit_hooks$get("source")
knit_hooks$get("inline")
}
\references{
  Usage: \url{http://yihui.github.com/knitr/objects}

  Components in \code{knit_hooks}:
  \url{http://yihui.github.com/knitr/hooks}
}
\keyword{datasets}
