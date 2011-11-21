\docType{data}
\name{optk}
\alias{optk}
\title{Options for the knitr package}
\format{List of 4
 $ get    :function (name, default = FALSE)  
 $ set    :function (...)  
 $ merge  :function (values)  
 $ restore:function ()}
\usage{
optk
}
\description{
  Options including whether to use a progress bar when
  knitting a document, and the cache directory, etc.
}
\examples{
optk$get("verbose")
optk$set(verbose = TRUE)  # change it
}
\references{
  Usage: \url{http://yihui.github.com/knitr/objects}

  A list of available options:
  \url{http://yihui.github.com/knitr/options#package_options}
}
\keyword{dataset}
