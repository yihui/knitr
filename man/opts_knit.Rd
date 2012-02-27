\docType{data}
\name{opts_knit}
\alias{opts_knit}
\title{Options for the knitr package}
\format{List of 4
 $ get    :function (name, default = FALSE)  
 $ set    :function (...)  
 $ merge  :function (values)  
 $ restore:function ()}
\usage{
opts_knit
}
\description{
  Options including whether to use a progress bar when
  knitting a document, and the base directory of images,
  etc.
}
\examples{
opts_knit$get("verbose")
opts_knit$set(verbose = TRUE)  # change it
}
\references{
  Usage: \url{http://yihui.name/knitr/objects}

  A list of available options:
  \url{http://yihui.name/knitr/options#package_options}
}
\keyword{datasets}
