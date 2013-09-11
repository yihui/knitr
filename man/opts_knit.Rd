\docType{data}
\name{opts_knit}
\alias{opts_knit}
\title{Options for the knitr package}
\format{List of 4
 $ get    :function (name, default = FALSE, drop = TRUE)  
 $ set    :function (...)  
 $ merge  :function (values)  
 $ restore:function (target = value)}
\usage{
opts_knit
}
\description{
  Options including whether to use a progress bar when
  knitting a document, and the base directory of images,
  etc.
}
\details{
  Besides the standard usage (\code{opts_knit$set()}), we
  can also set package options prior to loading
  \code{knitr} or calling \code{knit()} using
  \code{\link{options}()} in base R. A global option
  \code{knitr.foo} in \code{options()} will be set as an
  option \code{foo} in \code{opts_knit}, i.e. global
  options in base R with the prefix \code{knitr.}
  correspond to options in \code{opts_knit}. This can be
  useful to set package options in \file{~/.Rprofile}
  without loading \pkg{knitr}.
}
\examples{
opts_knit$get("verbose")
opts_knit$set(verbose = TRUE)  # change it
\dontrun{
# for unnamed chunks, use 'fig' as the figure prefix
options(knitr.unnamed.chunk.label = "fig")
knit("001-minimal.Rmd")  # from https://github.com/yihui/knitr-examples
}
}
\references{
  Usage: \url{http://yihui.name/knitr/objects}

  A list of available options:
  \url{http://yihui.name/knitr/options#package_options}
}
\keyword{datasets}
