\name{vignette_engines}
\alias{vignette_engines}
\title{Package vignette engines}
\description{
Since R 3.0.0, package vignettes can use non-Sweave engines, and \pkg{knitr}
has provided a few engines to compile vignettes via \code{\link{knit}()} with
different templates. See \url{http://yihui.name/knitr/demo/vignette/} for
more information.
}
\examples{
library(knitr)
vig_list = if (getRversion() > "3.0.0") tools::vignetteEngine(package = "knitr")
str(vig_list)
vig_list[["knitr::knitr"]][c("weave", "tangle")]
vig_list[["knitr::knitr_notangle"]][c("weave", "tangle")]
vig_list[["knitr::docco_classic"]][c("weave", "tangle")]
}
