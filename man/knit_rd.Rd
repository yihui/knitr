\name{knit_rd}
\alias{knit_rd}
\title{Knit package documentation}
\usage{
knit_rd(pkg)
}
\arguments{
  \item{pkg}{package name}
}
\value{
  All HTML pages corresponding to topics in the package are
  written under the current working directory. An
  \file{index.html} is also written as a table of content.
}
\description{
  Run examples in a package and insert output into the
  examples code.
}
\examples{
\dontrun{
knit_rd("maps")
knit_rd("rpart")
}
}
