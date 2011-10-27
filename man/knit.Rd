\name{knit}
\alias{knit}
\title{Knit a document}
\usage{
knit(input, output, pattern)
}
\arguments{
  \item{input}{path of the input file}

  \item{output}{path of output file (note the working
  directory will be set to the directory of the input file,
  so this argument is usually a filename without a
  directory name); if not set, this function will try to
  guess}

  \item{pattern}{the name of the pattern in
  \code{optk$get('all.patterns')} to be used}
}
\value{
  The parsed document is written into the output file.
}
\description{
  This function takes an input file, extracts the R code in
  it according to a list of patterns, evaluates the code
  and writes the output in another file.
}
\examples{
library(knitr)
(f = tempfile(fileext = ".Rnw"))
file.copy(system.file("examples", "sweave-ex.Rnw", 
    package = "knitr"), f, overwrite = TRUE)
knit(f)
}
