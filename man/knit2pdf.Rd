\name{knit2pdf}
\alias{knit2pdf}
\title{Convert Rnw to PDF using knit and texi2pdf}
\usage{
knit2pdf(input, output = NULL)
}
\arguments{
  \item{input}{path of the input file}

  \item{output}{path of the output file; if \code{NULL},
  this function will try to guess and it will be under the
  same directory as \code{input}}
}
\description{
  Knit the input Rnw document to a tex document, and
  compile it using \code{texi2pdf}.
}
\author{
  Ramnath Vaidyanathan and Yihui Xie
}
\seealso{
  \code{\link{knit}}, \code{\link[tools]{texi2pdf}}
}
