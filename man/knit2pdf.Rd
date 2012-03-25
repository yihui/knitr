\name{knit2pdf}
\alias{knit2pdf}
\title{Convert Rnw to PDF using knit and texi2pdf}
\usage{
knit2pdf(input, output = NULL, compiler = NULL)
}
\arguments{
  \item{compiler}{a character string which gives the LaTeX
  program used to compile the tex document to PDF (by
  default it uses the default setting of
  \code{\link[tools]{texi2pdf}}, which is often PDFLaTeX);
  this argument will be used to temporarily set the
  environmental variable \samp{PDFLATEX}}

  \item{input}{path of the input file}

  \item{output}{path of the output file; if \code{NULL},
  this function will try to guess and it will be under the
  current working directory}
  
  \item{...}{options to be passed to texi2pdf.}
}
\description{
  Knit the input Rnw document to a tex document, and
  compile it using \code{texi2pdf}.
}
\examples{
## compile with xelatex
## knit2pdf(..., compiler = 'xelatex')
}
\author{
  Ramnath Vaidyanathan and Yihui Xie
}
\seealso{
  \code{\link{knit}}, \code{\link[tools]{texi2pdf}}
}
