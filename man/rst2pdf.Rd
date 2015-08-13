% Please edit documentation in R/utils-conversion.R
\name{rst2pdf}
\alias{rst2pdf}
\title{A wrapper for rst2pdf}
\usage{
rst2pdf(input, command = "rst2pdf", options = "")
}
\arguments{
\item{input}{the input rst file}

\item{command}{a character string which gives the path of the
\command{rst2pdf} program (if it is not in PATH, the full path has to be
given)}

\item{options}{extra command line options, e.g. \code{'-v'}}
}
\value{
An input file \file{*.rst} will produce \file{*.pdf} and this output
  filename is returned if the conversion was successful.
}
\description{
Convert reST to PDF using \command{rst2pdf} (which converts from rst to PDF
using the ReportLab open-source library).
}
\author{
Alex Zvoleff and Yihui Xie
}
\references{
\url{https://github.com/rst2pdf/rst2pdf}
}
\seealso{
\code{\link{knit2pdf}}
}
