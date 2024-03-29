% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parser.R
\docType{data}
\name{knit_code}
\alias{knit_code}
\title{The code manager to manage code in all chunks}
\format{
An object of class \code{list} of length 7.
}
\usage{
knit_code
}
\description{
This object provides methods to manage code (as character vectors) in all
chunks in \pkg{knitr} source documents. For example,
\code{knitr::knit_code$get()} returns a named list of all code chunks (the
names are chunk labels), and \code{knitr::knit_code$get('foo')} returns the
character vector of the code in the chunk with the label \code{foo}.
}
\note{
The methods on this object include the \code{set()} method (i.e., you
  could do something like \code{knitr::knit_code$set(foo = "'my precious new
  code'")}), but we recommend that you do not use this method to modify the
  content of code chunks, unless you are
  \href{https://emitanaka.rbind.io/post/knitr-knitr-code/}{as creative as Emi
  Tanaka} and know what you are doing.
}
\keyword{datasets}
