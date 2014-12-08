% Please edit documentation in R/utils.R
\name{current_input}
\alias{current_input}
\title{Query the current input filename}
\usage{
current_input()
}
\value{
A character string, if this function is called inside an input
  document (otherwise \code{NULL}).
}
\description{
Returns the name of the input file passed to \code{\link{knit}()}.
}
