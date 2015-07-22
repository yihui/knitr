% Please edit documentation in R/params.R
\name{knit_params.yaml}
\alias{knit_params.yaml}
\title{Extract knit parameters from YAML text}
\usage{
knit_params.yaml(yaml, evaluate = TRUE)
}
\arguments{
\item{yaml}{Character vector containing the YAML text}

\item{evaluate}{If TRUE, expression values embedded within the YAML will be
evaluated. This is the default. When FALSE, parameters defined by an
expression will have the parsed expression in its \code{value} field and
\code{"expression"} in the \code{class} field.}
}
\value{
List of objects of class \code{knit_param} that correspond to the
parameters declared in the \code{params} section of the YAML. See
\code{\link{knit_params}} for a full description of these objects.
}
\description{
This function reads the YAML front-matter that has already been extracted
from a document and returns a list of any parameters declared there.
}
\seealso{
\code{\link{knit_params}}
}
