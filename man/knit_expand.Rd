% Please edit documentation in R/template.R
\name{knit_expand}
\alias{knit_expand}
\title{A simple macro preprocessor for templating purposes}
\usage{
knit_expand(file, ..., text = readLines(file, warn = FALSE), delim = c("{{", "}}"))
}
\arguments{
\item{file}{the template file}

\item{...}{a list of variables to be used for the code in the template; note
the variables will be searched in the parent frame as well}

\item{text}{an alternative way to \code{file} to specify the template code
directly (if provided, \code{file} will be ignored)}

\item{delim}{the (opening and ending) delimiters for the templating tags}
}
\value{
A character vector, with the tags evaluated and replaced by their
  values.
}
\description{
This function expands a template based on the R expressions in \code{{{}}}
(this tag can be customized by the \code{delim} argument). These expressions
are extracted, evaluated and replaced by their values in the original
template.
}
\examples{
# see the knit_expand vignette
if (interactive()) browseVignettes(package = "knitr")
}
\references{
This function was inspired by the pyexpander and m4
  (\url{http://www.gnu.org/software/m4/}), thanks to Frank Harrell.
}
