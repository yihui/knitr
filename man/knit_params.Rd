% Please edit documentation in R/params.R
\name{knit_params}
\alias{knit_params}
\title{Extract knit parameters from a document}
\usage{
knit_params(text)
}
\arguments{
\item{text}{Character vector containing the document text}
}
\value{
List of objects of class \code{knit_param} that correspond to the
  parameters declared in the \code{params} section of the YAML front matter.
  These objects have the following fields:

  \describe{
    \item{\code{name}}{The parameter name.}
    \item{\code{value}}{The default value for the parameter.}
    \item{\code{class}}{The R class names of the parameter's default value.}
    \item{\code{expr}}{The R expression that yielded the default value (if any).}
  }

  In addition, other fields included in the YAML may also be present
  alongside the name, type, and value fields (e.g. a \code{label} field
  that provides front-ends with a human readable name for the parameter).
}
\description{
This function reads the YAML front-matter section of a document and returns a
list of any parameters declared there. This function exists primarily to
support the parameterized reports feature of the \pkg{rmarkdown} package,
however is also used by the knitr \code{\link{purl}} function to include
the default parameter values in the R code it emits.
}
\details{
Parameters are included in YAML front matter using the \code{params} key.
This key can have any number of subkeys each of which represents a
parameter. For example:

\preformatted{
---
title: My Document
output: html_document
params:
  frequency: 10
  show_details: true
---
}

Parameter values can be provided inline as illustrated above or can be
included in a \code{value} sub-key. For example:

\preformatted{
---
title: My Document
output: html_document
params:
  frequency:
    value: 10
---
}

This second form is useful when you need to provide additional details
about the parameter (e.g. a \code{label} field as describe above).

You can use R code to yield the value of a parameter by prefacing the value
with \code{!r}, for example:

\preformatted{
---
title: My Document
output: html_document
params:
  start: !r Sys.Date()
---
}
}
