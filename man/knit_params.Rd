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
  parameters declared in the \code{params} section of the yaml front matter.
  These objects have the following fields:

  \describe{
    \item{\code{name}}{The parameter name.}
    \item{\code{type}}{The parameter type. This can be a standard R object
    type such as 'character', integer', 'numeric', or 'logical' as well as
    the special 'date' or 'file' types.}
    \item{\code{value}}{The default value for the parameter.}
  }

  In addition, other fields included in the yaml may also be present
  alongside the name, type, and value fields (e.g. a \code{label} field
  that provides front-ends with a human readable name for the parameter).
}
\description{
This function reads the yaml front-matter section of a document and returns a
list of any parameters declared there. This function exists primarly to
support the parameterized reports feature of the \pkg{rmarkdown} package,
however is also used by the knitr \code{\link{purl}} function to include
the default parameter values in the R code it emits.
}
\details{
Parameters are included in yaml front matter using the \code{params} key.
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

Parameter types are deduced implicity based on the value provided. However
in some cases additional type information is required (for example when
a character vector needs to be interpreted as a date or as a file path).
In these cases a special type designator precedes the value. For example:

\preformatted{
---
title: My Document
output: html_document
params:
  start: !date 2015-01-01
---
}
}
