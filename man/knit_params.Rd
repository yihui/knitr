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
    \item{\code{type}}{The parameter type. This can be a standard R object
    type such as \code{character}, \code{integer}, \code{numeric}, or
    \code{logical} as well as the special \code{date}, \code{datetime}, and
    \code{file} types. See the \emph{Types} section below for additional
    details.}
    \item{\code{value}}{The default value for the parameter.}
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

Parameter types are deduced implicitly based on the value provided. However
in some cases additional type information is required (for example when
a character vector needs to be interpreted as a date or as a file path).
In these cases a special type designater precedes the value. For example:

\preformatted{
---
title: My Document
output: html_document
params:
  start: !date 2015-01-01
---
}
}
\section{Types}{


All of the standard R types that can be parsed using
\code{\link[yaml]{yaml.load}} are supported. These types are used
implicitly based on the \code{value} provided so no special type
designater is required. Built-in types include \code{character},
\code{integer}, \code{numeric}, and \code{logical}.

In addition there are a number of custom types used to represent
dates and times as well as to note that character values have
special semantics (e.g. are the name of a file). These types are
specified by prefacing the YAML \code{value} with !\emph{typename},
for example:

\preformatted{
---
title: My Document
output: html_document
params:
  start: !date 2015-01-01
  end: !datetime 2015-01-01 12:30:00
  data: !file data.csv
---
}

Supported custom types include:

\describe{
  \item{\code{date}}{A character value representing a date.
  The underlying date value is parsed from the character
  value using the \code{\link[base]{as.Date}} function.}
  \item{\code{datetime}}{A character value representing a
  date and time. The underlying datetime value is parsed from
  the character value using the \code{\link[base]{as.POSIXct}}
  function. Note that these values should always speicifed using
  UTC (Universal Time, Coordinated).}
  \item{\code{file}}{A character value representing the name
  of a file.}
}
}
