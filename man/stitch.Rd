\name{stitch}
\alias{stitch}
\title{Automatically create a report based on an R script and a template}
\usage{
stitch(script, template = system.file("misc", "knitr-template.Rnw", 
    package = "knitr"), output = NULL)
}
\arguments{
  \item{script}{path to the R script}

  \item{template}{path of the template to use (by default
  the Rnw template in this package; there is also an HTML
  template in \pkg{knitr})}

  \item{output}{the output filename (passed to
  \code{\link{knit}})}
}
\value{
  path of the output document
}
\description{
  This is a convenience function for small-scale automatic
  reporting based on an R script and a template.
}
\details{
  The first two lines of the R script can contain the title
  and author of the report in comments of the form \samp{##
  title:} and \samp{## author:}. The template must have a
  chunk named \samp{auto-report}, which will be used to
  input all the R code from the script. See the examples
  below.
}
\examples{
s = system.file("misc", "stitch-test.R", package = "knitr")
\dontrun{
stitch(s)
}

## HTML report
out = stitch(s, system.file("misc", "knitr-minimal_knit_.html", 
    package = "knitr"))
if (interactive()) browseURL(out)
}
