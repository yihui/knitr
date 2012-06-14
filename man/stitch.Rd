\name{stitch}
\alias{stitch}
\title{Automatically create a report based on an R script and a template}
\usage{
stitch(script, template = system.file("misc", "knitr-template.Rnw", package = "knitr"), 
    output = NULL, envir = parent.frame())
}
\arguments{
  \item{script}{path to the R script}

  \item{template}{path of the template to use (by default
  the Rnw template in this package; there is also an HTML
  template in \pkg{knitr})}

  \item{output}{the output filename (passed to
  \code{\link{knit}}); by default it uses the base filename
  of the script}

  \item{envir}{the environment in which the code chunks are
  to be evaluated (can use \code{\link{new.env}()} to
  guarantee an empty new environment)}
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

# HTML report
stitch(s, system.file("misc", "knitr-template.Rhtml", package = "knitr"))

# or convert markdown to HTML
stitch(s, system.file("misc", "knitr-template.Rmd", package = "knitr"))
}
\seealso{
  \code{\link{spin}} (turn a specially formatted R script
  to a report)
}
