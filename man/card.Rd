\name{card}
\alias{card}
\title{Convert an R script to a literate programming document}
\usage{
card(wool, knit = TRUE, format = c("Rmd", "Rnw", "Rhtml", "Rtex", "Rrst"), 
    doc = "^#+'[ ]?")
}
\arguments{
  \item{wool}{the path to the R script}

  \item{knit}{logical: whether to compile the document
  after conversion}

  \item{format}{character: the output format (it takes five
  possible values); the default is R Markdown}

  \item{doc}{a regular expression to identify the
  documentation lines; by default it follows the roxygen
  convention, but it can be customized, e.g.  if you want
  to use \code{##} to denote documentation, you can use
  \code{'^##\\s*'}}
}
\value{
  The path of the literate programming document.
}
\description{
  This function takes a specially formatted R script and
  converts it to a literate programming document. By
  default normal text (documentation) should be written
  after the roxygen comment (\code{#'}) and code chunk
  options are written after \code{#+} or \code{#-}.
}
\details{
  The function name came from the carding process: carding
  breaks open the entangled scoured wool and removes the
  burrs and seeds, leaving the wool as a continuous web
  called a sliver (thanks to Jenny Hutchison). Obviously
  the entangled wool is the original R script, and the
  sliver is the literate programming document (ready to be
  knitted).
}
\examples{
#' write normal text like this and chunk options like below + label, opt=value

(s = system.file("examples", "knitr-card.R", package = "knitr"))
card(s)  # default markdown
o = card(s, knit = FALSE)  # convert only; do not make a purse yet
knit2html(o)  # compile to HTML

# other formats
card(s, FALSE, format = "Rnw")  # you need to write documentclass after #'
card(s, FALSE, format = "Rhtml")
card(s, FALSE, format = "Rtex")
card(s, FALSE, format = "Rrst")
}
\author{
  Yihui Xie, with the original idea from Richard FitzJohn
  (who named it as \code{sowsear()} which meant to make a
  silk purse out of a sow's ear)
}
\references{
  \url{http://yihui.name/knitr/demo/stitch/}
}
\seealso{
  \code{\link{stitch}} (feed a template with an R script)
}
