\name{knit2html}
\alias{knit2html}
\title{Convert markdown to HTML using knit() and markdownToHTML()}
\usage{
knit2html(input, ...)
}
\arguments{
  \item{...}{options passed to \code{\link{knit}}}

  \item{input}{path of the input file}
}
\description{
  This is a convenience function to knit the input markdown
  source and call \code{markdownToHTML()} to convert the
  result to HTML.
}
\examples{
# a minimal example
writeLines(c("# hello markdown", "``` {r hello-random, echo=TRUE}", "rnorm(5)", 
    "```"), "test.Rmd")
knit2html("test.Rmd")
if (interactive()) browseURL("test.html")
}
\seealso{
  \code{\link{knit}},
  \code{\link[markdown]{markdownToHTML}}
}
