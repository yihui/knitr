\name{knit2html}
\alias{knit2html}
\title{Convert markdown to HTML using knit() and markdownToHTML()}
\usage{
knit2html(input, ..., envir = parent.frame(), text = NULL, encoding = getOption("encoding"))
}
\arguments{
  \item{...}{options passed to
  \code{\link[markdown]{markdownToHTML}}}

  \item{input}{path of the input file}

  \item{envir}{the environment in which the code chunks are
  to be evaluated (can use \code{\link{new.env}()} to
  guarantee an empty new environment)}

  \item{text}{a character vector as an alternative way to
  provide the input file}

  \item{encoding}{the encoding of the input file; see
  \code{\link{file}}}
}
\value{
  If the argument \code{text} is NULL, a character string
  (HTML code) is returned; otherwise the result is written
  into a file and \code{NULL} is returned.
}
\description{
  This is a convenience function to knit the input markdown
  source and call \code{\link[markdown]{markdownToHTML}()}
  in the \pkg{markdown} package to convert the result to
  HTML.
}
\examples{
# a minimal example
writeLines(c("# hello markdown", "```{r hello-random, echo=TRUE}", "rnorm(5)", "```"), 
    "test.Rmd")
if (require("markdown")) {
    knit2html("test.Rmd")
    if (interactive()) 
        browseURL("test.html")
}
}
\seealso{
  \code{\link{knit}},
  \code{\link[markdown]{markdownToHTML}}
}
