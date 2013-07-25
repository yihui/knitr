\name{rocco}
\alias{rocco}
\title{Knit R Markdown using the classic Docco style}
\usage{
rocco(input, ...)
}
\arguments{
  \item{input}{path of the input R Markdown file}

  \item{...}{arguments to be passed to
  \code{\link{knit2html}}}
}
\value{
  An HTML file is written, and its name is returned.
}
\description{
  The classic Docco style is a two-column layout, with text
  in the left and code in the right column.
}
\examples{
# TODO: need a better example
}
\references{
  The Docco package by Jeremy Ashkenas:
  \url{https://github.com/jashkenas/docco}
}
