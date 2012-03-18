\name{pat_rnw}
\alias{pat_brew}
\alias{pat_gfm}
\alias{pat_html}
\alias{pat_rnw}
\alias{pat_tex}
\title{Set regular expressions to read input documents}
\usage{
pat_rnw()

pat_brew()

pat_tex()

pat_html()

pat_gfm()
}
\value{
  The patterns object \code{\link{knit_patterns}} is
  modified as a side effect.
}
\description{
  These are convenience functions to set pre-defined
  pattern lists (the syntax to read input documents). The
  function names are built from corresponding file
  extensions, e.g. \code{pat_rnw()} can set the Sweave
  syntax to read Rnw documents.
}
\examples{
## see how knit_patterns is modified
knit_patterns$get()
pat_rnw()
knit_patterns$get()

knit_patterns$restore()  # empty the list
}
