\name{eclipse_theme}
\alias{eclipse_theme}
\title{Download and convert a theme from eclipsecolorthemes.org to CSS}
\usage{
eclipse_theme(id)
}
\arguments{
  \item{id}{id of theme to save as CSS}
}
\value{
  Path to the CSS file converted from the website.
}
\description{
  This function uses the \pkg{XML} package to parse the
  theme as an XML file, then converts to a CSS file using a
  brew template in the \pkg{knitr} package. The CSS file
  can be further parsed with \code{knit_theme$get()}, and
  the result will be ready for \code{knit_theme$set()} to
  set the highlighting theme.
}
\examples{
## http://www.eclipsecolorthemes.org/?view=theme&id=1
\dontrun{
(css = eclipse_theme(1))
thm = knit_theme$get(css)
knit_theme$set(thm)
}
}
\author{
  Ramnath Vaidyanathan
}
\references{
  \url{http://www.eclipsecolorthemes.org/}
}
\seealso{
  \code{\link{knit_theme}}
}
