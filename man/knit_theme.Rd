\docType{data}
\name{knit_theme}
\alias{knit_theme}
\title{Syntax highlighting themes}
\format{List of 2
 $ set:function (theme)  
 $ get:function (theme = NULL)}
\usage{
knit_theme
}
\description{
  This object can be used to set or get themes in
  \pkg{knitr} for syntax highlighting.
}
\details{
  We can use \code{knit_theme$set(theme)} to set the theme,
  and \code{knit_theme$get(theme)} to get a theme. The
  \code{theme} is a character string for both methods
  (either the name of the theme, or the path to the CSS
  file of a theme), and for the \code{set()} method, it can
  also be a list returned by the \code{get()} method. See
  examples below.
}
\examples{
knit_theme$set("edit-vim")

knit_theme$get()  # names of all available themes

thm = knit_theme$get("acid")  # parse the theme to a list
knit_theme$set(thm)
}
\author{
  Ramnath Vaidyanathan and Yihui Xie
}
\references{
  \url{https://github.com/downloads/yihui/knitr/knitr-themes.pdf}
  (its Rnw source is at
  \url{https://github.com/yihui/knitr/blob/master/inst/examples/knitr-themes.Rnw})
}
\seealso{
  \code{\link{eclipse_theme}} (use Eclipse themes)
}
\keyword{datasets}
