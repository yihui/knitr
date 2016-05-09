% Please edit documentation in R/themes.R
\docType{data}
\name{knit_theme}
\alias{knit_theme}
\title{Syntax highlighting themes}
\format{An object of class \code{list} of length 2.}
\usage{
knit_theme
}
\description{
This object can be used to set or get themes in \pkg{knitr} for syntax
highlighting.
}
\details{
We can use \code{knit_theme$set(theme)} to set the theme, and
\code{knit_theme$get(theme)} to get a theme. The \code{theme} is a character
string for both methods (either the name of the theme, or the path to the CSS
file of a theme), and for the \code{set()} method, it can also be a list
returned by the \code{get()} method. See examples below.
}
\note{
The syntax highlighting here only applies to \file{.Rnw} (LaTeX) and
  \file{.Rhtml} (HTML) documents, and it does not work for other types of
  documents, such as \file{.Rmd} (R Markdown, which has its own syntax
  highlighting themes; see \url{http://rmarkdown.rstudio.com}).
}
\examples{
opts_knit$set(out.format = "latex")
knit_theme$set("edit-vim")

knit_theme$get()  # names of all available themes

thm = knit_theme$get("acid")  # parse the theme to a list
knit_theme$set(thm)

opts_knit$set(out.format = NULL)  # restore option
}
\author{
Ramnath Vaidyanathan and Yihui Xie
}
\references{
For a preview of all themes, see
  \url{https://gist.github.com/yihui/3422133}.
}
\keyword{datasets}
