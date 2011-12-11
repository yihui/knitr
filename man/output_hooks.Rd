\name{theme_latex}
\alias{theme_gfm}
\alias{theme_html}
\alias{theme_jekyll}
\alias{theme_latex}
\alias{theme_markdown}
\alias{theme_sweave}
\title{Themes for output}
\usage{
theme_latex()

theme_sweave()

theme_html()

theme_markdown()

theme_gfm()

theme_jekyll()
}
\value{
  \code{NULL}; corresponding hooks are set
}
\description{
  The theme functions are used to set output hooks.
  Currently there are built-in themes for LaTeX, HTML,
  Markdown, GFM (GitHub Flavored Markdown) and Jekyll (a
  blogging system on GitHub). The original Sweave style is
  supported via \code{theme_sweave()}.
}
\references{
  See output hooks in
  \url{http://yihui.github.com/knitr/hooks}
}
