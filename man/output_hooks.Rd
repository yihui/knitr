\name{render_latex}
\alias{render_gfm}
\alias{render_html}
\alias{render_jekyll}
\alias{render_latex}
\alias{render_listings}
\alias{render_markdown}
\alias{render_sweave}
\title{Set output hooks for different output formats}
\usage{
render_latex()

render_sweave()

render_listings()

render_html()

render_markdown()

render_gfm()

render_jekyll()
}
\value{
  \code{NULL}; corresponding hooks are set as a side effect
}
\description{
  These functions set built-in output hooks for LaTeX, HTML
  and Markdown.
}
\details{
  There are three variants of markdown documents: ordinary
  markdown (\code{render_markdown()}), GFM (GitHub Flavored
  Markdown; \code{render_gfm()}) and Jekyll (a blogging
  system on GitHub; \code{render_jekyll()}). For LaTeX
  output, there are three variants as well: \pkg{knitr}'s
  default style (\code{render_latex()}; use the LaTeX
  \pkg{framed} package), Sweave style
  (\code{render_sweave()}; use \file{Sweave.sty}) and
  listings style (\code{render_listings()}; use LaTeX
  \pkg{listings} package). Default HTML output hooks are
  set by \code{render_html()}.

  These functions can be used before \code{knit()} or in
  the first chunk of the input document (ideally this chunk
  has options \code{include = FALSE} and \code{cache =
  FALSE}) so that all the following chunks will be
  formatted as expected.

  You can use \code{\link{knit_hooks}} to further customize
  output hooks; see references.
}
\references{
  See output hooks in \url{http://yihui.name/knitr/hooks}
}
