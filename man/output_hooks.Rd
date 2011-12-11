\name{render_latex}
\alias{render_gfm}
\alias{render_html}
\alias{render_jekyll}
\alias{render_latex}
\alias{render_markdown}
\alias{render_sweave}
\title{Set output hooks for different output formats}
\usage{
render_latex()

render_sweave()

render_html()

render_markdown()

render_gfm()

render_jekyll()
}
\value{
  \code{NULL}; corresponding hooks are set
}
\description{
  Currently there are built-in output hooks for LaTeX,
  HTML, Markdown, GFM (GitHub Flavored Markdown) and Jekyll
  (a blogging system on GitHub). The original Sweave style
  is supported via \code{render_sweave()}. The prefix
  \samp{render_} stands for \dQuote{output hooks}.
}
\references{
  See output hooks in
  \url{http://yihui.github.com/knitr/hooks}
}
