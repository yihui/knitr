\name{render_latex}
\alias{render_html}
\alias{render_jekyll}
\alias{render_latex}
\alias{render_listings}
\alias{render_markdown}
\alias{render_rst}
\alias{render_sweave}
\title{Set output hooks for different output formats}
\usage{
render_latex()

render_sweave()

render_listings()

render_html()

render_markdown(strict = FALSE)

render_jekyll(highlight = c("pygments", "prettify", "none"), extra = "")

render_rst(strict = FALSE)
}
\arguments{
  \item{strict}{whether to use strict markdown or reST
  syntax; for markdown: if \code{TRUE}, code blocks will be
  indented by 4 spaces, otherwise they are put in fences
  made by three backticks; for reST, if \code{TRUE}, code
  is put under two colons and indented by 4 spaces,
  otherwise is put under the \samp{sourcecode} directive
  (e.g. it is useful for Sphinx)}

  \item{highlight}{which code highlighting engine to use:
  for \code{pygments}, the Liquid syntax is used (default
  approach Jekyll); for \code{prettify}, the output is
  prepared for the JavaScript library \file{prettify.js};
  for \code{none}, no highlighting engine will be used
  (code blocks are indented by 4 spaces)}

  \item{extra}{extra tags for the highlighting engine; for
  \code{pygments}, it can be \code{'linenos'}; for
  \code{prettify}, it can be \code{'linenums'}}
}
\value{
  \code{NULL}; corresponding hooks are set as a side effect
}
\description{
  These functions set built-in output hooks for LaTeX,
  HTML, Markdown and reStructuredText.
}
\details{
  There are three variants of markdown documents: ordinary
  markdown (\code{render_markdown(strict = TRUE)}),
  extended markdown (e.g. GitHub Flavored Markdown and
  pandoc; \code{render_markdown(strict = FALSE)}), and
  Jekyll (a blogging system on GitHub;
  \code{render_jekyll()}). For LaTeX output, there are
  three variants as well: \pkg{knitr}'s default style
  (\code{render_latex()}; use the LaTeX \pkg{framed}
  package), Sweave style (\code{render_sweave()}; use
  \file{Sweave.sty}) and listings style
  (\code{render_listings()}; use LaTeX \pkg{listings}
  package). Default HTML output hooks are set by
  \code{render_html()}, and reStructuredText uses
  \code{render_rst()}.

  These functions can be used before \code{knit()} or in
  the first chunk of the input document (ideally this chunk
  has options \code{include = FALSE} and \code{cache =
  FALSE}) so that all the following chunks will be
  formatted as expected.

  You can use \code{\link{knit_hooks}} to further customize
  output hooks; see references.
}
\references{
  See output hooks in \url{http://yihui.name/knitr/hooks}.

  Jekyll and Liquid:
  \url{https://github.com/mojombo/jekyll/wiki/Liquid-Extensions};
  prettify.js:
  \url{http://code.google.com/p/google-code-prettify/}
}
