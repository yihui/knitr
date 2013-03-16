\name{knit}
\alias{knit}
\alias{purl}
\title{Knit a document}
\usage{
knit(input, output = NULL, tangle = FALSE, text = NULL, envir = parent.frame(), encoding = getOption("encoding"))

purl(..., documentation = 1L)
}
\arguments{
  \item{input}{path of the input file}

  \item{output}{path of the output file; if \code{NULL},
  this function will try to guess and it will be under the
  current working directory}

  \item{tangle}{whether to tangle the R code from the input
  file (like \code{\link[utils]{Stangle}})}

  \item{text}{a character vector as an alternative way to
  provide the input file}

  \item{envir}{the environment in which the code chunks are
  to be evaluated (can use \code{\link{new.env}()} to
  guarantee an empty new environment)}

  \item{encoding}{the encoding of the input file; see
  \code{\link{file}}}

  \item{documentation}{an integer specifying the level of
  documentation to go the tangled script: \code{0} means
  pure code (discard all text chunks); \code{1} (default)
  means add the chunk headers to code; \code{2} means add
  all text chunks to code as roxygen comments}

  \item{...}{arguments passed to \code{\link{knit}()} from
  \code{purl()}}
}
\value{
  The compiled document is written into the output file,
  and the path of the output file is returned, but if the
  \code{output} path is \code{NULL}, the output is returned
  as a character vector.
}
\description{
  This function takes an input file, extracts the R code in
  it according to a list of patterns, evaluates the code
  and writes the output in another file. It can also tangle
  R source code from the input document (\code{purl()} is a
  wrapper to \code{knit(..., tangle = TRUE)}).
}
\details{
  For most of the time, it is not necessary to set any
  options outside the input document; in other words, a
  single call like \code{knit('my_input.Rnw')} is usually
  enough. This function will try to determine many internal
  settings automatically. For the sake of reproducibility,
  it is a better practice to include the options inside the
  input document (to be self-contained), instead of setting
  them before knitting the document.

  First the filename of the output document is determined
  in this way: \file{foo.Rnw} generates \file{foo.tex}, and
  other filename extensions like \file{.Rtex},
  \file{.Rhtml} (\file{.Rhtm}) and \file{.Rmd}
  (\file{.Rmarkdown}) will generate \file{.tex},
  \file{.html} and \file{.md} respectively. For other types
  of files, the file extension is reserved; if the filename
  contains \samp{_knit_}, this part will be removed in the
  output file, e.g., \file{foo_knit_.html} creates the
  output \file{foo.html}; if \samp{_knit_} is not found in
  the filename, \file{foo.ext} will produce
  \file{foo-out.ext}. If \code{tangle = TRUE},
  \file{foo.ext} generates an R script \file{foo.R}.

  We need a set of syntax to identify special markups for R
  code chunks and R options, etc. The syntax is defined in
  a pattern list. All built-in pattern lists can be found
  in \code{all_patterns} (call it \code{apat}). First
  \pkg{knitr} will try to decide the pattern list based on
  the filename extension of the input document, e.g.
  \samp{Rnw} files use the list \code{apat$rnw}, \samp{tex}
  uses the list \code{apat$tex}, \samp{brew} uses
  \code{apat$brew} and HTML files use \code{apat$html}; for
  unkown extensions, the content of the input document is
  matched against all pattern lists to automatically which
  pattern list is being used. You can also manually set the
  pattern list using the \code{\link{knit_patterns}} object
  or the \code{\link{pat_rnw}} series functions in advance
  and \pkg{knitr} will respect the setting.

  According to the output format
  (\code{opts_knit$get('out.format')}), a set of output
  hooks will be set to mark up results from R (see
  \code{\link{render_latex}}). The output format can be
  LaTeX, Sweave and HTML, etc. The output hooks decide how
  to mark up the results (you can customize the hooks).

  See the package website and manuals in the references to
  know more about \pkg{knitr}, including the full
  documentation of chunk options and demos, etc.
}
\note{
  The name \code{knit} comes from its counterpart
  \samp{weave} (as in Sweave), and the name \code{purl} (as
  \samp{tangle} in Stangle) comes from a knitting method
  `knit one, purl one'.

  If the input document has child documents, they will also
  be compiled recursively. See \code{\link{knit_child}}.

  The working directory when evaluating R code chunks is
  the directory of the input document by default, so if the
  R code involves with external files (like
  \code{read.table()}), it is better to put these files
  under the same directory of the input document so that we
  can use relative paths. However, it is possible to change
  this directory with the package option
  \code{\link{opts_knit}$set(root.dir = ...)} so all paths
  in code chunks are relative to this \code{root.dir}.

  The arguments \code{input} and \code{output} do not have
  to be restricted to files; they can be
  \code{stdin()}/\code{stdout()} or other types of
  connections, but the pattern list to read the input has
  to be set in advance (see \code{\link{pat_rnw}}), and the
  output hooks should also be set (see
  \code{\link{render_latex}}), otherwise \pkg{knitr} will
  try to guess the patterns and output format.
}
\examples{
library(knitr)
(f = system.file("examples", "knitr-minimal.Rnw", package = "knitr"))
knit(f)  # compile to tex

purl(f)  # tangle R code
purl(f, documentation = 0)  # extract R code only
purl(f, documentation = 2)  # also include documentation
}
\references{
  Package homepage: \url{http://yihui.name/knitr/}

  The \pkg{knitr} main manual:
  \url{https://bitbucket.org/stat/knitr/downloads/knitr-manual.pdf}

  The \pkg{knitr} graphics manual:
  \url{https://bitbucket.org/stat/knitr/downloads/knitr-graphics.pdf}
}
