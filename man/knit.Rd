\name{knit}
\alias{knit}
\alias{purl}
\title{Knit a document}
\usage{
knit(input, output = NULL, tangle = FALSE, text = NULL)

purl(...)
}
\arguments{
  \item{input}{path of the input file}

  \item{output}{path of the output file; if \code{NULL},
  this function will try to guess and it will be under the
  current working directory}

  \item{tangle}{whether to tangle the R code from the input
  file (like \code{\link[utils]{Stangle}})}

  \item{text}{a character vector as an alternative way to
  provide the input file (\code{text} is written into a
  temporary file as \code{input}); this argument is mainly
  for test purposes only}

  \item{...}{arguments passed to \code{\link{knit}}}
}
\value{
  The compiled document is written into the output file,
  and the path of the output file is returned.
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
  for other types of files, the file extension is reserved;
  if the filename contains \samp{_knit_}, this part will be
  removed in the output file, e.g., \file{foo_knit_.html}
  creates the output \file{foo.html}, so you can use files
  named in this way as templates; if \samp{_knit_} is not
  found in the filename, \file{foo.ext} will produce
  \file{foo-out.ext}. If \code{tangle = TRUE},
  \file{foo.ext} generates an R script \file{foo.R}.

  Based on the file extension of the input document, a list
  of patterns will be used to extract R code in the
  document. All built-in pattern lists can be found in
  \code{opts_knit$get('all.patterns')} (call it
  \code{apat}). \samp{Rnw} files use the list
  \code{apat$rnw}, \samp{tex} uses the list
  \code{apat$tex}, \samp{brew} uses \code{apat$brew} and
  HTML-like files use \code{apat$html} (e.g. \samp{html}
  and \samp{md} files). You can manually set the pattern
  list using the \code{\link{knit_patterns}} object, and
  \pkg{knitr} will respect the setting.

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
  the directory of the input document, so if the R code
  involves with external files (like \code{read.table()}),
  it is better to put these files under the same directory
  of the input document so that we can use relative paths.
  It is recommended to change the working directory to the
  input directory before calling \code{knit()}, especially
  when the input document contains child documents.
}
\examples{
library(knitr)
(f = tempfile(fileext = ".Rnw"))
file.copy(system.file("examples", "knitr-minimal.Rnw", 
    package = "knitr"), f, overwrite = TRUE)
knit(f)
## or setwd(dirname(f)); knit(basename(f))

purl(f)  # extract R code only
}
\references{
  Package homepage: \url{http://yihui.github.com/knitr/}

  The \pkg{knitr} main manual:
  \url{https://github.com/downloads/yihui/knitr/knitr-manual.pdf}

  The \pkg{knitr} graphics manual:
  \url{https://github.com/downloads/yihui/knitr/knitr-graphics.pdf}
}
