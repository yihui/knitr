\name{Sweave2knitr}
\alias{Sweave2knitr}
\title{Convert Sweave to knitr documents}
\usage{
Sweave2knitr(file, output = gsub("[.]([^.]+)$", "-knitr.\\\\1", file), encoding = "unknown", 
    text = NULL)
}
\arguments{
  \item{file}{the filename of the Rnw file}

  \item{output}{the output filename (by default
  \file{file.Rnw} produces \file{file-knitr.Rnw}); if
  \code{text} is not NULL, no output file will be produced}

  \item{encoding}{the encoding of the Rnw file}

  \item{text}{an alternative way to provide the Sweave code
  as a character string (if provided, the \code{file} will
  be ignored)}
}
\value{
  If \code{text} is \code{NULL}, the \code{output} file is
  written and \code{NULL} is returned, otherwise the
  converted text string is returned.
}
\description{
  This function converts an Sweave document to a
  \pkg{knitr}-compatible document.
}
\details{
  The pseudo command \samp{\\SweaveInput{file.Rnw}} is
  converted to a code chunk header
  \code{<<child='file.Rnw'>>=}.

  Similarly \samp{\\SweaveOpts{opt = value}} is converted
  to a code chunk \samp{opts_chunk$set(opt = value)} with
  the chunk option \code{include = FALSE}; the options are
  automatically fixed in the same way as local chunk
  options (explained below).

  The Sweave package \samp{\\usepackage{Sweave}} in the
  preamble is removed because it is not reguired.

  Chunk options are updated if necessary: option values
  \code{true} and \code{false} are changed to \code{TRUE}
  and \code{FALSE} respectively; \code{fig=TRUE} is removed
  because it is not necessary for \pkg{knitr} (plots will
  be automatically generated); \code{fig=FALSE} is changed
  to \code{fig.keep='none'}; the devices
  \code{pdf/jpeg/png/eps/tikz=TRUE} are converted to
  \code{dev='pdf'/'jpeg'/'png'/'postscript'/'tikz'};
  \code{pdf/jpeg/png/eps/tikz=FALSE} are removed;
  \code{results=tex/verbatim/hide} are changed to
  \code{results='asis'/'markup'/'hide'};
  \code{width/height} are changed to
  \code{fig.width/fig.height}; \code{prefix.string} is
  changed to \code{fig.path};
  \code{print/term/strip.white/prefix=TRUE/FALSE} are
  removed; most of the character options (e.g.
  \code{engine} and \code{out.width}) are quoted;
  \code{keep.source=TRUE/FALSE} is changed to
  \code{tidy=FALSE/TRUE} (note the order of values);
}
\note{
  If \samp{\\SweaveOpts{}} spans across multiple lines, it
  will not be fixed, and you have to fix it manually. The
  LaTeX-style syntax of Sweave chunks are ignored (see
  \code{?SweaveSyntaxLatex}); only the Noweb syntax is
  supported.
}
\examples{
Sweave2knitr(text = "<<echo=TRUE>>=")  # this is valid
Sweave2knitr(text = "<<png=true>>=")  # dev='png'
Sweave2knitr(text = "<<eps=TRUE, pdf=FALSE, results=tex, width=5, prefix.string=foo>>=")
Sweave2knitr(text = "<<,png=false,fig=TRUE>>=")
Sweave2knitr(text = "\\\\SweaveOpts{echo=false}")
Sweave2knitr(text = "\\\\SweaveInput{hello.Rnw}")
}
\references{
  The motivation of the changes in the syntax:
  \url{http://yihui.name/knitr/demo/sweave/}
}
\seealso{
  \code{\link{Sweave}}, \code{\link{gsub}}
}
