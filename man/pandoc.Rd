\name{pandoc}
\alias{pandoc}
\title{A Pandoc wrapper to convert Markdown documents to other formats}
\usage{
pandoc(input, format = "html", config = getOption("config.pandoc"))
}
\arguments{
  \item{input}{a character vector of the Markdown
  filenames}

  \item{format}{the output format (see References); it can
  be a character vector of multiple formats}

  \item{config}{the Pandoc configuration file; if missing,
  it is assumed to be a file with the same base name as the
  \code{input} file and an extension \code{.pandoc} (e.g.
  for \file{foo.md} it looks for \file{foo.pandoc})}
}
\value{
  The output filename(s) (or an error if the conversion
  failed).
}
\description{
  This function calls Pandoc to convert Markdown documents
  to other formats such as HTML, LaTeX/PDF and Word, etc,
  (optionally) based on a configuration file or in-file
  configurations which specify the options to use for
  Pandoc.
}
\details{
  There are two ways to input the Pandoc configurations --
  through a config file, or embed the configurations in the
  markdown file as special comments between
  \verb{<!--pandoc} and \verb{-->}.

  The configuration file is a DCF file (see
  \code{\link{read.dcf}}). This file must contain a field
  named \code{format} which means the output format. The
  configurations are written in the form of
  \code{tag:value} and passed to Pandoc (if no value is
  needed, just leave it empty, e.g. the option
  \code{standalone} or \code{s} for short). If there are
  multiple output formats, write each format and relevant
  configurations in a block, and separate blocks with blank
  lines.
}
\examples{
system("pandoc -h")  # see possible output formats
}
\references{
  Pandoc: \url{http://johnmacfarlane.net/pandoc/}; Examples
  and rules of the configurations:
  \url{http://yihui.name/knitr/demo/pandoc}
}
\seealso{
  \code{\link{read.dcf}}
}
