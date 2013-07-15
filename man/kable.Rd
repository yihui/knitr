\name{kable}
\alias{kable}
\title{Create tables in LaTeX, HTML, Markdown and reStructuredText}
\usage{
kable(x, format, digits = getOption("digits"), row.names = NA, align, output = TRUE, 
    ...)
}
\arguments{
  \item{x}{an R object (typically a matrix or data frame)}

  \item{format}{a character string; possible values are
  \code{latex}, \code{html}, \code{markdown},
  \code{pandoc}, and \code{rst}; this will be automatically
  determined if the function is called within \pkg{knitr};
  it can also be set in the global option
  \code{knitr.table.format}}

  \item{digits}{the maximum number of digits for numeric
  columns (passed to \code{round()})}

  \item{row.names}{whether to include row names; by
  default, row names are included if they are neither
  \code{NULL} nor identical to \code{1:nrow(x)}}

  \item{align}{the alignment of columns: a character vector
  consisting of \code{'l'} (left), \code{'c'} (center)
  and/or \code{'r'} (right); by default, numeric columns
  are right-aligned, and other columns are left-aligned; if
  \code{align = NULL}, the default alignment is used}

  \item{output}{whether to write out the output in the
  console}

  \item{...}{other arguments (see examples)}
}
\value{
  A character vector of the table source code. When
  \code{output = TRUE}, the results are also written into
  the console as a side-effect.
}
\description{
  This is a very simple table generator. It is simply by
  design. It is not intended to replace any other R
  packages for making tables.
}
\note{
  The tables for \code{format = 'markdown'} also work for
  Pandoc when the \code{pipe_tables} extension is enabled
  (this is the default behavior for Pandoc >= 1.10).
}
\examples{
kable(head(iris), format = "latex")
kable(head(iris), format = "html")
# use the booktabs package
kable(mtcars, format = "latex", booktabs = TRUE)
# use the longtable package
kable(matrix(1000, ncol = 5), format = "latex", digits = 2, longtable = TRUE)
# add some table attributes
kable(head(iris), format = "html", table.attr = "id=\"mytable\"")
# reST output
kable(head(mtcars), format = "rst")
# no row names
kable(head(mtcars), format = "rst", row.names = FALSE)
# R Markdown/Github Markdown tables
kable(head(mtcars[, 1:5]), format = "markdown")
# Pandoc tables
kable(head(mtcars), format = "pandoc")
# save the value
x = kable(mtcars, format = "html", output = FALSE)
cat(x, sep = "\\n")
# can also set options(knitr.table.format = 'html') so that the output is HTML
}
\seealso{
  Other R packages such as \pkg{xtable} and \pkg{tables}.
}
