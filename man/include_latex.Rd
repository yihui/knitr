% Please edit documentation in R/table.R
\name{include_latex}
\alias{include_latex}
\title{Embed external \code{.tex} file in \pkg{knitr} documents}
\usage{
include_latex(path)
}
\arguments{
\item{path}{a character vector of \code{.tex} file path}
}
\value{
Like \code{include_graphics}, \pkg{knitr} will convert the \code{.tex} file to pdf output
}
\description{
Due to the simplicity of \code{kable}, it would be useful to import external \code{.tex} file into \pkg{knitr} documents.
This function will extract the partition between \code{\begin{document}} and \code{\end{document}} of \code{.tex} file.
}
