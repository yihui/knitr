\name{fig_path}
\alias{fig_path}
\title{Path for figure files}
\usage{
fig_path(suffix = "", options = opts_current$get())
}
\arguments{
  \item{suffix}{a suffix of the filename}

  \item{options}{a list of options; by default the options
  of the current chunk}
}
\value{
  A character string (path)
}
\description{
  The filename of figure files is the combination of
  options \code{fig.path} and \code{label}. This function
  returns the path of figures for the current chunk by
  default.
}
\note{
  When there are multiple figures in a chunk, this function
  only provides a prefix of the filenames by default, and
  the actual filenames are of the form \file{prefix1},
  \file{prefix2}, ... where \file{prefix} is the string
  returned by this function.
}
\examples{
fig_path(".pdf", list(fig.path = "figure/abc-", label = "first-plot"))
fig_path(1:10, list(fig.path = "foo-", label = "bar"))
}
