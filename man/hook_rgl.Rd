\name{hook_rgl}
\alias{hook_rgl}
\title{A hook function to save rgl plots}
\usage{
hook_rgl(before, options, envir)
}
\arguments{
  \item{before,options,envir}{see references}
}
\description{
  This function can be set as a hook in \pkg{knitr} to save
  plots produced by the \pkg{rgl} package. According to the
  chunk option \samp{dev} (graphical device), plots can be
  save to different formats (\samp{postscript}: \samp{eps};
  \samp{pdf}: \samp{pdf}; other devices correspond to the
  default PNG format). The plot window will be adjusted
  according to chunk options \samp{width} and
  \samp{height}. Filenames are derived from chunk labels
  and the prefix string.
}
\references{
  \url{http://yihui.github.com/knitr/hooks#chunk_hooks}
}
