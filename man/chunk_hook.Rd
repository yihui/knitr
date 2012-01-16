\name{hook_rgl}
\alias{hook_pdfcrop}
\alias{hook_plot_custom}
\alias{hook_rgl}
\title{Built-in chunk hooks to extend knitr}
\usage{
hook_rgl(before, options, envir)

hook_pdfcrop(before, options, envir)

hook_plot_custom(before, options, envir)
}
\arguments{
  \item{before,options,envir}{see references}
}
\description{
  Hook functions are called when the corresponding chunk
  options are \code{TRUE} to do additional jobs beside the
  R code in chunks. This package provides a few useful
  hooks, which can also serve as examples of how to define
  chunk hooks in \pkg{knitr}.
}
\details{
  The function \code{hook_rgl} can be set as a hook in
  \pkg{knitr} to save plots produced by the \pkg{rgl}
  package. According to the chunk option \samp{dev}
  (graphical device), plots can be save to different
  formats (\samp{postscript}: \samp{eps}; \samp{pdf}:
  \samp{pdf}; other devices correspond to the default PNG
  format). The plot window will be adjusted according to
  chunk options \samp{width} and \samp{height}. Filenames
  are derived from chunk labels and the prefix string.

  The function \code{hook_pdfcrop} can use the program
  \command{pdfcrop} to crop the extra white margin in order
  to make better use of the space in the output document,
  otherwise we often have to struggle with
  \code{\link[graphics]{par}} to set appropriate margins.
  Note \command{pdfcrop} often comes with a LaTeX
  distribution such as MiKTeX or TeXLive, and you may not
  need to install it separately (use
  \code{Sys.which('pdfcrop')} to check it; if it not empty,
  you are able to use it).

  When the plots are not recordable via
  \code{\link[grDevices]{recordPlot}} and we save the plots
  to files manually via other functions (e.g. \pkg{rgl}
  plots), we can use the chunk hook \code{hook_plot_custom}
  to help write code for graphics output into the output
  document.
}
\examples{
knit_hooks$set(rgl = hook_rgl)
## then in code chunks, use the option rgl=TRUE
}
\references{
  \url{http://yihui.github.com/knitr/hooks#chunk_hooks}
}
\seealso{
  \code{\link[rgl]{rgl.snapshot}},
  \code{\link[rgl]{rgl.postscript}}
}
