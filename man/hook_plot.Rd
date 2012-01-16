\name{hook_plot_tex}
\alias{hook_plot_html}
\alias{hook_plot_md}
\alias{hook_plot_tex}
\title{Default plot hooks for different output formats}
\usage{
hook_plot_tex(x, options)

hook_plot_html(x, options)

hook_plot_md(x, options)
}
\arguments{
  \item{x}{a character vector of length 2 ; \code{x[1]} is
  the plot base filename, and \code{x[2]} is the file
  extension}

  \item{options}{a list of the current chunk options}
}
\value{
  A character string (code with plot filenames wrapped)
}
\description{
  These hook functions define how to mark up graphics
  output in different output formats.
}
\details{
  Depending on the options passed over,
  \code{hook_plot_tex} may return the normal
  \samp{\\includegraphics{}} command, or \samp{\\input{}}
  (for tikz files), or \samp{\\animategraphics{}} (for
  animations); it also takes many other options into
  consideration to align plots and set figure sizes, etc.
  Similarly, \code{hook_plot_html} and \code{hook_plot_md}
  return character strings which are HTML or Markdown code.

  In most cases we do not need to call these hooks
  explicitly, and it was designed to be used internally.
  Sometimes we may not be able to record R plots using
  \code{\link[grDevices]{recordPlot}}, and we can make use
  of these hooks to insert graphics output in the output
  document; see \code{\link{hook_plot_custom}} for details.
}
\examples{
## this is what happens for a chunk like this
## <<foo-bar-plot, dev=pdf, out.width=.7\\linewidth,
#   fig.align=right>>=
hook_plot_tex(c("foo-bar-plot", "pdf"), opts_chunk$merge(list(out.width = ".7\\\\linewidth", 
    fig.align = "right")))

## <<bar, dev=tikz>>=
hook_plot_tex(c("bar", "tikz"), opts_chunk$merge(list(dev = "tikz")))

## <<foo, dev=pdf, fig.show=animate, interval=.1>>=
## 5 plots are generated in this chunk
hook_plot_tex(c("foo5", "pdf"), opts_chunk$merge(list(fig.show = "animate", 
    interval = 0.1, fig.cur = 5, fig.num = 5)))
}
\references{
  \url{http://yihui.github.com/knitr/demo/plot-hook/}
}
\seealso{
  \code{\link{hook_plot_custom}}
}
