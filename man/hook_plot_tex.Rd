\name{hook_plot_tex}
\alias{hook_plot_tex}
\title{Default plot hook for LaTeX output}
\usage{
hook_plot_tex(x, options)
}
\arguments{
  \item{x}{a character vector of length 2 ; \code{x[1]} is
  the plot base filename, and \code{x[2]} is the file
  extension}

  \item{options}{a list of the current chunk options}
}
\value{
  A character string (LaTeX code)
}
\description{
  This hook function defines how to mark up plots in LaTeX
  output; depending on the options passed over, it may
  write the normal \samp{\\includegraphics{}} command, or
  \samp{\\input{}} (for tikz files), or
  \samp{\\animategraphics{}} (for animations).
}
\details{
  In most cases we do not need to call this hook function
  explicitly, and it was designed to be used internally.
  However, it can save a lot of efforts when the plots are
  not recordable via \code{\link[grDevices]{recordPlot}}
  and we save the plots to files manually (e.g. \pkg{rgl}
  plots). We can use a chunk hook to help write LaTeX code
  into the output document.
}
\examples{
## this is what happens for a chunk like this
## <<foo-bar-plot, dev=pdf, out.width=.7\\linewidth,
#   fig.align=right>>=
hook_plot_tex(c("foo-bar-plot", "pdf"), opts_chunk$merge(list(out.width = ".7\\\\linewidth", 
    fig.align = "right", fig.cur = 0, fig.num = 1)))

## <<bar, dev=tikz>>=
hook_plot_tex(c("bar", "tikz"), opts_chunk$merge(list(dev = "tikz", 
    fig.cur = 0, fig.num = 1)))

## <<foo, dev=pdf, fig.show=animate, interval=.1>>=
## 5 plots are generated in this chunk
hook_plot_tex(c("foo5", "pdf"), opts_chunk$merge(list(fig.show = "animate", 
    interval = 0.1, fig.cur = 5, fig.num = 5)))
}
\references{
  \url{http://yihui.github.com/knitr/demo/plot-hook/}
}
