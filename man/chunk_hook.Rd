% Please edit documentation in R/hooks-extra.R
\name{hook_pdfcrop}
\alias{hook_optipng}
\alias{hook_pdfcrop}
\alias{hook_plot_custom}
\alias{hook_purl}
\title{Built-in chunk hooks to extend knitr}
\usage{
hook_pdfcrop(before, options, envir)

hook_optipng(before, options, envir)

hook_plot_custom(before, options, envir)

hook_purl(before, options, envir)
}
\arguments{
\item{before,  options,  envir}{see references}
}
\description{
Hook functions are called when the corresponding chunk options are not
\code{NULL} to do additional jobs beside the R code in chunks. This package
provides a few useful hooks, which can also serve as examples of how to
define chunk hooks in \pkg{knitr}.
}
\details{
The function \code{hook_pdfcrop()} can use the program \command{pdfcrop} to
crop the extra white margin when the plot format is PDF to make better use of
the space in the output document, otherwise we often have to struggle with
\code{\link[graphics]{par}} to set appropriate margins. Note
\command{pdfcrop} often comes with a LaTeX distribution such as MiKTeX or
TeXLive, and you may not need to install it separately (use
\code{Sys.which('pdfcrop')} to check it; if it not empty, you are able to use
it). Similarly, when the plot format is not PDF (e.g. PNG), the program
\command{convert} in ImageMagick is used to trim the white margins (call
\command{convert input -trim output}).

The function \code{hook_optipng()} calls the program \command{optipng} to
optimize PNG images. Note the chunk option \code{optipng} can be used to
provide additional parameters to the program \command{optipng}, e.g.
\code{optipng = '-o7'}. See \url{http://optipng.sourceforge.net/} for
details.

When the plots are not recordable via \code{\link[grDevices]{recordPlot}} and
we save the plots to files manually via other functions (e.g. \pkg{rgl}
plots), we can use the chunk hook \code{hook_plot_custom} to help write code
for graphics output into the output document.

The hook \code{hook_purl()} can be used to write the code chunks to an R
script. It is an alternative approach to \code{\link{purl}}, and can be more
reliable when the code chunks depend on the execution of them (e.g.
\code{\link{read_chunk}()}, or \code{\link{opts_chunk}$set(eval = FALSE)}).
To enable this hook, it is recommended to associate it with the chunk option
\code{purl}, i.e. \code{knit_hooks$set(purl = hook_purl)}. When this hook is
enabled, an R script will be written while the input document is being
\code{\link{knit}}. Currently the code chunks that are not R code or have the
chunk option \code{purl=FALSE} are ignored. Please note when the cache is
turned on (the chunk option \code{cache = TRUE}), no chunk hooks will be
executed, hence \code{hook_purl()} will not work, either. To solve this
problem, we need \code{cache = 2} instead of \code{TRUE} (see
\url{http://yihui.name/knitr/demo/cache/} for the meaning of \code{cache =
2}).
}
\note{
The two hook functions \code{hook_rgl()} and \code{hook_webgl()} were
  moved from \pkg{knitr} to the \pkg{rgl} package (>= v0.95.1247) after
  \pkg{knitr} v1.10.5, and you can \code{library(rgl)} to get them.
}
\examples{
if (require("rgl") && exists("hook_rgl")) knit_hooks$set(rgl = hook_rgl)
# then in code chunks, use the option rgl=TRUE
}
\references{
\url{http://yihui.name/knitr/hooks#chunk_hooks}
}
\seealso{
\code{\link[rgl]{rgl.snapshot}}, \code{\link[rgl]{rgl.postscript}},
  \code{\link[rgl]{hook_rgl}}, \code{\link[rgl]{hook_webgl}}
}
