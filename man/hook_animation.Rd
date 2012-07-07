\name{hook_ffmpeg_html}
\alias{hook_ffmpeg_html}
\alias{hook_r2swf}
\alias{hook_scianimator}
\title{Hooks to create animations in HTML output}
\usage{
hook_ffmpeg_html(x, options)

hook_scianimator(x, options)

hook_r2swf(x, options)
}
\arguments{
  \item{x}{a character vector of length 2 ; \code{x[1]} is
  the plot base filename, and \code{x[2]} is the file
  extension}

  \item{options}{a list of the current chunk options}
}
\description{
  \code{hook_ffmpeg_html()} uses FFmpeg to convert images
  to a video; \code{hook_scianimator()} uses the JavaScript
  library SciAnimator to create animations;
  \code{hook_r2swf()} uses the \pkg{R2SWF} package.
}
\details{
  These hooks are mainly for the package option
  \code{animation.fun}, e.g. you can set
  \code{opts_knit$set(animation.fun = hook_scianimator)}.
}
