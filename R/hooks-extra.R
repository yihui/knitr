#' Built-in chunk hooks to extend knitr
#'
#' Hook functions are called when the corresponding chunk options are not
#' \code{NULL} to do additional jobs beside the R code in chunks. This package
#' provides a few useful hooks, which can also serve as examples of how to
#' define chunk hooks in \pkg{knitr}.
#'
#' The function \code{hook_rgl} can be set as a hook in \pkg{knitr} to save
#' plots produced by the \pkg{rgl} package. According to the chunk option
#' \code{dev} (graphical device), plots can be save to different formats
#' (\code{postscript}: \samp{eps}; \code{pdf}: \samp{pdf}; other devices
#' correspond to the default PNG format). The plot window will be adjusted
#' according to chunk options \code{fig.width} and \code{fig.height}. Filenames
#' are derived from chunk labels and the \code{fig.path} option.
#'
#' The function \code{hook_pdfcrop} can use the program \command{pdfcrop} to
#' crop the extra white margin when the plot format is PDF to make better use of
#' the space in the output document, otherwise we often have to struggle with
#' \code{\link[graphics]{par}} to set appropriate margins. Note
#' \command{pdfcrop} often comes with a LaTeX distribution such as MiKTeX or
#' TeXLive, and you may not need to install it separately (use
#' \code{Sys.which('pdfcrop')} to check it; if it not empty, you are able to use
#' it). Similarly, when the plot format is not PDF (e.g. PNG), the program
#' \command{convert} in ImageMagick is used to trim the white margins (call
#' \command{convert input -trim output}).
#'
#' The function \code{hook_optipng} calls the program \command{optipng} to
#' optimize PNG images. Note the chunk option \code{optipng} can be used to
#' provide additional parameters to the program \command{optipng}, e.g.
#' \code{optipng = '-o7'}. See \url{http://optipng.sourceforge.net/} for
#' details.
#'
#' When the plots are not recordable via \code{\link[grDevices]{recordPlot}} and
#' we save the plots to files manually via other functions (e.g. \pkg{rgl}
#' plots), we can use the chunk hook \code{hook_plot_custom} to help write code
#' for graphics output into the output document.
#' @rdname chunk_hook
#' @param before,options,envir see references
#' @references \url{http://yihui.name/knitr/hooks#chunk_hooks}
#' @seealso \code{\link[rgl]{rgl.snapshot}}, \code{\link[rgl]{rgl.postscript}}
#' @export
#' @examples knit_hooks$set(rgl = hook_rgl)
#' ## then in code chunks, use the option rgl=TRUE
hook_rgl = function(before, options, envir) {
  ## after a chunk has been evaluated
  if (before || !require('rgl') || rgl.cur() == 0) return()  # no active device
  name = fig_path()
  par3d(windowRect = 100 + options$dpi * c(0, 0, options$fig.width, options$fig.height))
  Sys.sleep(.05) # need time to respond to window size change

  fmt = opts_knit$get('out.format')
  if (fmt %in% c('html', 'markdown', 'jekyll', 'rst')) options$dev = 'png'

  ## support 3 formats: eps, pdf and png (default)
  switch(options$dev,
         postscript = rgl.postscript(str_c(name, '.eps'), fmt = 'eps'),
         pdf = rgl.postscript(str_c(name, '.pdf'), fmt = 'pdf'),
         rgl.snapshot(str_c(name, '.png'), fmt = 'png'))

  hook_plot_custom(before, options, envir)
}
#' @export
#' @rdname chunk_hook
hook_pdfcrop = function(before, options, envir) {
  ## crops plots after a chunk is evaluated and plot files produced
  ext = options$fig.ext
  if (options$dev == 'tikz' && options$external) ext = 'pdf'
  if (before || (fig.num <- options$fig.num) == 0L) return()
  if (ext == 'pdf' && !nzchar(Sys.which('pdfcrop'))) {
    warning('pdfcrop not installed or not in PATH')
    return()
  }
  if (ext != 'pdf' && !nzchar(Sys.which('convert'))) {
    warning('ImageMagick not installed or not in PATH')
    return()
  }
  paths = all_figs(options, ext, fig.num)

  lapply(paths, function(x) {
    message('cropping ', x)
    x = shQuote(x)
    cmd = if (ext == 'pdf') paste("pdfcrop", x, x) else paste('convert', x, '-trim', x)
    if (.Platform$OS.type == 'windows') cmd = paste(Sys.getenv("COMSPEC"), "/c", cmd)
    system(cmd)
  })
  return()
}
#' @export
#' @rdname chunk_hook
hook_optipng = function(before, options, envir) {
  if (before) return()
  ext = tolower(options$fig.ext)
  if (ext != 'png') {
    warning('this hook only works with PNG at the moment'); return()
  }
  if (!nzchar(Sys.which('optipng'))) {
    warning('cannot find optipng; please install and put it in PATH'); return()
  }
  paths = all_figs(options, ext)

  lapply(paths, function(x) {
    message('optimizing ', x)
    x = shQuote(x)
    cmd = paste('optipng', if (is.character(options$optipng)) options$optipng, x)
    if (.Platform$OS.type == 'windows') cmd = paste(Sys.getenv("COMSPEC"), "/c", cmd)
    system(cmd)
  })
  return()
}
#' @export
#' @rdname chunk_hook
hook_plot_custom = function(before, options, envir){
  if(before) return() # run hook after the chunk

  ext = options$fig.ext %n% dev2ext(options$dev)
  name = fig_path()
  fmt = opts_knit$get('out.format')
  if (fmt %in% c('sweave', 'listings')) fmt = 'latex'
  hook = switch(fmt, latex = hook_plot_tex, html = hook_plot_html,
                rst = hook_plot_rst, hook_plot_md)

  n = options$fig.num
  if (n <= 1L) hook(c(name, ext), options) else {
    res = unlist(lapply(seq_len(n), function(i) {
      options$fig.cur = i
      hook(c(str_c(name, i), ext), options)
    }), use.names = FALSE)
    str_c(res, collapse = '')
  }
}
