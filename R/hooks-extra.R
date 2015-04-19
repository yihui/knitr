#' Built-in chunk hooks to extend knitr
#'
#' Hook functions are called when the corresponding chunk options are not
#' \code{NULL} to do additional jobs beside the R code in chunks. This package
#' provides a few useful hooks, which can also serve as examples of how to
#' define chunk hooks in \pkg{knitr}.
#'
#' The function \code{hook_rgl()} can be set as a hook in \pkg{knitr} to save
#' plots produced by the \pkg{rgl} package. According to the chunk option
#' \code{dev} (graphical device), plots can be save to different formats
#' (\code{postscript}: \samp{eps}; \code{pdf}: \samp{pdf}; other devices
#' correspond to the default PNG format). The plot window will be adjusted
#' according to chunk options \code{fig.width} and \code{fig.height}. Filenames
#' are derived from chunk labels and the \code{fig.path} option.
#'
#' The function \code{hook_webgl()} is a wrapper for the
#' \code{\link[rgl]{writeWebGL}()} function in the \pkg{rgl} package. It writes
#' WebGL code to the output to reproduce the \pkg{rgl} scene in a browser.
#'
#' The function \code{hook_pdfcrop()} can use the program \command{pdfcrop} to
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
#' The function \code{hook_optipng()} calls the program \command{optipng} to
#' optimize PNG images. Note the chunk option \code{optipng} can be used to
#' provide additional parameters to the program \command{optipng}, e.g.
#' \code{optipng = '-o7'}. See \url{http://optipng.sourceforge.net/} for
#' details.
#'
#' When the plots are not recordable via \code{\link[grDevices]{recordPlot}} and
#' we save the plots to files manually via other functions (e.g. \pkg{rgl}
#' plots), we can use the chunk hook \code{hook_plot_custom} to help write code
#' for graphics output into the output document.
#'
#' The hook \code{hook_purl()} can be used to write the code chunks to an R
#' script. It is an alternative approach to \code{\link{purl}}, and can be more
#' reliable when the code chunks depend on the execution of them (e.g.
#' \code{\link{read_chunk}()}, or \code{\link{opts_chunk}$set(eval = FALSE)}).
#' To enable this hook, it is recommended to associate it with the chunk option
#' \code{purl}, i.e. \code{knit_hooks$set(purl = hook_purl)}. When this hook is
#' enabled, an R script will be written while the input document is being
#' \code{\link{knit}}. Currently the code chunks that are not R code or have the
#' chunk option \code{purl=FALSE} are ignored. Please note when the cache is
#' turned on (the chunk option \code{cache = TRUE}), no chunk hooks will be
#' executed, hence \code{hook_purl()} will not work, either. To solve this
#' problem, we need \code{cache = 2} instead of \code{TRUE} (see
#' \url{http://yihui.name/knitr/demo/cache/} for the meaning of \code{cache =
#' 2}).
#' @rdname chunk_hook
#' @param before,options,envir see references
#' @references \url{http://yihui.name/knitr/hooks#chunk_hooks}
#' @seealso \code{\link[rgl]{rgl.snapshot}}, \code{\link[rgl]{rgl.postscript}}
#' @export
#' @examples knit_hooks$set(rgl = hook_rgl)
#' # then in code chunks, use the option rgl=TRUE
hook_rgl = function(before, options, envir) {
  # after a chunk has been evaluated
  if (before || rgl::rgl.cur() == 0) return()  # no active device
  name = fig_path('', options)
  rgl::par3d(windowRect = 100 + options$dpi * c(0, 0, options$fig.width, options$fig.height))
  Sys.sleep(.05) # need time to respond to window size change

  in_base_dir(save_rgl(name, options$dev))

  options$fig.num = 1L  # only one figure in total
  hook_plot_custom(before, options, envir)
}

save_rgl = function(name, devices) {
  if (!file_test('-d', dirname(name))) dir.create(dirname(name), recursive = TRUE)
  # support 3 formats: eps, pdf and png (default)
  for (dev in devices) switch(
    dev,
    postscript = rgl::rgl.postscript(paste0(name, '.eps'), fmt = 'eps'),
    pdf = rgl::rgl.postscript(paste0(name, '.pdf'), fmt = 'pdf'),
    rgl::rgl.snapshot(paste0(name, '.png'), fmt = 'png')
  )
}

#' @export
#' @rdname chunk_hook
hook_pdfcrop = function(before, options, envir) {
  # crops plots after a chunk is evaluated and plot files produced
  ext = options$fig.ext
  if (options$dev == 'tikz' && options$external) ext = 'pdf'
  if (before || (fig.num <- options$fig.num %n% 0L) == 0L) return()
  paths = all_figs(options, ext, fig.num)
  in_base_dir(for (f in paths) plot_crop(f))
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

  in_base_dir(
    lapply(paths, function(x) {
      message('optimizing ', x)
      x = shQuote(x)
      cmd = paste('optipng', if (is.character(options$optipng)) options$optipng, x)
      (if (is_windows()) shell else system)(cmd)
    })
  )
  return()
}
#' @export
#' @rdname chunk_hook
hook_plot_custom = function(before, options, envir){
  if (before) return() # run hook after the chunk
  if (options$fig.show == 'hide') return() # do not show figures

  ext = options$fig.ext %n% dev2ext(options$dev)
  hook = knit_hooks$get('plot')

  n = options$fig.num
  if (n == 0L) n = options$fig.num = 1L # make sure fig.num is at least 1
  res = unlist(lapply(seq_len(n), function(i) {
    options$fig.cur = i
    hook(fig_path(ext, options, i), reduce_plot_opts(options))
  }), use.names = FALSE)
  paste(res, collapse = '')
}
#' @export
#' @rdname chunk_hook
hook_webgl = local({commonParts = TRUE; function(before, options, envir) {
  # after a chunk has been evaluated
  if (before || rgl::rgl.cur() == 0) return()  # no active device
  name = tempfile('rgl', '.', '.html'); on.exit(unlink(name))
  dpi = options$dpi / options$fig.retina  # should not consider Retina displays (#901)
  rgl::par3d(windowRect = 100 + dpi * c(0, 0, options$fig.width, options$fig.height))
  Sys.sleep(.05) # need time to respond to window size change

  prefix = gsub('[^[:alnum:]]', '_', options$label) # identifier for JS, better be alnum
  prefix = sub('^([^[:alpha:]])', '_\\1', prefix) # should start with letters or _
  rgl::writeWebGL(
    dir = dirname(name), filename = name, template = NULL, prefix = prefix,
    snapshot = FALSE, commonParts = commonParts
  )
  commonParts <<- FALSE
  res = readLines(name)
  res = res[!grepl('^\\s*$', res)] # remove blank lines
  paste(gsub('^\\s+', '', res), collapse = '\n') # no indentation at all (for Pandoc)
}})

#" a hook function to write out code from chunks
#' @export
#' @rdname chunk_hook
hook_purl = function(before, options, envir) {
  # at the moment, non-R chunks are ignored; it is unclear what I should do
  if (before || !options$purl || options$engine != 'R') return()

  output = .knitEnv$tangle.file
  if (isFALSE(.knitEnv$tangle.start)) {
    .knitEnv$tangle.start = TRUE
    unlink(output)
    # write out knit_params() data from YAML
    params = .knitEnv$tangle.params
    if (length(params)) writeLines(params, output)
    .knitEnv$tangle.params = NULL
  }

  code = options$code
  if (isFALSE(options$eval)) code = comment_out(code, '# ', newline = FALSE)
  if (is.character(output)) {
    cat(label_code(code, options$params.src), file = output, sep = '\n', append = TRUE)
  }
}
