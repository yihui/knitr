#' Built-in chunk hooks to extend knitr
#'
#' Hook functions are called when the corresponding chunk options are not
#' \code{NULL} to do additional jobs beside the R code in chunks. This package
#' provides a few useful hooks, which can also serve as examples of how to
#' define chunk hooks in \pkg{knitr}.
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
#' \code{optipng = '-o7'}.
#'
#' The function \code{hook_pngquant()} calls the program \command{pngquant} to
#' optimize PNG images. Note the chunk option \code{pngquant} can be used to
#' provide additional parameters to the program \command{pngquant}, e.g.
#' \code{pngquant = '--speed=1 --quality=0-50'}.
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
#' @seealso \code{\link[rgl]{rgl.snapshot}}, \code{\link[rgl]{rgl.postscript}},
#'   \code{\link[rgl]{hook_rgl}}, \code{\link[rgl]{hook_webgl}}
#' @note The two hook functions \code{hook_rgl()} and \code{hook_webgl()} were
#'   moved from \pkg{knitr} to the \pkg{rgl} package (>= v0.95.1247) after
#'   \pkg{knitr} v1.10.5, and you can \code{library(rgl)} to get them.
#' @export
#' @examples if (require('rgl') && exists('hook_rgl')) knit_hooks$set(rgl = hook_rgl)
#' # then in code chunks, use the option rgl=TRUE
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
  hook_png(before, options, envir, 'optipng')
}

hook_png = function(
  before, options, envir, cmd = c('optipng', 'pngquant'), post_process = identity
) {
  if (before) return()
  ext = tolower(options$fig.ext)
  if (ext != 'png') {
    warning('this hook only works with PNG at the moment'); return()
  }
  cmd = match.arg(cmd)
  if (!nzchar(Sys.which(cmd))) {
    warning('cannot find ', cmd, '; please install and put it in PATH'); return()
  }
  paths = all_figs(options, ext)

  in_base_dir(
    lapply(paths, function(x) {
      message('optimizing ', x)
      cmd = paste(cmd, if (is.character(options[[cmd]])) options[[cmd]], shQuote(x))
      (if (is_windows()) shell else system)(cmd)
      post_process(x)
    })
  )
  return()
}

#' @export
#' @rdname chunk_hook
hook_pngquant = function(before, options, envir) {
  if (is.null(options[['pngquant']])) options$pngquant = '--skip-if-larger'
  hook_png(before, options, envir, 'pngquant', function(x) {
    # pngquant creates an output file with '-fs8.png' as the extension.
    x_opt = sub("\\.png$", "-fs8.png", x)
    file.rename(x_opt, x)
  })
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
