#' Built-in chunk hooks to extend knitr
#'
#' Hook functions are called when the corresponding chunk options are not
#' \code{NULL} to do additional jobs beside the R code in chunks. This package
#' provides a few useful hooks, which can also serve as examples of how to
#' define chunk hooks in \pkg{knitr}.
#'
#' The function \code{hook_pdfcrop()} calls \code{\link{plot_crop}()} to crop
#' the white margins of PDF plots.
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
#' The function \code{hook_mogrify()} calls the program \command{mogrify}.  Note
#' the chunk option \code{mogrify} can be used to provide additional parameters
#' to the program \command{mogrify} (with default \code{-trim} to trim PNG
#' files).
#'
#' When the plots are not recordable via \code{grDevices::\link{recordPlot}()}
#' and we save the plots to files manually via other functions (e.g. \pkg{rgl}
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
#' \url{https://yihui.org/knitr/demo/cache/} for the meaning of \code{cache =
#' 2}).
#' @rdname chunk_hook
#' @param before,options,envir See \emph{References} below.
#' @references \url{https://yihui.org/knitr/hooks/#chunk_hooks}
#' @seealso \code{rgl::\link[rgl:snapshot]{rgl.snapshot}},
#'   \code{rgl::\link[rgl:postscript]{rgl.postscript}},
#'   \code{rgl::\link[rgl]{hook_rgl}},
#'   \code{rgl::\link[rgl:hook_rgl]{hook_webgl}}
#' @note The two hook functions \code{hook_rgl()} and \code{hook_webgl()} were
#'   moved from \pkg{knitr} to the \pkg{rgl} package (>= v0.95.1247) after
#'   \pkg{knitr} v1.10.5, and you can \code{library(rgl)} to get them.
#' @export
#' @examples if (require('rgl') && exists('hook_rgl')) knit_hooks$set(rgl = hook_rgl)
#' # then in code chunks, use the option rgl=TRUE
hook_pdfcrop = function(before, options, envir) {
  # crops plots after a chunk is evaluated and plot files produced
  if (before) return()
  in_base_dir(for (f in get_plot_files()) plot_crop(f))
}

get_plot_files = function() {
  unique(opts_knit$get('plot_files'))
}

#' @export
#' @rdname chunk_hook
hook_optipng = function(before, options, envir) {
  hook_png(before, options, envir, 'optipng')
}

hook_png = function(
  before, options, envir, cmd = c('optipng', 'pngquant', 'mogrify'), post_process = identity
) {
  if (before) return()
  cmd = match.arg(cmd)
  if (!nzchar(Sys.which(cmd))) {
    warning('cannot find ', cmd, '; please install and put it in PATH'); return()
  }
  paths = get_plot_files()
  paths = grep('[.]png$', paths, ignore.case = TRUE, value = TRUE)

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
  options[['pngquant']] = paste(options[['pngquant']], '--ext -fs8.png')
  hook_png(before, options, envir, 'pngquant', function(x) {
    # pngquant creates an output file with '-fs8.png' as the extension.
    x2 = sub("\\.png$", "-fs8.png", x)
    if (file.exists(x2)) file.rename(x2, x)
  })
}

#' @export
#' @rdname chunk_hook
hook_mogrify = function(before, options, envir) {
  if (is.null(options[['mogrify']])) options$mogrify = '-trim'
  hook_png(before, options, envir, cmd = 'mogrify', identity)
}

#' @export
#' @rdname chunk_hook
hook_plot_custom = function(before, options, envir){
  if (before) return() # run hook after the chunk
  if (options$fig.show == 'hide') return() # do not show figures

  ext = dev2ext(options)
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
    if (length(params)) write_utf8(params, output)
    .knitEnv$tangle.params = NULL
  }

  code = options$code
  if (isFALSE(options$eval)) code = comment_out(code, '# ', newline = FALSE)
  if (is.character(output)) {
    code = c(
      if (file.exists(output)) read_utf8(output),
      label_code(code, options$params.src)
    )
    write_utf8(code, output)
  }
}
