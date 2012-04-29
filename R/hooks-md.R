#' @rdname hook_plot
#' @export
hook_plot_md = function(x, options) {
  if(options$fig.show == 'animate') {
    .ani.plot.hook.html(x, options)
  } else {
    base = opts_knit$get('base.url')
    if (is.null(base)) base = ''
    cap = if (is.null(fig.cap <- options$fig.cap)) {
      sprintf('plot of chunk %s', options$label)
    } else {
      if (options$fig.num == 1L) fig.cap[1] else fig.cap[options$fig.cur]
    }
    sprintf('![%s](%s%s) ', cap, base, .upload.url(x))
  }
}

#' @rdname output_hooks
#' @export
#' @param strict whether to use strict markdown or reST syntax; for markdown: if
#'   \code{TRUE}, code blocks will be indented by 4 spaces, otherwise they are
#'   put in fences made by three backticks; for reST, if \code{TRUE}, code is
#'   put under two colons and indented by 4 spaces, otherwise is put under the
#'   \samp{sourcecode} directive (e.g. it is useful for Sphinx)
render_markdown = function(strict = FALSE) {
  knit_hooks$restore()
  opts_chunk$set(dev = 'png', highlight = FALSE)
  ## four spaces lead to <pre></pre>
  hook.t = function(x, options) {
    if (strict) {
      str_c('\n\n', indent_block(x), '\n')
    } else str_c('\n\n```\n', x, '```\n\n')
  }
  hook.r = function(x, options) str_c('\n\n```r\n', x, '```\n\n')
  hook.o = function(x, options) if (output_asis(x, options)) x else hook.t(x, options)
  knit_hooks$set(source = if (strict) hook.t else hook.r, output = hook.o,
                 warning = hook.t, error = hook.t, message = hook.t,
                 inline = function(x) sprintf(if (inherits(x, 'AsIs')) '%s' else '`%s`',
                                              .inline.hook(format_sci(x, 'html'))),
                 plot = hook_plot_md)
}
#' @rdname output_hooks
#' @export
render_jekyll = function() {
  render_markdown()
  hook.r = function(x, options) str_c('\n\n{% highlight r %}\n', x, '{% endhighlight %}\n\n')
  hook.t = function(x, options) str_c('\n\n{% highlight text %}\n', x, '{% endhighlight %}\n\n')
  hook.o = function(x, options) if (output_asis(x, options)) x else hook.t(x, options)
  knit_hooks$set(source = hook.r, output = hook.o, warning = hook.t,
                 error = hook.t, message = hook.t)
}