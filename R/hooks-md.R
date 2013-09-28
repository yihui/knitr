#' @rdname hook_plot
#' @export
hook_plot_md = function(x, options) {
  if (options$fig.show == 'animate') return(hook_plot_html(x, options))

  base = opts_knit$get('base.url') %n% ''
  cap = .img.cap(options)

  if(is.null(w <- options$out.width) & is.null(h <- options$out.height) &
    is.null(s <- options$out.extra) & options$fig.align == 'default') {
    return(sprintf('![%s](%s%s) ', cap, base, .upload.url(x)))
  }
  # use HTML syntax <img src=...>
  .img.tag(.upload.url(x), w, h, cap, c(s, sprintf(
    'style="display: block; margin: %s;"', switch(
      options$fig.align, left = 'auto auto auto 0', center = 'auto',
      right = 'auto 0 auto auto')
  )))
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
  set_html_dev()
  opts_knit$set(out.format = 'markdown')
  ## four spaces lead to <pre></pre>
  hook.t = function(x, options) {
    if (strict) {
      paste('\n', indent_block(x), '', sep = '\n')
    } else paste('\n\n```\n', x, '```\n\n', sep = '')
  }
  hook.r = function(x, options) {
    paste('\n\n```', tolower(options$engine), '\n', x, '```\n\n', sep = '')
  }
  hook.o = function(x, options) if (output_asis(x, options)) x else hook.t(x, options)
  knit_hooks$set(
    source = function(x, options) {
      x = hilight_source(x, 'markdown', options)
      (if (strict) hook.t else hook.r)(paste(c(x, ''), collapse = '\n'), options)
    }, output = hook.o,
    warning = hook.t, error = hook.t, message = hook.t,
    inline = function(x) .inline.hook(format_sci(x, 'html')),
    plot = hook_plot_md,
    chunk = function(x, options) {
      x = gsub('[\n]{2,}(```|    )', '\n\n\\1', x)
      x = gsub('[\n]+$', '', x)
      x = gsub('^[\n]+', '\n', x)
      if (is.null(s <- options$indent)) return(x)
      line_prompt(x, prompt = s, continue = s)
    }
  )
}
#'@param highlight which code highlighting engine to use: for \code{pygments},
#'  the Liquid syntax is used (default approach Jekyll); for \code{prettify},
#'  the output is prepared for the JavaScript library \file{prettify.js}; for
#'  \code{none}, no highlighting engine will be used (code blocks are indented
#'  by 4 spaces)
#'@param extra extra tags for the highlighting engine; for \code{pygments}, it
#'  can be \code{'linenos'}; for \code{prettify}, it can be \code{'linenums'}
#'@rdname output_hooks
#'@export
render_jekyll = function(highlight = c('pygments', 'prettify', 'none'), extra = '') {
  hi = match.arg(highlight)
  render_markdown(TRUE)
  if (hi == 'none') return()
  switch(hi, pygments = {
    hook.r = function(x, options) {
      str_c('\n\n{% highlight ', tolower(options$engine), if (extra != '') ' ', extra, ' %}\n',
            x, '\n{% endhighlight %}\n\n')
    }
    hook.t = function(x, options) str_c('\n\n{% highlight text %}\n', x, '{% endhighlight %}\n\n')
  }, prettify = {
    hook.r = function(x, options) {
      str_c('\n\n<pre><code class="prettyprint ', extra, '">',
            escape_html(x), '</code></pre>\n\n')
    }
    hook.t = function(x, options) str_c('\n\n<pre><code>', escape_html(x), '</code></pre>\n\n')
  })
  hook.o = function(x, options) if (output_asis(x, options)) x else hook.t(x, options)
  knit_hooks$set(source = function(x, options) {
    x = paste(hilight_source(x, 'markdown', options), collapse = '\n')
    hook.r(x, options)
  }, output = hook.o, warning = hook.t, error = hook.t, message = hook.t)
}
