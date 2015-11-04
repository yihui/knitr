#' @rdname hook_plot
#' @export
hook_plot_md = function(x, options) {
  # if not using R Markdown v2 or output is HTML, just return v1 output
  if (is.null(to <- pandoc_to()) || is_html_output(to))
    return(hook_plot_md_base(x, options))
  if (!is.null(options$out.width) || !is.null(options$out.height) ||
        !is.null(options$out.extra) || options$fig.align != 'default') {
    if (to %in% c('beamer', 'latex')) {
      # Pandoc < 1.13 does not support \caption[]{} so suppress short caption
      if (is.null(options$fig.scap)) options$fig.scap = NA
      return(hook_plot_tex(x, options))
    }
    if (to == 'docx') {
      warning('Chunk options fig.align, out.width, out.height, out.extra ',
              'are not supported for Word output')
      options$out.width = options$out.height = options$out.extra = NULL
      options$fig.align = 'default'
    }
  }
  hook_plot_md_base(x, options)
}

is_html_output = function(fmt = pandoc_to()) {
  if (length(fmt) == 0) return(FALSE)
  grepl('^markdown', fmt) ||
    fmt %in% c('html', 'html5', 'revealjs', 's5', 'slideous', 'slidy')
}

hook_plot_md_base = function(x, options) {
  if (options$fig.show == 'animate') return(hook_plot_html(x, options))

  base = opts_knit$get('base.url') %n% ''
  cap = .img.cap(options)

  w = options$out.width; h = options$out.height
  s = options$out.extra; a = options$fig.align
  ai = options$fig.show == 'asis'
  pandoc_html = cap != '' && is_html_output()
  if (is.null(w) && is.null(h) && is.null(s) && a == 'default' && (!pandoc_html || !ai)) {
    return(sprintf('![%s](%s%s)%s ', cap, base, .upload.url(x), if (cap == '') '\\' else ''))
  }
  # use HTML syntax <img src=...>
  if (pandoc_html) {
    plot1 = ai || options$fig.cur <= 1L
    plot2 = ai || options$fig.cur == options$fig.num
    d1 = if (plot1) sprintf('<div class="figure"%s>\n', css_text_align(a))
    d2 = if (plot2) sprintf('\n<p class="caption">%s</p>\n</div>', cap)
    paste0(d1, sprintf(
      '<img src="%s" alt="%s" %s />',
      paste0(opts_knit$get('base.url'), .upload.url(x)), cap, .img.attr(w, h, s)
    ), d2)
  } else .img.tag(
    .upload.url(x), w, h, cap,
    c(s, sprintf('style="%s"', css_align(a)))
  )
}

css_align = function(align) {
  sprintf('display: block; margin: %s;', switch(
    align, left = 'auto auto auto 0', center = 'auto', right = 'auto 0 auto auto'
  ))
}

css_text_align = function(align) {
  if (align == 'default') '' else sprintf(' style="text-align: %s"', align)
}

#' @rdname output_hooks
#' @export
#' @param strict whether to use strict markdown or reST syntax; for markdown: if
#'   \code{TRUE}, code blocks will be indented by 4 spaces, otherwise they are
#'   put in fences made by three backticks; for reST, if \code{TRUE}, code is
#'   put under two colons and indented by 4 spaces, otherwise is put under the
#'   \samp{sourcecode} directive (e.g. it is useful for Sphinx)
render_markdown = function(strict = FALSE) {
  set_html_dev()
  opts_knit$set(out.format = 'markdown')
  # four spaces lead to <pre></pre>
  hook.t = function(x, options) {
    if (strict) {
      paste('\n', indent_block(x), '', sep = '\n')
    } else {
      x = paste(c('', x), collapse = '\n')
      fence = '```'
      if (grepl('\n`{3,}', x)) {
        l = attr(gregexpr('\n`{3,}', x)[[1]], 'match.length', exact = TRUE)
        l = max(l)
        if (l >= 4) fence = paste(rep('`', l), collapse = '')
      }
      paste0('\n\n', fence, x, fence, '\n\n')
    }
  }
  hook.r = function(x, options) {
    language = tolower(options$engine)
    if (language == 'node')
        language = 'javascript'
    if (!options$highlight) language = 'text'
    paste0('\n\n```', language, '\n', x, '```\n\n')
  }
  knit_hooks$set(
    source = function(x, options) {
      x = hilight_source(x, 'markdown', options)
      (if (strict) hook.t else hook.r)(paste(c(x, ''), collapse = '\n'), options)
    }, output = hook.t, warning = hook.t, error = hook.t, message = hook.t,
    inline = function(x) {
      fmt = pandoc_to()
      fmt = if (length(fmt) == 1L) 'latex' else 'html'
      .inline.hook(format_sci(x, fmt))
    },
    plot = hook_plot_md,
    chunk = function(x, options) {
      x = gsub('[\n]{2,}(```|    )', '\n\n\\1', x)
      x = gsub('[\n]+$', '', x)
      x = gsub('^[\n]+', '\n', x)
      if (isTRUE(options$collapse)) {
        x = gsub(paste0('\n([`]{3,})\n+\\1(', tolower(options$engine), ')?\n'), "\n", x)
      }
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
      paste0(
        '\n\n{% highlight ', tolower(options$engine), if (extra != '') ' ', extra,
        ' %}\n', x, '\n{% endhighlight %}\n\n'
      )
    }
    hook.t = function(x, options) paste0(
      '\n\n{% highlight text %}\n', x, '{% endhighlight %}\n\n'
    )
  }, prettify = {
    hook.r = function(x, options) {
      paste0(
        '\n\n<pre><code class="prettyprint ', extra, '">', escape_html(x),
        '</code></pre>\n\n'
      )
    }
    hook.t = function(x, options) paste0(
      '\n\n<pre><code>', escape_html(x), '</code></pre>\n\n'
    )
  })
  knit_hooks$set(source = function(x, options) {
    x = paste(hilight_source(x, 'markdown', options), collapse = '\n')
    hook.r(x, options)
  }, output = hook.t, warning = hook.t, error = hook.t, message = hook.t)
}
