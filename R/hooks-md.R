#' @rdname hook_plot
#' @export
hook_plot_md = function(x, options) {
  # if not using R Markdown v2 or output is HTML, just return v1 output
  if (is.null(to <- pandoc_to()) || is_html_output(to))
    return(hook_plot_md_base(x, options))
  if ((options$fig.show == 'animate' || is_tikz_dev(options)) && is_latex_output())
    return(hook_plot_tex(x, options))
  office_output = to %in% c('docx', 'pptx', 'rtf', 'odt')
  if (need_special_plot_hook(options)) {
    if (is_latex_output()) {
      # Pandoc < 1.13 does not support \caption[]{} so suppress short caption
      if (is.null(options$fig.scap)) options$fig.scap = NA
      return(hook_plot_tex(x, options))
    }
    if (office_output) {
      if (options$fig.align != 'default') {
        warn_options_unsupported('fig.align', to)
        options$fig.align = 'default'
      }
      if (!is.null(options$fig.alt)) {
        warn_options_unsupported('fig.alt', to)
        options$fig.alt = NULL
      }
      return(hook_plot_md_pandoc(x, options))
    }
  }
  hook_plot_md_base(x, options)
}

# decide if the markdown plot hook is not enough and needs special hooks like
# hook_plot_tex() to handle chunk options like out.width
need_special_plot_hook = function(options) {
  opts = opts_chunk$get(default = TRUE)
  for (i in c(
    'out.width', 'out.height', 'out.extra', 'fig.align', 'fig.subcap',
    'fig.env', 'fig.scap', 'fig.alt'
  )) if (!identical(options[[i]], opts[[i]])) return(TRUE)
  FALSE
}

hook_plot_md_base = function(x, options) {
  # when fig.id = TRUE, add an `id` attribute to images of the form fig:label-i
  if (isTRUE(options$fig.id)) options$fig.id = function(options) {
    id = sprintf('%s%s-%s', options$fig.lp, options$label, options$fig.cur)
    sprintf('id="%s"', xfun::alnum_id(id))
  }
  if (is.function(options$fig.id)) options$out.extra = c(
    options$out.extra, options$fig.id(options)
  )
  if (options$fig.show == 'animate') return(hook_plot_html(x, options))

  cap = .img.cap(options)
  alt = .img.cap(options, alt = TRUE, escape = TRUE)

  w = options[['out.width']]; h = options[['out.height']]
  s = options$out.extra; a = options$fig.align
  ai = options$fig.show == 'asis'
  # whether to use <object> to embed SVG graphics
  is_svg = grepl('[.]svg$', x, ignore.case = TRUE) && getOption('knitr.svg.object', FALSE)
  # self-contained mode?
  sc = any(c('--embed-resources', '--self-contained') %in% opts_knit$get('rmarkdown.pandoc.args'))
  lnk = options$fig.link
  pandoc_html = cap != '' && is_html_output()
  in_bookdown = isTRUE(opts_knit$get('bookdown.internal.label'))
  plot1 = ai || options$fig.cur <= 1L
  plot2 = ai || options$fig.cur == options$fig.num
  to = pandoc_to(); from = pandoc_from()
  if (is.null(w) && is.null(h) && is.null(s) && is.null(options$fig.alt) &&
      a == 'default' && !(pandoc_html && in_bookdown) && !is_svg) {
    # append <!-- --> to ![]() to prevent the figure environment in these cases
    nocap = cap == '' && !is.null(to) && !grepl('^markdown', to) &&
      (options$fig.num == 1 || ai) && !grepl('-implicit_figures', from)
    x2 = paste0(opts_knit$get('base.url'), .upload.url(x))
    res = sprintf('![%s](%s)', cap, x2)
    if (!is.null(lnk) && !is.na(lnk)) res = sprintf('[%s](%s)', res, lnk)
    res = paste0(res, if (nocap) '<!-- -->' else '', if (is_latex_output()) ' ' else '')
    return(res)
  }
  add_link = function(x) {
    if (is.null(lnk) || is.na(lnk)) return(x)
    sprintf('<a href="%s" target="_blank">%s</a>', lnk, x)
  }
  img_code = function(s2 = NULL) {
    img = if (is_svg && sc) svg_code(x, s) else .img.tag(x, w, h, alt, c(s, s2))
    add_link(img)
  }
  # use HTML syntax <img src=...>
  if (pandoc_html && !isTRUE(grepl('-implicit_figures', from))) {
    d1 = if (plot1) sprintf('<div class="figure"%s>\n', css_text_align(a))
    d2 = sprintf('<p class="caption">%s</p>', cap)
    img = img_code()
    # whether to place figure caption at the top or bottom of a figure
    if (isTRUE(options$fig.topcaption)) {
      paste0(d1, if (ai || options$fig.cur <= 1) d2, img, if (plot2) '</div>')
    } else {
      paste0(d1, img, if (plot2) paste0('\n', d2, '\n</div>'))
    }
  } else {
    img_code(sprintf('style="%s"', css_align(a)))
  }
}

# read svg, remove the xml/doctype declaration, and put the code in a raw html block
svg_code = function(file, extra = NULL) {
  x = read_utf8(file)
  while (length(x) > 0 && !grepl('^\\s*<svg .+', x[1])) x = x[-1]
  if (length(x) > 0 && length(extra) == 1) {
    if (grepl(r <- '\\s*>\\s*$', x[1])) {
      x[1] = paste0(gsub(r, ' ', x[1]), extra, '>')
    } else {
      x[1] = paste(x[1], extra)
    }
  }
  raw_html(x)
}

hook_plot_md_pandoc = function(x, options) {
  if (options$fig.show == 'animate') return(hook_plot_html(x, options))

  base = opts_knit$get('base.url') %n% ''
  cap = .img.cap(options)

  at = paste(
    c(
      sprintf('width=%s', options[['out.width']]),
      sprintf('height=%s', options[['out.height']]),
      options[['out.extra']]
    ),
    collapse = ' '
  )
  if (at != '') at = paste0('{', at, '}')

  sprintf('![%s](%s%s)%s', cap, base, .upload.url(x), at)
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
#' @param strict Boolean; whether to use strict markdown or reST syntax. For markdown, if
#'   \code{TRUE}, code blocks will be indented by 4 spaces, otherwise they are
#'   put in fences made by three backticks. For reST, if \code{TRUE}, code is
#'   put under two colons and indented by 4 spaces, otherwise it is put under the
#'   \samp{sourcecode} directive (this is useful for e.g. Sphinx).
#' @param fence_char A single character to be used in the code blocks fence.
#'   This can be e.g. a backtick or a tilde, depending on your Markdown rendering
#'   engine.
render_markdown = function(strict = FALSE, fence_char = '`') {
  set_html_dev()
  opts_knit$set(out.format = 'markdown')
  knit_hooks$set(hooks_markdown(strict, fence_char))
}

#' @rdname output_hooks
#' @export
hooks_markdown = function(strict = FALSE, fence_char = '`') {
  fence = paste(rep(fence_char, 3), collapse = '')
  # four spaces will generate a <pre> block
  hook.t = function(x, attr = NULL, class = NULL) {
    if (strict) {
      paste('\n', indent_block(x), '\n', sep = '\n')
    } else {
      fenced_block(x, attr, class, .char = fence_char)
    }
  }
  hook.o = function(class) {
    force(class)
    function(x, options) {
      if (class == 'output' && output_asis(x, options)) return(x)
      hook.t(x, options[[paste0('attr.', class)]], options[[paste0('class.', class)]])
    }
  }
  hook.r = function(x, options) {
    lang = tolower(options$lang %n% eng2lang(options$engine))
    if (!options$highlight) lang = 'text'
    fenced_block(x, options$attr.source, c(lang, options$class.source), .char = fence_char)
  }
  list(
    source = function(x, options) {
      x = hilight_source(x, 'markdown', options)
      if (strict) hook.t(x) else hook.r(sub('\n$', '\n\n', x), options)
    },
    inline = function(x) {
      if (is_latex_output()) .inline.hook.tex(x) else {
        .inline.hook(format_sci(x, if (length(pandoc_to()) == 1L) 'latex' else 'html'))
      }
    },
    plot = hook_plot_md,
    chunk = function(x, options) {
      x = gsub(paste0('[\n]{2,}(', fence, '|    )'), '\n\n\\1', x)
      x = gsub('[\n]+$', '', x)
      x = gsub('^[\n]+', '\n', x)
      if (isTRUE(options$collapse)) {
        r = sprintf('\n([%s]{3,})\n+\\1((\\{[.]| )?%s[^\n]*)?\n', fence_char, tolower(options$engine))
        x = gsub(r, '\n', x)
        x = gsub(asis_token, '', x, fixed = TRUE)
      }
      x = pandoc_div(x, options[['attr.chunk']], options[['class.chunk']])
      if (is.null(s <- options$indent)) return(x)
      line_prompt(x, prompt = s, continue = s)
    },
    output = hook.o('output'), warning = hook.o('warning'),
    error = hook.o('error'), message = hook.o('message')
  )
}

pandoc_div = function(x, attr = NULL, class = NULL) {
  if (is.null(attr) && is.null(class)) return(x)
  x = fenced_block(x, attr, class, .char = ':')
  x = gsub('^\n\n|\n\n$', '', x)
  gsub('^(:::+) *', '\\1 ', x)  # add a space if necessary
}

# turn a class string "a b" to c(".a", ".b") for Pandoc fenced code blocks
block_class = function(x, attr = NULL) {
  if (length(x)) x = unlist(strsplit(x, '\\s+'))
  if (length(x) > 1 || length(attr)) gsub('^[.]*', '.', x) else x
}

# add a fence around content (either fenced code block ``` or Div :::)
fenced_block = function(x, attr = NULL, class = NULL, .char = '`') {
  x = sub('\n$', '', x)
  x = xfun::fenced_block(x, c(block_class(class, attr), attr), char = .char)
  x = one_string(c('', x, '', ''))
  # remove the space between ``` and { for backward-compatibility
  sub('``` {', '```{', x, fixed = TRUE)
}

# convert some engine names to language names
eng2lang = function(x) {
  d = c(
    asy = 'cpp', mysql = 'sql', node = 'javascript', psql = 'sql',
    rscript = 'r', rcpp = 'cpp', tikz = 'tex'
  )
  x = tolower(x)
  if (x %in% names(d)) d[x] else x
}

#' @param highlight Which code highlighting engine to use: if \code{pygments},
#'   the Liquid syntax is used (default approach Jekyll); if \code{prettify},
#'   the output is prepared for the JavaScript library \file{prettify.js}; if
#'   \code{none}, no highlighting engine will be used, and code blocks are simply
#'   indented by 4 spaces).
#' @param extra Extra tags for the highlighting engine. For \code{pygments}, this
#'   can be \code{'linenos'}; for \code{prettify}, it can be \code{'linenums'}.
#' @rdname output_hooks
#' @export
render_jekyll = function(highlight = c('pygments', 'prettify', 'none'), extra = '') {
  render_markdown(TRUE)
  knit_hooks$set(hooks_jekyll(highlight = highlight, extra = extra))
}

#' @rdname output_hooks
#' @export
hooks_jekyll = function(highlight = c('pygments', 'prettify', 'none'), extra = '') {
  hook.m = hooks_markdown(TRUE)
  hi = match.arg(highlight)
  if (hi == 'none') return(hook.m)
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
        '\n\n<pre><code class="prettyprint ', extra, '">', html_escape(x),
        '</code></pre>\n\n'
      )
    }
    hook.t = function(x, options) paste0(
      '\n\n<pre><code>', html_escape(x), '</code></pre>\n\n'
    )
  })
  source = function(x, options) {
    x = one_string(hilight_source(x, 'markdown', options))
    hook.r(x, options)
  }
  merge_list(hook.m, list(
    source = source, warning = hook.t, message = hook.t, error = hook.t,
    output = function(x, options) {
      if (output_asis(x, options)) x else hook.t(x, options)
    }
  ))
}
