#' Default plot hooks for different output formats
#'
#' These hook functions define how to mark up graphics output in different
#' output formats.
#'
#' Depending on the options passed over, \code{hook_plot_tex} may return the
#' normal \samp{\\includegraphics{}} command, or \samp{\\input{}} (for tikz
#' files), or \samp{\\animategraphics{}} (for animations); it also takes many
#' other options into consideration to align plots and set figure sizes, etc.
#' Similarly, \code{hook_plot_html}, \code{hook_plot_md} and
#' \code{hook_plot_rst} return character strings which are HTML, Markdown, reST
#' code.
#'
#' In most cases we do not need to call these hooks explicitly, and they were
#' designed to be used internally. Sometimes we may not be able to record R
#' plots using \code{\link[grDevices]{recordPlot}}, and we can make use of these
#' hooks to insert graphics output in the output document; see
#' \code{\link{hook_plot_custom}} for details.
#' @param x a character vector of length 2 ; \code{x[1]} is the plot base
#'   filename, and \code{x[2]} is the file extension
#' @param options a list of the current chunk options
#' @rdname hook_plot
#' @return A character string (code with plot filenames wrapped)
#' @references \url{http://yihui.name/knitr/hooks}
#' @seealso \code{\link{hook_plot_custom}}
#' @export
#' @examples ## this is what happens for a chunk like this
#'
#' ## <<foo-bar-plot, dev='pdf', fig.align='right'>>=
#' hook_plot_tex(c('foo-bar-plot', 'pdf'), opts_chunk$merge(list(fig.align='right')))
#'
#' ## <<bar, dev='tikz'>>=
#' hook_plot_tex(c('bar', 'tikz'), opts_chunk$merge(list(dev='tikz')))
#'
#' ## <<foo, dev='pdf', fig.show='animate', interval=.1>>=
#'
#' ## 5 plots are generated in this chunk
#' hook_plot_tex(c('foo5', 'pdf'), opts_chunk$merge(list(fig.show='animate',interval=.1,fig.cur=5, fig.num=5)))
hook_plot_tex = function(x, options) {
  rw = options$resize.width; rh = options$resize.height
  resize1 = resize2 = ''
  if (!is.null(rw) || !is.null(rh)) {
    resize1 = sprintf('\\resizebox{%s}{%s}{', rw %n% '!', rh %n% '!')
    resize2 = '} '
  }

  tikz = is_tikz_dev(options)

  a = options$fig.align
  fig.cur = options$fig.cur %n% 1L; fig.num = options$fig.num %n% 1L
  animate = options$fig.show == 'animate'
  if (!tikz && animate && fig.cur < fig.num) return('')

  usesub = length(subcap <- options$fig.subcap) && fig.num > 1
  ## multiple plots: begin at 1, end at fig.num
  ai = options$fig.show != 'hold'
  plot1 = ai || fig.cur <= 1L; plot2 = ai || fig.cur == fig.num
  align1 = if (plot1 || usesub)
    switch(a, left = '\n\n', center = '\n\n{\\centering ', right = '\n\n\\hfill{}', '\n')
  align2 = if (plot2 || usesub)
    switch(a, left = '\\hfill{}\n\n', center = '\n\n}\n\n', right = '\n\n', '')

  ## figure environment: caption, short caption, label
  cap = options$fig.cap; scap = options$fig.scap; fig1 = fig2 = ''
  mcap = fig.num > 1L && options$fig.show == 'asis' && !length(subcap)
  # use subfloats
  sub1 = sub2 = ''
  if(length(cap) && !is.na(cap)) {
    lab = str_c(options$fig.lp, options$label)
    if (plot1) {
      fig1 = sprintf('\\begin{%s}[%s]\n', options$fig.env, options$fig.pos)
    }
    if (usesub) sub1 = sprintf('\\subfloat[%s\\label{%s}]{', subcap, str_c(lab, fig.cur))
    if (plot2) {
      if (is.null(scap)) scap = str_split(cap, '\\.|;|:')[[1L]][1L]
      scap = if(is.na(scap)) '' else str_c('[', scap, ']')
      fig2 = sprintf('\\caption%s{%s\\label{%s}}\n\\end{%s}\n', scap, cap,
                     str_c(lab, ifelse(mcap, fig.cur, '')), options$fig.env)
    }
    if (usesub) sub2 = '}'
  }

  # maxwidth does not work with animations
  if (animate && identical(options$out.width, '\\maxwidth')) options$out.width = NULL
  size = paste(c(sprintf('width=%s', options$out.width),
                 sprintf('height=%s', options$out.height),
                 options$out.extra), collapse = ',')

  paste(fig1, sub1, align1, resize1,

        if (tikz) {
          sprintf('\\input{%s.tikz}', x[1])
        } else if (animate) {
          ## \animategraphics{} should be inserted only *once*!
          aniopts = options$aniopts
          aniopts = if (is.na(aniopts)) NULL else gsub(';', ',', aniopts)
          size = paste(c(size, sprintf('%s', aniopts)), collapse = ',')
          if (nzchar(size)) size = sprintf('[%s]', size)
          sprintf('\\animategraphics%s{%s}{%s}{%s}{%s}', size, 1/options$interval,
                  sub(str_c(fig.num, '$'), '', x[1]), 1L, fig.num)
        } else {
          if (nzchar(size)) size = sprintf('[%s]', size)
          sprintf('\\includegraphics%s{%s} ', size, x[1])
        },

        resize2, align2, sub2, fig2, sep = '')
}

.chunk.hook.tex = function(x, options) {
  col = if (ai <- output_asis(x, options)) '' else
    str_c(color_def(options$background), ifelse(is_tikz_dev(options), '', '\\color{fgcolor}'))
  k1 = str_c(col, '\\begin{kframe}\n')
  k2 = '\\end{kframe}'
  x = .rm.empty.envir(str_c(k1, x, k2))
  size = if (options$size == 'normalsize') '' else str_c('\\', options$size)
  if (!ai) x = str_c('\\begin{knitrout}', size, '\n', x, '\n\\end{knitrout}')
  if (options$split) {
    name = fig_path('.tex', options)
    if (!file.exists(dirname(name)))
      dir.create(dirname(name))
    cat(x, file = name)
    sprintf('\\input{%s}', name)
  } else x
}

## rm empty kframe and verbatim environments
.rm.empty.envir = function(x) {
  x = gsub('\\\\begin\\{(kframe)\\}\\s*\\\\end\\{\\1\\}', '', x)
  gsub('\\\\end\\{(verbatim|alltt)\\}\\s*\\\\begin\\{\\1\\}[\n]?', '', x)
}

## inline hook for tex
.inline.hook.tex = function(x) {
  if(is.numeric(x)) {
    x = format_sci(x, 'latex')
    if (getOption('OutDec') != '.') x = sprintf('\\text{%s}', x)
  }
  .inline.hook(x)
}
# an example of a chunk hook
.param.hook = function(before, options, envir) {
  if (before) {
    'do something before the code chunk'
  } else {
    'do something after the code chunk'
  }
}

.verb.hook = function(x, options) str_c('\\begin{verbatim}\n', x, '\\end{verbatim}\n')
.color.block = function(color1 = '', color2 = '') {
  function(x, options) {
    x = gsub('\n*$', '', x)
    sprintf('\n\n{\\ttfamily\\noindent%s%s%s}',
            color1, escape_latex(x, newlines = TRUE, spaces = TRUE), color2)
  }
}

#' Set output hooks for different output formats
#'
#' These functions set built-in output hooks for LaTeX, HTML, Markdown and
#' reStructuredText.
#'
#' There are three variants of markdown documents: ordinary markdown
#' (\code{render_markdown(strict = TRUE)}), extended markdown (e.g. GitHub
#' Flavored Markdown and pandoc; \code{render_markdown(strict = FALSE)}), and
#' Jekyll (a blogging system on GitHub; \code{render_jekyll()}). For LaTeX
#' output, there are three variants as well: \pkg{knitr}'s default style
#' (\code{render_latex()}; use the LaTeX \pkg{framed} package), Sweave style
#' (\code{render_sweave()}; use \file{Sweave.sty}) and listings style
#' (\code{render_listings()}; use LaTeX \pkg{listings} package). Default HTML
#' output hooks are set by \code{render_html()}, and reStructuredText uses
#' \code{render_rst()}.
#'
#' These functions can be used before \code{knit()} or in the first chunk of the
#' input document (ideally this chunk has options \code{include = FALSE} and
#' \code{cache = FALSE}) so that all the following chunks will be formatted as
#' expected.
#'
#' You can use \code{\link{knit_hooks}} to further customize output hooks; see
#' references.
#' @rdname output_hooks
#' @return \code{NULL}; corresponding hooks are set as a side effect
#' @export
#' @references See output hooks in \url{http://yihui.name/knitr/hooks}.
#'
#'   Jekyll and Liquid:
#'   \url{https://github.com/mojombo/jekyll/wiki/Liquid-Extensions};
#'   prettify.js: \url{http://code.google.com/p/google-code-prettify/}
render_latex = function() {
  test_latex_pkg('framed', system.file('misc', 'framed.sty', package = 'knitr'))
  opts_chunk$set(out.width = '\\maxwidth')
  h = opts_knit$get('header')
  if (!nzchar(h['framed'])) set_header(framed = .header.framed)
  if (!nzchar(h['highlight'])) set_header(highlight = .header.hi.tex)
  knit_hooks$restore()
  knit_hooks$set(
    source = function(x, options) {
      if (options$engine == 'R' && options$highlight) x else .verb.hook(x)
    },
    output = function(x, options) {
      if (output_asis(x, options)) {
        str_c('\\end{kframe}', x, '\\begin{kframe}')
      } else .verb.hook(x)
    },
    warning = .color.block('\\color{warningcolor}{', '}'),
    message = .color.block('\\itshape\\color{messagecolor}{', '}'),
    error = .color.block('\\bfseries\\color{errorcolor}{', '}'),
    inline = .inline.hook.tex, chunk = .chunk.hook.tex,
    plot = function(x, options) {
      ## escape plot environments from kframe
      str_c('\\end{kframe}', hook_plot_tex(x, options), '\n\\begin{kframe}')
    }
  )
}
#' @rdname output_hooks
#' @export
render_sweave = function() {
  opts_chunk$set(highlight = FALSE, comment = NA, prompt = TRUE) # mimic Sweave settings
  opts_knit$set(out.format = 'sweave')
  test_latex_pkg('Sweave', file.path(R.home('share'), 'texmf', 'tex', 'latex', 'Sweave.sty'))
  set_header(framed = '', highlight = '\\usepackage{Sweave}')
  knit_hooks$restore()
  ## wrap source code in the Sinput environment, output in Soutput
  hook.i = function(x, options) str_c('\\begin{Sinput}\n', x, '\\end{Sinput}\n')
  hook.s = function(x, options) str_c('\\begin{Soutput}\n', x, '\\end{Soutput}\n')
  hook.o = function(x, options) if (output_asis(x, options)) x else hook.s(x, options)
  hook.c = function(x, options) {
    if (output_asis(x, options)) return(x)
    str_c('\\begin{Schunk}\n', x, '\\end{Schunk}')
  }
  knit_hooks$set(source = hook.i, output = hook.o, warning = hook.s,
                 message = hook.s, error = hook.s, inline = .inline.hook.tex,
                 plot = hook_plot_tex, chunk = hook.c)
}
#' @rdname output_hooks
#' @export
render_listings = function() {
  render_sweave()
  opts_chunk$set(prompt = FALSE)
  opts_knit$set(out.format = 'listings')
  test_latex_pkg('Sweavel', system.file('misc', 'Sweavel.sty', package = 'knitr'))
  set_header(framed = '', highlight = '\\usepackage{Sweavel}')
  invisible(NULL)
}

## may add textile, and many other markup languages

#' Some potentially useful document hooks
#'
#' A document hook is a function to post-process the output document.
#'
#' \code{hook_movecode()} is a document hook to move code chunks out of LaTeX
#' floating environments like \samp{figure} and \samp{table} when the chunks
#' were actually written inside the floats. This function is primarily designed
#' for LyX: we often insert code chunks into floats to generate figures or
#' tables, but in the final output we do not want the code to float with the
#' environments, so we use regular expressions to find out the floating
#' environments, extract the code chunks and move them out. To disable this
#' behavior, use a comment \code{\% knitr_do_not_move} in the floating
#' environment.
#' @rdname hook_document
#' @param x a character string (the content of the whole document output)
#' @return The post-processed document as a character string.
#' @note These functions are hackish. Also note \code{hook_movecode()} assumes
#'   you to use the default output hooks for LaTeX (not Sweave or listings), and
#'   every figure/table environment must have a label.
#' @export
#' @references \url{http://yihui.name/knitr/hooks}
#' @examples \dontrun{knit_hooks$set(document = hook_movecode)}
hook_movecode = function(x) {
  x = split_lines(x)
  res = split(x, cumsum(grepl('^\\\\(begin|end)\\{figure\\}', x)))
  x = split_lines(unlist(lapply(res, function(p) {
    if (length(p) <= 4 || !grepl('^\\\\begin\\{figure\\}', p[1]) ||
          length(grep('% knitr_do_not_move', p)) ||
          !any(grepl('\\\\begin\\{(alltt|kframe)\\}', p))) return(p)
    idx = c(1, grep('\\\\includegraphics', p))
    if (length(idx) <= 1) return(p) # no graphics
    if (length(i <- grep('\\{\\\\centering.*\\\\includegraphics', p))) {
      idx = c(idx, i - 1, j2 <- i + 1)
      for (j in j2) {
        while (p[j] != '}') idx = c(idx, j <- j + 1) # find } for {\\centering
      }
    }
    if (length(i <- grep('\\\\hfill\\{\\}.*\\\\includegraphics', p)))
      idx = c(idx, i - 1, i + 1)
    if (length(i <- grep('\\\\includegraphics.*\\\\hfill\\{\\}', p)))
      idx = c(idx, i - 1, i + 1)
    idx = sort(c(idx, seq(grep('\\\\caption', p), grep('\\\\label', p))))
    idx = unique(idx)
    p = paste(c(p[-idx], p[idx]), collapse = '\n')
    gsub('\\\\end\\{(kframe)\\}\\s*\\\\begin\\{\\1\\}', '', p)
  }), use.names = FALSE))

  res = split(x, cumsum(grepl('^\\\\(begin|end)\\{table\\}', x)))
  res = paste(unlist(lapply(res, function(p) {
    if (length(p) <= 4 || !grepl('^\\\\begin\\{table\\}', p[1]) ||
          length(grep('% knitr_do_not_move', p)) ||
          !any(grepl('\\\\begin\\{(alltt|kframe)\\}', p))) return(p)
    if (!any(grepl('\\\\label\\{.*\\}', p))) return(p)
    idx = c(1, seq(grep('\\\\caption', p), grep('\\\\label', p)))
    i0 = grep('\\\\begin\\{tabular\\}', p); i1 = grep('\\\\end\\{tabular\\}', p)
    for (i in seq_along(i0)) idx = c(idx, i0[i]:i1[i])
    idx = sort(idx)
    p = paste(c(p[-idx], p[idx]), collapse = '\n')
    gsub('\\\\end\\{(kframe)\\}\\s*\\\\begin\\{\\1\\}', '', p)
  }), use.names = FALSE), collapse = '\n')
  .rm.empty.envir(res)
}
