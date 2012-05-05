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
  if (!options$include) return('')
  rw = options$resize.width; rh = options$resize.height
  resize1 = resize2 = ''
  if (!is.null(rw) || !is.null(rh)) {
    resize1 = sprintf('\\resizebox{%s}{%s}{', ifelse(is.null(rw), '!', rw),
                      ifelse(is.null(rh), '!', rh))
    resize2 = '} '
  }

  tikz = is_tikz_dev(options)

  a = options$fig.align; fig.cur = options$fig.cur; fig.num = options$fig.num
  if (is.null(fig.cur)) fig.cur = 0L; if (is.null(fig.num)) fig.num = 1L
  animate = options$fig.show == 'animate'
  if (!tikz && animate && fig.cur < fig.num) return('')

  align1 = align2 = ''
  ## multiple plots: begin at 1, end at fig.num
  ai = options$fig.show != 'hold'
  plot1 = ai || fig.cur <= 1L; plot2 = ai || fig.cur == 0L || fig.cur == fig.num
  if (plot1) align1 = switch(a, left = '\n\n', center = '\n\n{\\centering ',
                             right = '\n\n\\hfill{}', '')
  if (plot2) align2 = switch(a, left = '\\hfill{}\n\n', center = '\n\n}\n\n',
                             right = '\n\n', '')
  ## figure environment: caption, short caption, label
  cap = options$fig.cap; scap = options$fig.scap; fig1 = fig2 = ''
  mcap = fig.num > 1L && options$fig.show == 'asis'
  if (mcap) {
    cap = rep(cap, length.out = fig.num)[fig.cur] # multiple captions
    scap = rep(scap, length.out = fig.num)[fig.cur]
  } else {
    cap = cap[1L]; scap = scap[1L]
  }
  if(length(cap) && !is.na(cap)) {
    if (plot1) {
      fig1 = sprintf('\\begin{figure}[%s]\n', options$fig.pos)
    }
    if (plot2) {
      lab = str_c(options$fig.lp, options$label, ifelse(mcap, fig.cur, ''))
      if (is.null(scap)) scap = str_split(cap, '\\.|;|:')[[1L]][1L]
      scap = if(is.na(scap)) '' else str_c('[', scap, ']')
      fig2 = sprintf('\\caption%s{%s\\label{%s}}\n\\end{figure}\n', scap, cap, lab)
    }
  }

  size = paste(c(sprintf('width=%s', options$out.width),
                 sprintf('height=%s', options$out.height)), collapse = ',')

  paste(fig1, align1, resize1,

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

        resize2, align2, fig2, sep = '')
}

.chunk.hook.tex = function(x, options) {
  col = if (ai <- output_asis(x, options)) '' else
    str_c(color_def(options$background), ifelse(is_tikz_dev(options), '', '\\color{fgcolor}'))
  k1 = str_c(col, '\\begin{kframe}\n')
  k2 = '\\end{kframe}'
  x = str_c(k1, x, k2)
  ## rm empty kframe and verbatim environments
  x = gsub('\\\\begin\\{(kframe)\\}\\s*\\\\end\\{\\1\\}', '', x)
  x = gsub('\\\\end\\{(verbatim)\\}\\s*\\\\begin\\{\\1\\}[\n]?', '', x)
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

## inline hook for tex
.inline.hook.tex = function(x) {
  if (is.numeric(x)) x = format_sci(x, 'latex')
  .inline.hook(x)
}
## single param hook: a function of one argument
.param.hook = function(before, options, envir) {
  if (before) {
    'do something before the code chunk'
  } else {
    'do something after the code chunk'
  }
}

.verb.hook = function(x, options) str_c('\\begin{verbatim}\n', x, '\\end{verbatim}\n')

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
#' @references See output hooks in \url{http://yihui.name/knitr/hooks}
render_latex = function() {
  if (child_mode()) return()
  test_latex_pkg('framed', system.file('misc', 'framed.sty', package = 'knitr'))
  opts_chunk$set(out.width = '\\maxwidth')
  h = opts_knit$get('header')
  if (!nzchar(h['framed'])) set_header(framed = .header.framed)
  if (!nzchar(h['highlight'])) set_header(highlight = .header.hi.tex)
  knit_hooks$restore()
  knit_hooks$set(source = function(x, options) {
    if (options$highlight) {
      ## gsub() makes sure " will not produce an umlaut
      str_c('\\begin{flushleft}\n', gsub('"', '"{}', x, fixed = TRUE),
            '\\end{flushleft}\n')
    } else .verb.hook(x, options)
  }, output = function(x, options) {
    if (output_asis(x, options)) {
      str_c('\\end{kframe}\n', x, '\n\\begin{kframe}')
    } else .verb.hook(x, options)
  }, warning = .verb.hook, message = .verb.hook, error = .verb.hook,
                 inline = .inline.hook.tex, chunk = .chunk.hook.tex,
                 plot = function(x, options) {
                   ## escape plot environments from kframe
                   str_c('\\end{kframe}', hook_plot_tex(x, options), '\\begin{kframe}')
                 })
}
#' @rdname output_hooks
#' @export
render_sweave = function() {
  if (child_mode()) return()
  opts_chunk$set(highlight = FALSE, comment = NA, prompt = TRUE) # mimic Sweave settings
  test_latex_pkg('Sweave', file.path(R.home("share"), "texmf", "tex", "latex", "Sweave.sty"))
  set_header(framed = '', highlight = '\\usepackage{Sweave}')
  knit_hooks$restore()
  ## wrap source code in the Sinput environment, output in Soutput
  hook.i = function(x, options) str_c('\\begin{Sinput}\n', x, '\\end{Sinput}\n')
  hook.s = function(x, options) str_c('\\begin{Soutput}\n', x, '\\end{Soutput}\n')
  hook.o = function(x, options) if (output_asis(x, options)) x else hook.s(x, options)
  hook.c = function(x, options) str_c('\\begin{Schunk}\n', x, '\\end{Schunk}\n')
  knit_hooks$set(source = hook.i, output = hook.o, warning = hook.s,
                 message = hook.s, error = hook.s, inline = .inline.hook.tex,
                 plot = hook_plot_tex, chunk = hook.c)
}
#' @rdname output_hooks
#' @export
render_listings = function() {
  if (child_mode()) return()
  render_sweave()
  opts_chunk$set(prompt = FALSE)
  test_latex_pkg('Sweavel', system.file('misc', 'Sweavel.sty', package = 'knitr'))
  set_header(framed = '', highlight = '\\usepackage{Sweavel}')
  invisible(NULL)
}

## may add textile, and many other markup languages
