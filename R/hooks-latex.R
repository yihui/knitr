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
#' plots using \code{grDevices::\link{recordPlot}()}, and we can make use of
#' these hooks to insert graphics output in the output document; see
#' \code{\link{hook_plot_custom}} for details.
#' @param x Filename for the plot (a character string).
#' @param options A list of the current chunk options.
#' @rdname hook_plot
#' @return A character string of code, with plot filenames wrapped.
#' @references \url{https://yihui.org/knitr/hooks/}
#' @seealso \code{\link{hook_plot_custom}}
#' @export
#' @examples # this is what happens for a chunk like this
#'
#' # <<foo-bar-plot, dev='pdf', fig.align='right'>>=
#' hook_plot_tex('foo-bar-plot.pdf', opts_chunk$merge(list(fig.align='right')))
#'
#' # <<bar, dev='tikz'>>=
#' hook_plot_tex('bar.tikz', opts_chunk$merge(list(dev='tikz')))
#'
#' # <<foo, dev='pdf', fig.show='animate', interval=.1>>=
#'
#' # 5 plots are generated in this chunk
#' hook_plot_tex('foo5.pdf', opts_chunk$merge(list(fig.show='animate',interval=.1,fig.cur=5, fig.num=5)))
hook_plot_tex = function(x, options) {
  # This function produces the image inclusion code for LaTeX.
  # optionally wrapped in code that resizes it, aligns it, handles it
  # as a subfigure, and/or wraps it in a float. Here is a road map of
  # the intermediate variables this function fills in (or leaves empty,
  # as needed), and an impression of their (possible) contents.
  #
  #     fig1,                   # \begin{...}[...]
  #       align1,               #   {\centering
  #         sub1,               #     \subfloat[...]{
  #           resize1,          #       \resizebox{...}{...}{
  #             tikz code       #         '\\input{chunkname.tikz}'
  #             or animate code #         or '\\animategraphics[size]{1/interval}{chunkname}{1}{fig.num}'
  #             or plain code   #         or '\\includegraphics[size]{chunkname}'
  #           resize2,          #       }
  #         sub2,               #     }
  #       align2,               #   }
  #     fig2                    #   \caption[...]{...\label{...}}
  #                             # \end{...}  % still fig2

  rw = options$resize.width
  rh = options$resize.height
  rc = options$resize.command
  resize1 = resize2 = ''
  if (is.null(rc)) {
    if (!is.null(rw) || !is.null(rh)) {
      resize1 = sprintf('\\resizebox{%s}{%s}{', rw %n% '!', rh %n% '!')
      resize2 = '} '
    }
  } else {
    # users can specify a custom "resize" command (we can use an arbitrary
    # command, e.g., framebox)
    resize1 = paste0('\\', rc, '{')
    resize2 = '} '
  }

  tikz = is_tikz_dev(options)

  a = options$fig.align
  fig.cur = options$fig.cur %n% 1L
  fig.num = options$fig.num %n% 1L
  animate = options$fig.show == 'animate'
  fig.ncol = options$fig.ncol %n% fig.num
  if (is.null(fig.sep <- options$fig.sep)) {
    fig.sep = character(fig.num)
    if (fig.ncol < fig.num) fig.sep[seq(fig.ncol, fig.num - 1L, fig.ncol)] = '\\newline'
  }
  sep.cur = NULL

  # If this is a non-tikz animation, skip to the last fig.
  if (!tikz && animate && fig.cur < fig.num) return('')

  usesub = length(subcap <- options$fig.subcap) && fig.num > 1
  # multiple plots: begin at 1, end at fig.num
  ai = options$fig.show != 'hold'

  # TRUE if this picture is standalone or first in set
  plot1 = ai || fig.cur <= 1L
  # TRUE if this picture is standalone or last in set
  plot2 = ai || fig.cur == fig.num

  # open align code if this picture is standalone/first in set
  align1 = if (plot1)
    switch(a, left = '\n\n', center = '\n\n{\\centering ', right = '\n\n\\hfill{}', '\n')
  # close align code if this picture is standalone/last in set
  align2 = if (plot2)
    switch(a, left = '\\hfill{}\n\n', center = '\n\n}\n\n', right = '\n\n', '')

  # figure environment: caption, short caption, label
  cap = options$fig.cap
  scap = options$fig.scap
  fig1 = fig2 = ''
  mcap = fig.num > 1L && options$fig.show == 'asis' && !length(subcap)
  # initialize subfloat strings
  sub1 = sub2 = ''

  # Wrap in figure environment only if user specifies a caption
  if (length(cap) && !is.na(cap)) {
    lab = paste0(options$fig.lp, options$label)
    # If pic is standalone/first in set: open figure environment
    if (plot1) {
      pos = options$fig.pos
      if (pos != '' && !grepl('^[[{]', pos)) pos = sprintf('[%s]', pos)
      fig1 = sprintf('\\begin{%s}%s', options$fig.env, pos)
    }
    # Add subfloat code if needed
    if (usesub) {
      sub1 = sprintf('\\subfloat[%s%s]{', subcap, create_label(lab, '-', fig.cur, latex = TRUE))
      sub2 = '}'
      sep.cur = fig.sep[fig.cur]; if (is.na(sep.cur)) sep.cur = NULL
    }

    # If pic is standalone/last in set:
    # * place caption with label
    # * close figure environment
    if (plot2) {
      if (is.null(scap) && !grepl('[{].*?[:.;].*?[}]', cap)) {
        scap = strsplit(cap, '[:.;]( |\\\\|$)')[[1L]][1L]
      }
      scap = if (is.null(scap) || is.na(scap)) '' else sprintf('[%s]', scap)
      cap = if (cap == '') '' else sprintf(
        '\\caption%s{%s}%s\n', scap, cap,
        create_label(lab, if (mcap) c('-', fig.cur), latex = TRUE)
      )
      fig2 = sprintf('%s\\end{%s}\n', cap, options$fig.env)
    }
  } else if (pandoc_to(c('latex', 'beamer'))) {
    # use alignment environments for R Markdown latex output (\centering won't work)
    align.env = switch(a, left = 'flushleft', center = 'center', right = 'flushright')
    align1 = if (plot1) if (a == 'default') '\n' else sprintf('\n\n\\begin{%s}', align.env)
    align2 = if (plot2) if (a == 'default') '' else sprintf('\\end{%s}\n\n', align.env)
  }

  ow = options$out.width
  # maxwidth does not work with animations
  if (animate && identical(ow, '\\maxwidth')) ow = NULL
  if (is.numeric(ow)) ow = paste0(ow, 'px')
  size = paste(c(sprintf('width=%s', ow),
                 sprintf('height=%s', options$out.height),
                 options$out.extra), collapse = ',')

  paste0(
    fig1, align1, sub1, resize1,
    if (tikz) {
      sprintf('\\input{%s}', x)
    } else if (animate) {
      # \animategraphics{} should be inserted only *once*!
      aniopts = options$aniopts
      aniopts = if (is.na(aniopts)) NULL else gsub(';', ',', aniopts)
      size = paste(c(size, sprintf('%s', aniopts)), collapse = ',')
      if (nzchar(size)) size = sprintf('[%s]', size)
      sprintf('\\animategraphics%s{%s}{%s}{%s}{%s}', size, 1 / options$interval,
              sub(sprintf('%d$', fig.num), '', sans_ext(x)), 1L, fig.num)
    } else {
      if (nzchar(size)) size = sprintf('[%s]', size)
      res = sprintf(
        '\\includegraphics%s{%s} ', size,
        if (getOption('knitr.include_graphics.ext', FALSE)) x else sans_ext(x)
      )
      lnk = options$fig.link
      if (is.null(lnk) || is.na(lnk)) res else sprintf('\\href{%s}{%s}', lnk, res)
    },

    resize2, sub2, sep.cur, align2, fig2
  )
}

.chunk.hook.tex = function(x, options) {
  ai = output_asis(x, options)
  col = if (!ai) paste0(
    color_def(options$background), if (!is_tikz_dev(options)) '\\color{fgcolor}'
  )
  k1 = paste0(col, '\\begin{kframe}\n')
  k2 = '\\end{kframe}'
  x = .rm.empty.envir(paste0(k1, x, k2))
  size = if (options$size == 'normalsize') '' else sprintf('\\%s', options$size)
  if (!ai) x = sprintf('\\begin{knitrout}%s\n%s\n\\end{knitrout}', size, x)
  if (options$split) {
    name = fig_path('.tex', options, NULL)
    if (!file.exists(dirname(name)))
      dir.create(dirname(name))
    write_utf8(x, name)
    sprintf('\\input{%s}', name)
  } else x
}

# rm empty kframe and verbatim environments
.rm.empty.envir = function(x) {
  x = gsub('\\\\begin\\{(kframe)\\}\\s*\\\\end\\{\\1\\}', '', x)
  gsub('\\\\end\\{(verbatim|alltt)\\}\\s*\\\\begin\\{\\1\\}[\n]?', '', x)
}

# inline hook for tex
.inline.hook.tex = function(x) {
  if (is.numeric(x)) {
    x = format_sci(x, 'latex')
    i = grep('[}]', x)
    x[i] = sprintf('\\ensuremath{%s}', x[i])
    # why \text{}: https://github.com/yihui/knitr/issues/348
    if (getOption('OutDec') != '.') x = sprintf('\\text{%s}', x)
  }
  .inline.hook(x)
}

.verb.hook = function(x)
  one_string(c('\\begin{verbatim}', sub('\n$', '', x), '\\end{verbatim}', ''))
.color.block = function(color1 = '', color2 = '') {
  function(x, options) {
    x = gsub('\n*$', '', x)
    x = escape_latex(x, newlines = TRUE, spaces = TRUE)
    # babel might have problems with "; see http://stackoverflow.com/q/18125539/559676
    x = gsub('"', '"{}', x)
    sprintf('\n\n{\\ttfamily\\noindent%s%s%s}', color1, x, color2)
  }
}

#' Set or get output hooks for different output formats
#'
#' The \code{render_*} functions set built-in output hooks for LaTeX, HTML,
#' Markdown, reStructuredText, AsciiDoc and Textile. The \code{hooks_*}
#' functions return a list of the output hooks for the corresponding format.
#'
#' There are three variants of markdown documents: ordinary markdown
#' (\code{render_markdown(strict = TRUE)} which calls \code{hooks_markdown(strict
#' = TRUE)}), extended markdown (e.g. GitHub Flavored Markdown and pandoc;
#' \code{render_markdown(strict = FALSE)} which calls
#' \code{hooks_markdown(strict = FALSE)}), and Jekyll (a blogging system on
#' GitHub; \code{render_jekyll()} which calls \code{hooks_jekyll()}).\cr
#' For LaTeX output, there are three variants as well: \pkg{knitr}'s default
#' style (\code{render_latex()} which calls \code{hooks_latex()}; use the LaTeX
#' \pkg{framed} package), Sweave style (\code{render_sweave()} which calls
#' \code{hooks_sweave()}; use \file{Sweave.sty}) and listings style
#' (\code{render_listings()} which calls \code{hooks_listings()}; use LaTeX
#' \pkg{listings} package).\cr
#' Default HTML output hooks are set by \code{render_html()} (which calls
#' \code{hooks_html()}); \code{render_rst()} (which calls \code{hooks_rst()}) is
#' for reStructuredText; \code{render_textile()} (which calls
#' \code{hooks_textile()}) is for Textile and \code{render_asciidoc()} (which
#' calls \code{hooks_asciidoc()}) is AsciiDoc.
#'
#' The \code{render_*} functions can be used before \code{knit()} or in the first chunk of the
#' input document (ideally this chunk has options \code{include = FALSE} and
#' \code{cache = FALSE}) so that all the following chunks will be formatted as
#' expected.
#'
#' You can also use \code{\link{knit_hooks}} to set the format's hooks with the
#' \code{hooks_*} functions; see references for more to further customize output
#' hooks.
#'
#' @rdname output_hooks
#' @return
#' \itemize{
#'   \item \code{NULL} for \code{render_*} functions; corresponding hooks are
#'   set as a side effect
#'   \item A list of output hooks for \code{hooks_*} functions; hooks are not
#'   set in that case.
#' }
#' @export
#' @references See output hooks in \url{https://yihui.org/knitr/hooks/}, and
#'   some examples in
#'   \url{https://bookdown.org/yihui/rmarkdown-cookbook/output-hooks.html}
#'
#'   Jekyll and Liquid:
#'   \url{https://github.com/jekyll/jekyll/wiki/Liquid-Extensions};
#'   prettify.js: \url{http://code.google.com/p/google-code-prettify/}
#' @examples
#' # modify the output hooks for a format
#' knitr::render_markdown()
#'
#' # only set some of the hooks from a format
#' knitr::knit_hooks$set(knitr::hooks_markdown()["source"])
#'
render_latex = function() {
  opts_chunk$set(out.width = '\\maxwidth', dev = 'pdf')
  opts_knit$set(out.format = 'latex')
  h = opts_knit$get('header')
  if (!nzchar(h['framed'])) set_header(framed = .header.framed)
  if (!nzchar(h['highlight'])) set_header(highlight = .header.hi.tex)
  knit_hooks$set(hooks_latex())
}

#' @rdname output_hooks
#' @export
hooks_latex = function() {
  list(
    source = function(x, options) {
      x = hilight_source(x, 'latex', options)
      if (options$highlight) {
        if (options$engine == 'R' || x[1] != '\\noindent') {
          one_string(c('\\begin{alltt}', x, '\\end{alltt}', ''))
        } else {
          if ((n <- length(x)) > 4) x[n - 2] = sub('\\\\\\\\$', '', x[n - 2])
          one_string(c(x, ''))
        }
      } else .verb.hook(x)
    },
    output = function(x, options) {
      if (output_asis(x, options)) {
        paste0('\\end{kframe}', x, '\\begin{kframe}')
      } else .verb.hook(x)
    },
    warning = .color.block('\\color{warningcolor}{', '}'),
    message = .color.block('\\itshape\\color{messagecolor}{', '}'),
    error = .color.block('\\bfseries\\color{errorcolor}{', '}'),
    inline = .inline.hook.tex, chunk = .chunk.hook.tex,
    plot = function(x, options) {
      # escape plot environments from kframe
      paste0('\\end{kframe}', hook_plot_tex(x, options), '\n\\begin{kframe}')
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
  knit_hooks$set(hooks_sweave())
}

#' @rdname output_hooks
#' @export
hooks_sweave = function() {
  # wrap source code in the Sinput environment, output in Soutput
  hook.i = function(x, options) {
    one_string(
      c('\\begin{Sinput}', hilight_source(x, 'sweave', options), '\\end{Sinput}', ''))
  }
  hook.s = function(x, options) {
    if (output_asis(x, options)) return(x)
    paste0('\\begin{Soutput}\n', x, '\\end{Soutput}\n')
  }
  hook.c = function(x, options) {
    if (output_asis(x, options)) return(x)
    paste0('\\begin{Schunk}\n', x, '\\end{Schunk}')
  }
  list(source = hook.i, output = hook.s, warning = hook.s,
       message = hook.s, error = hook.s, plot = hook_plot_tex,
       inline = .inline.hook.tex, chunk = hook.c)
}

#' @rdname output_hooks
#' @export
render_listings = function() {
  opts_chunk$set(highlight = FALSE, comment = NA, prompt = FALSE) # mimic Sweave settings
  opts_knit$set(out.format = 'listings')
  test_latex_pkg('Sweavel', system.file('misc', 'Sweavel.sty', package = 'knitr'))
  set_header(framed = '', highlight = '\\usepackage{Sweavel}')
  knit_hooks$set(hooks_listings())
}

#' @rdname output_hooks
#' @export
hooks_listings = hooks_sweave

# may add textile, and many other markup languages

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
#' @param x A character string (the whole output document).
#' @return The post-processed document as a character string.
#' @note These functions are hackish. Also note \code{hook_movecode()} assumes
#'   you to use the default output hooks for LaTeX (not Sweave or listings), and
#'   every figure/table environment must have a label.
#' @export
#' @references \url{https://yihui.org/knitr/hooks/}
#' @examples \dontrun{knit_hooks$set(document = hook_movecode)}
#' # see example 103 at https://github.com/yihui/knitr-examples
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
    p = one_string(c(p[-idx], p[idx]))
    gsub('\\\\end\\{(kframe)\\}\\s*\\\\begin\\{\\1\\}', '', p)
  }), use.names = FALSE))

  res = split(x, cumsum(grepl('^\\\\(begin|end)\\{table\\}', x)))
  res = one_string(unlist(lapply(res, function(p) {
    if (length(p) <= 4 || !grepl('^\\\\begin\\{table\\}', p[1]) ||
          length(grep('% knitr_do_not_move', p)) ||
          !any(grepl('\\\\begin\\{(alltt|kframe)\\}', p))) return(p)
    if (!any(grepl('\\\\label\\{.*\\}', p))) return(p)
    idx = c(1, seq(grep('\\\\caption', p), grep('\\\\label', p)))
    i0 = grep('\\\\begin\\{tabular\\}', p); i1 = grep('\\\\end\\{tabular\\}', p)
    for (i in seq_along(i0)) idx = c(idx, i0[i]:i1[i])
    idx = sort(idx)
    p = one_string(c(p[-idx], p[idx]))
    gsub('\\\\end\\{(kframe)\\}\\s*\\\\begin\\{\\1\\}', '', p)
  }), use.names = FALSE))
  .rm.empty.envir(res)
}
