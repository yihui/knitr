#' @include themes.R
#' @include highlight.R

# doc is the output of processed document
insert_header = function(doc) {
  if (is.null(b <- knit_patterns$get('header.begin'))) return(doc)

  if (out_format('html'))
    return(insert_header_html(doc, b))
  if (out_format(c('latex', 'listings', 'sweave')))
    return(insert_header_latex(doc, b))
  doc
}

# Makes latex header with macros required for highlighting, tikz and framed
make_header_latex = function() {
  h = paste(c(
    sprintf('\\usepackage[%s]{graphicx}\\usepackage[%s]{color}',
            opts_knit$get('latex.options.graphicx') %n% '',
            opts_knit$get('latex.options.color') %n% ''),
    .header.maxwidth, opts_knit$get('header'),
    if (getOption('OutDec') != '.') '\\usepackage{amsmath}',
    if (out_format('latex')) '\\usepackage{alltt}'
  ), collapse = '\n')
  if (opts_knit$get('self.contained')) h else {
    writeLines(h, 'knitr.sty')
    '\\usepackage{knitr}'
  }
}

insert_header_latex = function(doc, b) {
  i = grep(b, doc)
  if (length(i) >= 1L) {
    # it is safer to add usepackage{upquote} before begin{document} than after
    # documentclass{article} because it must appear after usepackage{fontenc};
    # see this weird problem: http://stackoverflow.com/q/12448507/559676
    if (!out_format('listings') && length(j <- grep(p <- '(\\s*)(\\\\begin\\{document\\})', doc)[1L])) {
      doc[j] = sub(p, '\n\\\\IfFileExists{upquote.sty}{\\\\usepackage{upquote}}{}\n\\2', doc[j])
    }
    i = i[1L]
    l = stringr__str_locate(doc[i], b)
    for (k in seq_len(nrow(l))) {
      tmp = stringr__str_sub(doc[i], l[k, 1], l[k, 2])
      doc[i] <- stringr__str_sub_assign(doc[i],
                                        l[k, 1],
                                        l[k, 2],
                                        value = paste0(tmp, make_header_latex()))
    }
  } else if (parent_mode() && !child_mode()) {
    # in parent mode, we fill doc to be a complete document
    doc[1L] = paste(c(getOption('tikzDocumentDeclaration'), make_header_latex(),
                      .knitEnv$tikzPackages, '\\begin{document}', doc[1L]), collapse = '\n')
    doc[length(doc)] = paste(
      c(doc[length(doc)], .knitEnv$bibliography, '\\end{document}'), collapse = '\n'
    )
  }
  doc
}

make_header_html = function() {
  h = opts_knit$get('header')
  h = h[setdiff(names(h), c('tikz', 'framed'))]
  if (opts_knit$get('self.contained')) {
    paste(c('<style type="text/css">', h[['highlight']], '</style>',
            unlist(h[setdiff(names(h), 'highlight')])), collapse = '\n')
  } else {
    writeLines(h, 'knitr.css')
    '<link rel="stylesheet" href="knitr.css" type="text/css" />'
  }
}

insert_header_html = function(doc, b) {
  i = grep(b, doc)
  if (length(i) == 1L) {
    l = stringr__str_locate(doc[i], b)
    for (k in seq_len(nrow(l))) {
      tmp = stringr__str_sub(doc[i], l[k, 1], l[k, 2])
      doc[i] <- stringr__str_sub_assign(doc[i],
                                        l[k, 1],
                                        l[k, 2],
                                        value = paste0(tmp, make_header_html()))
    }
  }
  doc
}

#' Set the header information
#'
#' Some output documents may need appropriate header information. For example,
#' for LaTeX output, we need to write \samp{\\usepackage{tikz}} into the
#' preamble if we use tikz graphics; this function sets the header information
#' to be written into the output.
#'
#' By default, \pkg{knitr} will set up the header automatically. For example, if
#' the tikz device is used, \pkg{knitr} will add \samp{\\usepackage{tikz}} to
#' the LaTeX preamble, and this is done by setting the header component
#' \code{tikz} to be a character string: \code{set_header(tikz =
#' '\\usepackage{tikz}')}. Similary, when we highlight R code using the
#' \pkg{highlight} package (i.e. the chunk option \code{highlight = TRUE}),
#' \pkg{knitr} will set the \code{highlight} component of the header vector
#' automatically; if the output type is HTML, this component will be different
#' -- instead of LaTeX commands, it contains CSS definitions.
#'
#' For power users, all the components can be modified to adapt to a customized
#' type of output. For instance, we can change \code{highlight} to LaTeX
#' definitions of the \pkg{listings} package (and modify the output hooks
#' accordingly), so we can decorate R code using the \pkg{listings} package.
#' @param ... Header components; currently possible components are
#'   \code{highlight}, \code{tikz} and \code{framed}, which contain the
#'   necessary commands to be used in the HTML header or LaTeX preamble. Note that
#'   HTML output does not use the \code{tikz} and \code{framed} components, since
#'   they do not make sense in the context of HTML.
#' @return The header vector in \code{opts_knit} is set.
#' @export
#' @examples set_header(tikz = '\\usepackage{tikz}')
#' opts_knit$get('header')
set_header = function(...) {
  opts_knit$set(header = merge_list(opts_knit$get('header'), c(...)))
}

.default.sty = inst_dir('themes', 'default.css')
# header for Latex Syntax Highlighting
.header.hi.tex = theme_to_header_latex(.default.sty)$highlight
.knitr.sty = inst_dir('misc', 'knitr.sty')
.header.framed = paste(readLines(.knitr.sty), collapse = '\n')
# CSS for html syntax highlighting
.header.hi.html = theme_to_header_html(.default.sty)$highlight
rm(list = c('.default.sty', '.knitr.sty')) # do not need them any more

.header.sweave.cmd =
'\\newcommand{\\SweaveOpts}[1]{}  % do not interfere with LaTeX
\\newcommand{\\SweaveInput}[1]{} % because they are not real TeX commands
\\newcommand{\\Sexpr}[1]{}       % will only be parsed by R
'

.header.maxwidth =
'%% maxwidth is the original width if it is less than linewidth
%% otherwise use linewidth (to make sure the graphics do not exceed the margin)
\\makeatletter
\\def\\maxwidth{ %
  \\ifdim\\Gin@nat@width>\\linewidth
    \\linewidth
  \\else
    \\Gin@nat@width
  \\fi
}
\\makeatother
'
