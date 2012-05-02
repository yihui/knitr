## doc is the output of processed document
insert_header = function(doc) {
  if (is.null(b <- knit_patterns$get('header.begin'))) return(doc)

  fmt = opts_knit$get('out.format')
  if (fmt == 'html')
    return(insert_header_html(doc, b))
  if (fmt %in% c('latex', 'listings', 'sweave'))
    return(insert_header_latex(doc, b))
  doc
}

## Makes latex header with macros required for highlighting, tikz and framed
make_header_latex = function() {
  h = "\\usepackage{graphicx, color}"
  h = paste(c(h, .header.maxwidth, opts_knit$get('header')), collapse = "\n")
  if (opts_knit$get('self.contained')) h else {
    writeLines(h, 'knitr.sty')
    '\\usepackage{knitr}'
  }
}

insert_header_latex = function(doc, b) {
  i = which(str_detect(doc, b))
  if (length(i) == 1L) {
    l = str_locate(doc[i], b)
    tmp = str_sub(doc[i], l[, 1], l[, 2])
    str_sub(doc[i], l[,1], l[,2]) = str_c(tmp, "\n", make_header_latex())
  } else if (length(i) == 0L && parent_mode()) {
    # in parent mode, we fill doc to be a complete document
    doc = str_c(getOption('tikzDocumentDeclaration'), make_header_latex(),
                .knitEnv$packages, "\\begin{document}", doc, "\\end{document}")
  }
  doc
}

make_header_html = function() {
  h = opts_knit$get('header')[['highlight']]
  if (opts_knit$get('self.contained')){
    str_c('<style type="text/css">', h, '</style>', collapse = "\n")
  } else {
    writeLines(h, 'knitr.css')
    '<link rel="stylesheet" href="knitr.css" type="text/css" />'
  }
}

insert_header_html = function(doc, b) {
  i = which(str_detect(doc, b))
  if (length(i) == 1L) {
    l = str_locate(doc[i], b)
    tmp = str_sub(doc[i], l[, 1], l[, 2])
    str_sub(doc[i], l[,1], l[,2]) = str_c(tmp, "\n", make_header_html())
  }
  doc
}

#' Set the header information
#'
#' Some output documents may need appropriate header information, for example,
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
#' @param ... the header components; currently possible components are
#'   \code{highlight}, \code{tikz} and \code{framed}, which contain the
#'   necessary commands to be used in the HTML header or LaTeX preamble; note
#'   HTML output only uses the \code{highlight} component (the other two are
#'   ignored)
#' @return The header vector in \code{opts_knit} is set.
#' @export
#' @examples set_header(tikz = '\\usepackage{tikz}')
#' opts_knit$get('header')
set_header = function(...) {
  h = opts_knit$get('header')
  z = c(...)
  h[names(z)] = z
  opts_knit$set(header = h)
}

# where is the inst directory?
.inst.dir = file.path(c('..', '.'), 'inst')
.inst.dir = .inst.dir[file.exists(.inst.dir)]

.default.sty = file.path(.inst.dir, 'themes', 'default.css')
.default.sty = .default.sty[file.exists(.default.sty)][1L]
# header for Latex Syntax Highlighting
.header.hi.tex = paste(c('\\IfFileExists{upquote.sty}{\\usepackage{upquote}}{}',
                         theme_to_header_latex(.default.sty)$highlight),
                       collapse = '\n')
.knitr.sty = file.path(.inst.dir, 'misc', 'knitr.sty')
.knitr.sty = .knitr.sty[file.exists(.knitr.sty)][1L]
.header.framed = paste(readLines(.knitr.sty), collapse = "\n")
# CSS for html syntax highlighting
.header.hi.html = paste(theme_to_header_html(.default.sty)$highlight,
                        collapse = '\n')
rm(list = c('.inst.dir', '.default.sty', '.knitr.sty')) # do not need them any more

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
