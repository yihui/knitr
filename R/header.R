## x is the output of processed document
insert_header = function(x) {
    if (is.null(b <- knit_patterns$get('header.begin'))) return(x)
    h = opts_knit$get('header')
    i = which(str_detect(x, b))
    if (length(i) == 1L) {
        fmt = opts_knit$get('out.format')
        if (fmt %in% c('markdown', 'gfm', 'jekyll')) return(x)
        if (identical('latex', fmt))
            h = c('\\usepackage{graphicx, color}', h)
        if (identical('html', fmt))
            h = h['highlight']
        h = h[nzchar(h)]; if (length(h) == 0) h = ''
        loc = str_locate(x[i], b)
        str_sub(x[i], loc[, 1], loc[, 2]) =
            str_c(str_sub(x[i], loc[, 1], loc[, 2]), '\n', str_c(h, collapse = '\n'))
    } else if (length(i) == 0L) {
        if (parent_mode()) {
            h = c('\\usepackage{graphicx, color}', h)
            x = c(getOption('tikzDocumentDeclaration'), str_c(h, collapse = '\n'),
                 .knitEnv$tikzPackages, '\\begin{document}', x, '\\end{document}')
        }
    }
    x
}

##' Set the header information
##'
##' Some output documents may need appropriate header information, for
##' example, for LaTeX output, we need to write
##' \samp{\\usepackage{tikz}} into the preamble if we use tikz
##' graphics; this function sets the header information to be written
##' into the output.
##'
##' By default, \pkg{knitr} will set up the header automatically. For
##' example, if the tikz device is used, \pkg{knitr} will add
##' \samp{\\usepackage{tikz}} to the LaTeX preamble, and this is done
##' by setting the header component \code{tikz} to be a character
##' string: \code{set_header(tikz = '\\usepackage{tikz}')}. Similary,
##' when we highlight R code using the \pkg{highlight} package
##' (i.e. the chunk option \code{highlight = TRUE}), \pkg{knitr} will
##' set the \code{highlight} component of the header vector
##' automatically; if the output type is HTML, this component will be
##' different -- instead of LaTeX commands, it contains CSS
##' definitions.
##'
##' For power users, all the components can be modified to adapt to a
##' customized type of output. For instance, we can change
##' \code{highlight} to LaTeX definitions of the \pkg{listings}
##' package (and modify the output hooks accordingly), so we can
##' decorate R code using the \pkg{listings} package.
##' @param ... the header components; currently possible components
##' are \code{highlight}, \code{tikz} and \code{framed}, which contain
##' the necessary commands to be used in the HTML header or LaTeX
##' preamble; note HTML output only uses the \code{highlight}
##' component (the other two are ignored)
##' @return The header vector in \code{opts_knit} is set.
##' @export
##' @examples set_header(tikz = '\\usepackage{tikz}')
##' opts_knit$get('header')
set_header = function(...) {
    h = opts_knit$get('header')
    z = c(...)
    h[names(z)] = z
    opts_knit$set(header = h)
}

## many thanks to Donald Arseneau
.header.framed = '\\usepackage{framed}
\\makeatletter
\\newenvironment{kframe}{%
 \\def\\FrameCommand##1{\\hskip\\@totalleftmargin \\hskip-\\fboxsep
 \\colorbox{shadecolor}{##1}\\hskip-\\fboxsep
     % There is no \\@totalrightmargin, so:
     \\hskip-\\linewidth \\hskip-\\@totalleftmargin \\hskip\\columnwidth}%
 \\MakeFramed {\\advance\\hsize-\\width
   \\@totalleftmargin\\z@ \\linewidth\\hsize
   \\@setminipage}}%
 {\\par\\unskip\\endMakeFramed}
\\makeatother

\\newenvironment{knitrout}{}{} % an empty environment to be redefined in TeX
'

## LaTeX styles for highlight
.header.hi.tex =
    str_c(c("\\newcommand{\\hlnumber}[1]{\\textcolor[rgb]{0,0,0}{#1}}%",
            "\\newcommand{\\hlfunctioncall}[1]{\\textcolor[rgb]{.5,0,.33}{\\textbf{#1}}}%",
            "\\newcommand{\\hlstring}[1]{\\textcolor[rgb]{.6,.6,1}{#1}}%",
            "\\newcommand{\\hlkeyword}[1]{\\textbf{#1}}%",
            "\\newcommand{\\hlargument}[1]{\\textcolor[rgb]{.69,.25,.02}{#1}}%",
            "\\newcommand{\\hlcomment}[1]{\\textcolor[rgb]{.18,.6,.34}{#1}}%",
            "\\newcommand{\\hlroxygencomment}[1]{\\textcolor[rgb]{.44,.48,.7}{#1}}%",
            "\\newcommand{\\hlformalargs}[1]{\\hlargument{#1}}%",
            "\\newcommand{\\hleqformalargs}[1]{\\hlargument{#1}}%",
            "\\newcommand{\\hlassignement}[1]{\\textbf{#1}}%",
            "\\newcommand{\\hlpackage}[1]{\\textcolor[rgb]{.59,.71,.145}{#1}}%",
            "\\newcommand{\\hlslot}[1]{\\textit{#1}}%",
            "\\newcommand{\\hlsymbol}[1]{#1}%",
            "\\newcommand{\\hlprompt}[1]{\\textcolor[rgb]{.5,.5,.5}{#1}}%",
            boxes_latex(), "\\definecolor{fgcolor}{rgb}{0,0,0}"), collapse = '\n')

.header.hi.html =
    str_c(c('<style type="text/css">', '.knitr {
	background-color: #F7F7F7;
}', '.error {
	font-weight: bold;
	color: #FF0000;
}', '.warning {
	font-weight: bold;
}', '.message {
	font-style: italic;
}', '.source, .output, .warning, .error, .message {
	padding: 0.5em 1em;
}', styler('default'), '</style>'), collapse = '\n')
