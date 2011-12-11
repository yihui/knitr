## x is the output of processed document
insert_header = function(x) {
    if (!is.null(b <- knit_patterns$get('header.begin'))) {
        h = opts_knit$get('header')
        i = which(str_detect(x, b))[1]
        if (length(i) == 1) {
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
        }
    }
    x

}

## e.g. set_header(tikz = '\\usepackage{tikz}')
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
          boxes_latex()), collapse = '\n')
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
