## x is the output of processed document
insert_header = function(x) {
    if (!is.null(b <- knit_patterns$get('header.begin'))) {
        h = opts_knit$get('header')
        i = which(str_detect(x, b))[1]
        if (length(i) == 1) {
            theme = opts_knit$get('theme')
            if (identical('latex', theme))
                h = c('\\usepackage{graphicx, color}', h)
            if (identical('html', theme))
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


.header.framed = '\\usepackage{framed}
\\newenvironment{kframe}{%
\\def\\FrameCommand{\\fboxsep=\\FrameSep \\colorbox{shadecolor}}%
\\MakeFramed {\\advance\\hsize-\\width \\FrameRestore}}%
{\\endMakeFramed}'

## LaTeX styles for highlight
.header.hi.tex = str_c(c(styler('default', 'sty', styler_assistant_latex), boxes_latex()), collapse = '\n')
.header.hi.html = str_c(styler('default'), collapse = '\n')
