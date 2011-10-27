## x is the output of processed document
insert_header = function(x) {
    if (!is.null(b <- knit_patterns$get('header.begin'))) {
        h = opts_knit$get('header')
        i = which(str_detect(x, b))[1]
        if (length(i) == 1) {
            out.type = opts_knit$get('out.type')
            if (identical('tex', out.type))
                h = c('\\usepackage{graphicx, color}', h)
            if (identical('html', out.type))
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

set_header_highlight = function() {
    out.type = opts_knit$get('out.type')
    if (is.null(out.type)) return()
    set_header(highlight = switch(out.type, tex = .hi.tex.header, html = .hi.html.header, ''))
}
