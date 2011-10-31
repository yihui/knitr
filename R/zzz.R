.onLoad = function(lib, pkg) {
    res = try(system("kpsewhich framed.sty", intern=TRUE), silent = TRUE)
    if (!inherits(res, 'try-error') && length(res))
        set_header(framed = '\\usepackage{framed}') else {
            warning("unable to find LaTeX package 'framed'; LaTeX output may be ugly")
        }
}
