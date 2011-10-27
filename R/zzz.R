.onLoad = function(lib, pkg) {

    ## looking for interactive device; adapted from R source: src/library/grDevices/R/zzz.R
    dsp = Sys.getenv("DISPLAY")
    dev = if(.Platform$OS.type == "windows") {
        grDevices::windows
    } else if (.Platform$GUI == "AQUA" || ((!nzchar(dsp) || grepl("^/tmp/launch-", dsp))
                && .Call("makeQuartzDefault", PACKAGE = 'grDevices'))) {
        grDevices::quartz
    } else if (nzchar(dsp) && .Platform$GUI %in% c("X11", "Tk")) {
        grDevices::X11
    }

    opts_knit$set(screen.dev = dev)

    res = try(system("kpsewhich framed.sty", intern=TRUE), silent = TRUE)
    if (!inherits(res, 'try-error') && length(res))
        set_header(framed = '\\usepackage{framed}') else {
            warning("unable to find LaTeX package 'framed'; LaTeX output may be ugly")
        }
}
