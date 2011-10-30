dev2ext = function(x) {
    ## graphics devices in base R, plus those in Cairo, cairoDevice, tikzDevice
    switch(x, bmp = 'bmp', postscript = 'eps', pdf = 'pdf', png = 'png', svg = 'svg',
           jpeg = 'jpeg', pictex = 'tex', tiff = 'tiff', win.metafile = 'wmf',
           cairo_pdf = 'pdf', cairo_ps = 'eps',

           CairoJPEG = 'jpeg', CairoPNG = 'png', CairoPS = 'eps', CairoPDF = 'pdf',
           CairoSVG = 'svg', CairoTIFF = 'tiff',

           Cairo_pdf = 'pdf', Cairo_png = 'png', Cairo_ps = 'eps',
           Cairo_svg = 'svg',

           tikz = 'tikz',

           stop('device name \'', x, '\' not supported yet; please go to ',
                'https://github.com/yihui/knit/issues to report this issue',
                call. = FALSE))
}

## save a recorded plot
save_plot = function(plot, name, options) {

    dev = options$dev; ext = options$fig.ext; dpi = options$dpi

    ## guess file type if NULL
    if (is.null(ext)) {
        ext = dev2ext(dev)
    }
    path = str_c(name, ".", ext)

    ## built-in devices
    device =
        switch(dev,
               bmp = function(...) bmp(...,  res = dpi, units = "in"),
               postscript = function(...) {
                   postscript(..., onefile = FALSE, horizontal = FALSE, paper = "special")
               },
               jpeg = function(...) jpeg(..., res = dpi, units = "in"),
               pdf = function(...) pdf(...),
               png = function(...) png(..., res = dpi, units = "in"),
               svg = function(...) svg(...),
               pictex = function(...) pictex(...),
               tiff = function(...) tiff(..., res = dpi, units = "in"),
               win.metafile = function(...) win.metafile(...),
               cairo_pdf = function(...) cairo_pdf(...),
               cairo_ps = function(...) cairo_ps(...),

               CairoJPEG = load_device('CairoJPEG', 'Cairo', dpi = dpi),
               CairoPNG = load_device('CairoPNG', 'Cairo', dpi = dpi),
               CairoTIFF = load_device('CairoTIFF', 'Cairo', dpi = dpi),
               CairoPS = load_device('CairoPS', 'Cairo'),
               CairoPDF = load_device('CairoPDF', 'Cairo'),
               CairoSVG = load_device('CairoSVG', 'Cairo'),

               Cairo_pdf = load_device('Cairo_pdf', 'cairoDevice'),
               Cairo_png = load_device('Cairo_png', 'cairoDevice'),
               Cairo_ps = load_device('Cairo_ps', 'cairoDevice'),
               Cairo_svg = load_device('Cairo_svg', 'cairoDevice'),

               tikz = function(...) {
                   if (do.call('require', list(package = 'tikzDevice'))) {
                       get('tikz', envir = as.environment('package:tikzDevice'))(...,
                                   sanitize = options$sanitize)
                   } else {
                       stop("package 'tikzDevice' not available (has to be installed)",
                            call. = FALSE)
                   }
               },

               get(dev))

    ## re-plot the recorded plot to an off-screen device
    device(path, width = options$width, height = options$height)
    print(plot)
    dev.off()

    invisible(c(name, ext))
}

## this is mainly for Cairo and cairoDevice
load_device = function(name, package, dpi = NULL) {
    if (do.call('require', list(package = package))) {
        dev = get(name, envir = as.environment(str_c('package:', package)))
        ## dpi is for bitmap devices; units must be inches!
        if (is.null(dpi)) {
            function(...) dev(...)
        } else function(...) dev(..., dpi = dpi, units = 'in')
    } else stop("package '", package, "' not available; please install it first")
}

## merge low-level plotting changes
merge_low_plot = function(x, idx) {
    idx = which(idx); n = length(idx); m = NULL # store indices that will be removed
    for (i in 1:(n - 1)) {
        if (is_low_change(x[[idx[i]]], x[[idx[i + 1]]])) m = c(m, idx[i])
    }
    if (is.null(m)) x else x[-m]
}

## compare two recorded plots
is_low_change = function(p1, p2) {
    p1 = p1[[1]]; p2 = p2[[1]]  # real plot info is in [[1]]
    if ((n2 <- length(p2)) < (n1 <- length(p1))) return(FALSE)  # length must increase
    identical(p1[1:n1], p2[1:n1])
}
