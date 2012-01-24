dev2ext = function(x) {
    ## graphics devices in base R, plus those in Cairo, cairoDevice, tikzDevice
    switch(x, bmp = 'bmp', postscript = 'eps', pdf = 'pdf', png = 'png', svg = 'svg',
           jpeg = 'jpeg', pictex = 'tex', tiff = 'tiff', win.metafile = 'wmf',
           cairo_pdf = 'pdf', cairo_ps = 'eps',

           quartz_pdf = 'pdf', quartz_png = 'png', quartz_jpeg = 'jpeg',
           quartz_tiff = 'tiff', quartz_gif = 'gif', quartz_psd = 'psd', quartz_bmp = 'bmp',

           CairoJPEG = 'jpeg', CairoPNG = 'png', CairoPS = 'eps', CairoPDF = 'pdf',
           CairoSVG = 'svg', CairoTIFF = 'tiff',

           Cairo_pdf = 'pdf', Cairo_png = 'png', Cairo_ps = 'eps',
           Cairo_svg = 'svg',

           tikz = 'tikz',

           stop('device name \'', x, '\' not supported yet; please go to ',
                'https://github.com/yihui/knitr/issues to report this issue',
                call. = FALSE))
}

## quartiz devices under Mac
quartz_dev = function(type, dpi) {
    force(type); force(dpi)
    function(file, width, height) {
        quartz(file = file, width = width, height = height, type = type, dpi = dpi)
    }
}

## save a recorded plot
save_plot = function(plot, name, options) {

    dev = options$dev; ext = options$fig.ext; dpi = options$dpi
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

               quartz_pdf = quartz_dev('pdf', dpi),
               quartz_png = quartz_dev('png', dpi),
               quartz_jpeg = quartz_dev('jpeg', dpi),
               quartz_tiff = quartz_dev('tiff', dpi),
               quartz_gif = quartz_dev('gif', dpi),
               quartz_psd = quartz_dev('psd', dpi),
               quartz_bmp = quartz_dev('bmp', dpi),

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
                       packages =
                           switch(getOption("tikzDefaultEngine"),
                                  pdftex = getOption("tikzLatexPackages"),
                                  xetex = getOption("tikzXelatexPackages"))
                       get('tikz', envir = as.environment('package:tikzDevice'))(...,
                                   sanitize = options$sanitize,
                                   standAlone = options$external,
                                   packages = c('\n', packages, .knitEnv$tikzPackages))
                   } else {
                       stop("package 'tikzDevice' not available (has to be installed)",
                            call. = FALSE)
                   }
               },

               get(dev))

    ## re-plot the recorded plot to an off-screen device
    device(path, width = options$fig.width, height = options$fig.height)
    print(plot)
    dev.off()

    ## compile tikz to pdf
    if (dev == 'tikz' && options$external) {
        unlink(pdf.plot <- str_c(name, '.pdf'))
        owd = setwd(dirname(path))
        system(str_c(switch(getOption("tikzDefaultEngine"),
                            pdftex = getOption('tikzLatex'),
                            xetex = getOption("tikzXelatex"),
                            stop("a LaTeX engine must be specified for tikzDevice",
                                 call. = FALSE)), shQuote(basename(path)), sep = ' '))
        setwd(owd)
        if (file.exists(pdf.plot)) ext = 'pdf' else {
            stop('failed to compile ', path, ' to PDF', call. = FALSE)
        }
    }

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
        p1 = x[[idx[i]]]; p2 = x[[idx[i + 1]]]
        if (is_low_change(p1, p2)) {
            m = c(m, if (is_par_change(p1, p2)) idx[i + 1] else idx[i])
        }
    }
    if (is.null(m)) x else x[-m]
}

## compare two recorded plots
is_low_change = function(p1, p2) {
    p1 = p1[[1]]; p2 = p2[[1]]  # real plot info is in [[1]]
    if ((n2 <- length(p2)) < (n1 <- length(p1))) return(FALSE)  # length must increase
    identical(p1[1:n1], p2[1:n1])
}

plot_calls = evaluate:::plot_calls

## is the new plot identical to the old one except a few par/layout primitives in the end?
is_par_change = function(p1, p2) {
    n1 = length(prim1 <- plot_calls(p1))
    n2 = length(prim2 <- plot_calls(p2))
    if (n2 <= n1) return(TRUE)
    all(prim2[(n1 + 1):n2] %in% c('layout', 'par'))  # TODO: is this list exhaustive?
}
