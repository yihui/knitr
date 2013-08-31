## graphics devices in base R, plus those in Cairo, cairoDevice, tikzDevice
auto_exts = c(
  bmp = 'bmp', postscript = 'eps', pdf = 'pdf', png = 'png', svg = 'svg',
  jpeg = 'jpeg', pictex = 'tex', tiff = 'tiff', win.metafile = 'wmf',
  cairo_pdf = 'pdf', cairo_ps = 'eps',

  quartz_pdf = 'pdf', quartz_png = 'png', quartz_jpeg = 'jpeg',
  quartz_tiff = 'tiff', quartz_gif = 'gif', quartz_psd = 'psd',
  quartz_bmp = 'bmp',

  CairoJPEG = 'jpeg', CairoPNG = 'png', CairoPS = 'eps', CairoPDF = 'pdf',
  CairoSVG = 'svg', CairoTIFF = 'tiff',

  Cairo_pdf = 'pdf', Cairo_png = 'png', Cairo_ps = 'eps', Cairo_svg = 'svg',

  tikz = 'tikz'
)

dev2ext = function(x) {
  res = auto_exts[x]
  if (any(idx <- is.na(res))) {
    for (i in x[idx]) check_dev(i)
    stop('cannot find appropriate filename extensions for device ', x[idx],
         "; please use chunk option 'fig.ext' (http://yihui.name/knitr/options)",
         call. = FALSE)
  }
  res
}

check_dev = function(dev) {
  if (exists(dev, mode = 'function', envir = knit_global()))
    get(dev, mode = 'function', envir = knit_global()) else
      stop('the graphical device', sQuote(dev), 'does not exist (as a function)')
}

## quartiz devices under Mac
quartz_dev = function(type, dpi) {
  force(type); force(dpi)
  function(file, width, height, ...) {
    grDevices::quartz(file = file, width = width, height = height, type = type, dpi = dpi, ...)
  }
}

# a wrapper of the tikzDevice::tikz device
tikz_dev = function(...) {
  suppressPackageStartupMessages(do.call('library', list('tikzDevice')))
  packages = switch(
    getOption('tikzDefaultEngine'),
    pdftex = getOption('tikzLatexPackages'),
    xetex = getOption('tikzXelatexPackages'),
    luatex = getOption('tikzLualatexPackages')
  )
  getFromNamespace('tikz', 'tikzDevice')(
    ..., packages = c('\n\\nonstopmode\n', packages, .knitEnv$tikzPackages)
  )
}

## save a recorded plot
save_plot = function(plot, name, dev, width, height, ext, dpi, options) {

  path = paste(name, ext, sep = '.')
  # when cache=2 and plot file exists, just return the filename
  if (options$cache == 2 && cache$exists(options$hash)) {
    if (!file.exists(path)) {
      purge_cache(options)
      stop('cannot find ', path, '; the cache has been purged; please re-compile')
    }
    return(c(name, if (dev == 'tikz' && options$external) 'pdf' else ext))
  }

  ## built-in devices
  device = switch(
    dev,
    bmp = function(...) bmp(...,  res = dpi, units = 'in'),
    postscript = function(...) {
      postscript(..., onefile = FALSE, horizontal = FALSE, paper = 'special')
    },
    jpeg = function(...) jpeg(..., res = dpi, units = 'in'),
    pdf = grDevices::pdf,
    png = function(...) png(..., res = dpi, units = 'in'),
    svg = grDevices::svg,
    pictex = grDevices::pictex,
    tiff = function(...) tiff(..., res = dpi, units = 'in'),
    win.metafile = grDevices::win.metafile,
    cairo_pdf = grDevices::cairo_pdf,
    cairo_ps = grDevices::cairo_ps,

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
      tikz_dev(..., sanitize = options$sanitize, standAlone = options$external)
    },

    check_dev(dev)
  )

  dargs = options$dev.args
  if (is.list(dargs) && length(options$dev) > 1L) {
    # dev.args is list(dev1 = list(arg1 = val1, ...), dev2 = list(arg2, ...))
    if (all(options$dev %in% names(dargs))) dargs = dargs[[dev]]
  }
  ## re-plot the recorded plot to an off-screen device
  do.call(device, c(list(path, width = width, height = height), dargs))
  print(plot)
  dev.off()

  ## compile tikz to pdf
  if (dev == 'tikz' && options$external) {
    unlink(pdf.plot <- paste(name, '.pdf', sep = ''))
    owd = setwd(dirname(path))
    # add old wd to TEXINPUTS (see #188)
    oti = Sys.getenv('TEXINPUTS'); on.exit(Sys.setenv(TEXINPUTS = oti))
    Sys.setenv(TEXINPUTS = paste(owd, oti, sep = ':'))
    system(paste(switch(getOption('tikzDefaultEngine'),
                        pdftex = getOption('tikzLatex'),
                        xetex = getOption('tikzXelatex'),
                        luatex = getOption('tikzLualatex'),
                        stop('a LaTeX engine must be specified for tikzDevice',
                             call. = FALSE)), shQuote(basename(path))),
           ignore.stdout = TRUE)
    setwd(owd)
    if (file.exists(pdf.plot)) ext = 'pdf' else {
      stop('failed to compile ', path, ' to PDF', call. = FALSE)
    }
  }

  c(name, ext)
}

## this is mainly for Cairo and cairoDevice
load_device = function(name, package, dpi = NULL) {
  dev = getFromNamespace(name, package)
  ## dpi is for bitmap devices; units must be inches!
  if (is.null(dpi)) dev else function(...) dev(..., dpi = dpi, units = 'in')
}


## merge low-level plotting changes
merge_low_plot = function(x, idx = sapply(x, is.recordedplot)) {
  idx = which(idx); n = length(idx); m = NULL # store indices that will be removed
  if (n <= 1) return(x)
  i1 = idx[1]; i2 = idx[2]  # compare plots sequentially
  for (i in 1:(n - 1)) {
    # remove the previous plot and move its index to the next plot
    if (is_low_change(x[[i1]], x[[i2]])) m = c(m, i1)
    i1 = idx[i + 1]
    i2 = idx[i + 2]
  }
  if (is.null(m)) x else x[-m]
}

## compare two recorded plots
is_low_change = function(p1, p2) {
  p1 = p1[[1]]; p2 = p2[[1]]  # real plot info is in [[1]]
  if ((n2 <- length(p2)) < (n1 <- length(p1))) return(FALSE)  # length must increase
  identical(p1[1:n1], p2[1:n1])
}

# recycle some plot options such as fig.cap, out.width/height, etc when there
# are multiple plots per chunk
.recyle.opts = c('fig.cap', 'fig.scap', 'fig.env', 'fig.pos', 'fig.subcap',
                 'out.width', 'out.height', 'out.extra')
recycle_plot_opts = function(options) {
  n = options$fig.num
  for (i in .recyle.opts) {
    if (length(options[[i]]) == 0L) next
    options[[i]] = rep(options[[i]], length.out = n)
  }
  options
}

# when passing options to plot hooks, reduce the recycled options to scalars
reduce_plot_opts = function(options) {
  if (options$fig.show == 'animate' || options$fig.num <= 1L) return(options)
  fig.cur = options$fig.cur
  for (i in .recyle.opts) options[[i]] = options[[i]][fig.cur]
  options
}

# the memory address of a NativeSymbolInfo object will be lost if it is saved to
# disk; see http://markmail.org/message/zat2r2pfsvhrsfqz for the full
# discussion; the hack below was stolen (with permission) from RStudio:
# https://github.com/rstudio/rstudio/blob/master/src/cpp/r/R/Tools.R
fix_recordedPlot = function(plot) {
  # restore native symbols for R >= 3.0
  if (Rversion >= '3.0.0') {
    for (i in seq_along(plot[[1]])) {
      # get the symbol then test if it's a native symbol
      symbol = plot[[1]][[i]][[2]][[1]]
      if (inherits(symbol, 'NativeSymbolInfo')) {
        # determine the dll that the symbol lives in
        name = symbol[[if (is.null(symbol$package)) 'dll' else 'package']][['name']]
        pkgDLL = getLoadedDLLs()[[name]]
        # reconstruct the native symbol and assign it into the plot
        nativeSymbol = getNativeSymbolInfo(
          name = symbol$name, PACKAGE = pkgDLL, withRegistrationInfo = TRUE
        )
        plot[[1]][[i]][[2]][[1]] <- nativeSymbol
      }
    }
  } else if (Rversion >= '2.14') {
    # restore native symbols for R >= 2.14
    try({
      for(i in seq_along(plot[[1]])) {
        if(inherits(plot[[1]][[i]][[2]][[1]], 'NativeSymbolInfo')) {
          nativeSymbol = getNativeSymbolInfo(plot[[1]][[i]][[2]][[1]]$name)
          plot[[1]][[i]][[2]][[1]] = nativeSymbol
        }
      }
    }, silent = TRUE)
  }
  plot
}

# fix plots in evaluate() results
fix_evaluate = function(list, fix = TRUE) {
  if (!fix) return(list)
  lapply(list, function(x) {
    if (is.recordedplot(x)) fix_recordedPlot(x) else x
  })
}

# remove the plots from the evaluate results for the case of cache=2; if we only
# want to keep high-level plots, we need MD5 digests of the plot components so
# that we will be able to filter out low-level changes later
remove_plot = function(list, keep.high = TRUE) {
  lapply(list, function(x) {
    if (is.recordedplot(x)) structure(
      if (keep.high) digest_plot(x) else NULL, class = 'recordedplot'
    ) else x
  })
}
# replace the content of the recorded plot with MD5 digests so that merge_plot()
# will still work, and this will also save disk space for the case of cache=2
digest_plot = function(x, level = 1) {
  if (!is.list(x) || level >= 3) return(digest::digest(x))
  lapply(x, digest_plot, level = level + 1)
}
