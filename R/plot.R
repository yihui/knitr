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
  if (any(idx <- is.na(res)))
    stop('cannot find appropriate filename extensions for device ', x[idx],
         "; please use chunk option 'fig.ext' (http://yihui.name/knitr/options)",
         call. = FALSE)
  res
}

## quartiz devices under Mac
quartz_dev = function(type, dpi) {
  force(type); force(dpi)
  function(file, width, height, ...) {
    quartz(file = file, width = width, height = height, type = type, dpi = dpi,
           ...)
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
  get('tikz', envir = as.environment('package:tikzDevice'))(
    ..., packages = c('\n\\nonstopmode\n', packages, .knitEnv$tikzPackages)
  )
}

## save a recorded plot
save_plot = function(plot, name, dev, ext, dpi, options) {

  path = str_c(name, ".", ext)

  ## built-in devices
  device = switch(
    dev,
    bmp = function(...) bmp(...,  res = dpi, units = "in"),
    postscript = function(...) {
      postscript(..., onefile = FALSE, horizontal = FALSE, paper = "special")
    },
    jpeg = function(...) jpeg(..., res = dpi, units = "in"),
    pdf = grDevices::pdf,
    png = function(...) png(..., res = dpi, units = "in"),
    svg = grDevices::svg,
    pictex = grDevices::pictex,
    tiff = function(...) tiff(..., res = dpi, units = "in"),
    win.metafile = function(...) win.metafile(...),
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

    get(dev, mode = 'function')
  )

  dargs = options$dev.args
  if (is.list(dargs) && length(options$dev) > 1L) {
    # dev.args is list(dev1 = list(arg1 = val1, ...), dev2 = list(arg2, ...))
    if (all(options$dev %in% names(dargs))) dargs = dargs[[dev]]
  }
  ## re-plot the recorded plot to an off-screen device
  do.call(device, c(list(path, width = options$fig.width, height = options$fig.height), dargs))
  print(plot)
  dev.off()

  ## compile tikz to pdf
  if (dev == 'tikz' && options$external) {
    unlink(pdf.plot <- str_c(name, '.pdf'))
    owd = setwd(dirname(path))
    # add old wd to TEXINPUTS (see #188)
    oti = Sys.getenv('TEXINPUTS'); on.exit(Sys.setenv(TEXINPUTS = oti))
    Sys.setenv(TEXINPUTS = str_c(owd, oti, sep = ':'))
    system(str_c(switch(getOption("tikzDefaultEngine"),
                        pdftex = getOption('tikzLatex'),
                        xetex = getOption("tikzXelatex"),
                        luatex = getOption("tikzLualatex"),
                        stop("a LaTeX engine must be specified for tikzDevice",
                             call. = FALSE)), shQuote(basename(path)), sep = ' '),
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
  do.call('library', list(package = package))
  dev = get(name, envir = as.environment(str_c('package:', package)))
  ## dpi is for bitmap devices; units must be inches!
  if (is.null(dpi)) dev else function(...) dev(..., dpi = dpi, units = 'in')
}


## filter out plot objects purely for layout (raised by par(), layout())

# layout() results in plot_calls() of length 1 under R >= 2.16; all calls are
# par/layout for par()/layout() under R <= 2.15, and are .External2 for R >=
# 2.16; these blank plot objects should be removed
rm_blank_plot = function(res) {
  Filter(function(x) {
    !is.recordedplot(x) ||
      identical(pc <- plot_calls(x), 'recordGraphics') ||
      identical(pc, 'persp') ||
      (length(pc) > 1L && !all(pc %in% c('par', 'layout', '.External2')))
  }, res)
}

## merge low-level plotting changes
merge_low_plot = function(x, idx = sapply(x, is.recordedplot)) {
  idx = which(idx); n = length(idx); m = NULL # store indices that will be removed
  i1 = idx[1]; i2 = idx[2]  # compare plots sequentially
  for (i in 1:(n - 1)) {
    p1 = x[[i1]]; p2 = x[[i2]]
    if (is_low_change(p1, p2)) {
      # if the next plot only differs with the previous plot by par() changes,
      # remove the next plot and keep the previous fixed, otherwise remove the
      # previous and move its index to the next plot
      if (is_par_change(p1, p2)) r = i2 else {
        r = i1; i1 = idx[i + 1]
      }
      m = c(m, r)
    } else i1 = idx[i + 1]
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

plot_calls = evaluate:::plot_calls

## is the new plot identical to the old one except a few par/layout primitives in the end?
is_par_change = function(p1, p2) {
  n1 = length(prim1 <- plot_calls(p1))
  n2 = length(prim2 <- plot_calls(p2))
  if (n2 <= n1) return(TRUE)
  all(prim2[(n1 + 1):n2] %in% c('layout', 'par', '.External2'))  # TODO: is this list exhaustive?
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
