# graphics devices in base R, plus those in Cairo, cairoDevice, tikzDevice
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

  svglite = 'svg',

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
  unname(res)
}

check_dev = function(dev) {
  if (exists(dev, mode = 'function', envir = knit_global()))
    get(dev, mode = 'function', envir = knit_global()) else
      stop('the graphical device', sQuote(dev), 'does not exist (as a function)')
}

# quartiz devices under Mac
quartz_dev = function(type, dpi) {
  force(type); force(dpi)
  function(file, width, height, ...) {
    grDevices::quartz(file = file, width = width, height = height, type = type, dpi = dpi, ...)
  }
}

# a wrapper of the tikzDevice::tikz device
tikz_dev = function(...) {
  loadNamespace('tikzDevice')
  packages = switch(
    getOption('tikzDefaultEngine'),
    pdftex = getOption('tikzLatexPackages'),
    xetex = getOption('tikzXelatexPackages'),
    luatex = getOption('tikzLualatexPackages')
  )
  tikzDevice::tikz(..., packages = c('\n\\nonstopmode\n', packages, .knitEnv$tikzPackages))
}

# save a recorded plot
save_plot = function(plot, name, dev, width, height, ext, dpi, options) {

  path = paste(name, ext, sep = '.')
  # when cache=2 and plot file exists, just return the filename
  if (options$cache == 2 && cache$exists(options$hash, options$cache.lazy)) {
    if (!file.exists(path)) {
      purge_cache(options)
      stop('cannot find ', path, '; the cache has been purged; please re-compile')
    }
    return(paste(name, if (dev == 'tikz' && options$external) 'pdf' else ext, sep = '.'))
  }

  # built-in devices
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

    svglite = load_device('svglite', 'svglite'),

    tikz = function(...) {
      tikz_dev(..., sanitize = options$sanitize, standAlone = options$external)
    },

    check_dev(dev)
  )
  in_base_dir(plot2dev(plot, name, dev, device, path, width, height, options))
}

plot2dev = function(plot, name, dev, device, path, width, height, options) {
  dargs = get_dargs(options$dev.args, dev)
  # re-plot the recorded plot to an off-screen device
  do.call(device, c(list(path, width = width, height = height), dargs))
  showtext(options$fig.showtext)  # showtext support
  print(plot)
  dev.off()

  # compile tikz to pdf
  if (dev == 'tikz' && options$external) {
    unlink(pdf.plot <- paste0(name, '.pdf'))
    owd = setwd(dirname(path))
    # add old wd to TEXINPUTS (see #188)
    oti = Sys.getenv('TEXINPUTS'); on.exit(Sys.setenv(TEXINPUTS = oti))
    Sys.setenv(TEXINPUTS = paste(owd, oti, sep = ':'))
    latex = switch(
      getOption('tikzDefaultEngine'),
      pdftex = getOption('tikzLatex'),
      xetex  = getOption('tikzXelatex'),
      luatex = getOption('tikzLualatex'),
      stop('a LaTeX engine must be specified for tikzDevice', call. = FALSE)
    )
    system2(latex, shQuote(basename(path)), stdout = NULL)
    setwd(owd)
    if (!file.exists(pdf.plot)) {
      if (file.exists(log <- paste(name, 'log', sep = '.')))
        message(paste(readLines(log), collapse = '\n'))
      stop('failed to compile ', path, ' to PDF', call. = FALSE)
    }
    path = pdf.plot
  }

  fig_process(options$fig.process, path)
}

# filter the dev.args option
get_dargs = function(dargs, dev) {
  if (length(dargs) == 0) return()
  if (is.list(dargs) && all(sapply(dargs, is.list))) {
    # dev.args is list(dev1 = list(arg1 = val1, ...), dev2 = list(arg2, ...))
    dargs = dargs[[dev]]
  }
  dargs
}

# this is mainly for Cairo and cairoDevice
load_device = function(name, package, dpi = NULL) {
  dev = getFromNamespace(name, package)
  # dpi is for bitmap devices; units must be inches!
  if (is.null(dpi)) dev else function(...) dev(..., dpi = dpi, units = 'in')
}


# merge low-level plotting changes
merge_low_plot = function(x, idx = sapply(x, evaluate::is.recordedplot)) {
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

# compare two recorded plots
is_low_change = function(p1, p2) {
  p1 = p1[[1]]; p2 = p2[[1]]  # real plot info is in [[1]]
  if (length(p2) < (n1 <- length(p1))) return(FALSE)  # length must increase
  identical(p1[1:n1], p2[1:n1])
}

# recycle some plot options such as fig.cap, out.width/height, etc when there
# are multiple plots per chunk
.recyle.opts = c('fig.cap', 'fig.scap', 'fig.env', 'fig.pos', 'fig.subcap',
                 'out.width', 'out.height', 'out.extra')

# when passing options to plot hooks, reduce the recycled options to scalars
reduce_plot_opts = function(options) {
  i = options$fig.cur %n% 1L
  for (o in .recyle.opts) {
    v = options[[o]]
    if ((n <- length(v)) == 0) next
    if ((j <- i %% n) == 0) j = n
    options[o] = list(v[j])
  }
  options
}

# the memory address of a NativeSymbolInfo object will be lost if it is saved to
# disk; see http://markmail.org/message/zat2r2pfsvhrsfqz for the full
# discussion; the hack below was stolen (with permission) from RStudio:
# https://github.com/rstudio/rstudio/blob/master/src/cpp/r/R/Tools.R
fix_recordedPlot = function(plot) {
  # restore native symbols for R >= 3.0
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
  attr(plot, 'pid') = Sys.getpid()
  plot
}

# fix plots in evaluate() results
fix_evaluate = function(list, fix = TRUE) {
  if (!fix) return(list)
  lapply(list, function(x) {
    if (evaluate::is.recordedplot(x)) fix_recordedPlot(x) else x
  })
}

# remove the plots from the evaluate results for the case of cache=2; if we only
# want to keep high-level plots, we need MD5 digests of the plot components so
# that we will be able to filter out low-level changes later
remove_plot = function(list, keep.high = TRUE) {
  lapply(list, function(x) {
    if (evaluate::is.recordedplot(x)) structure(
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

# a null device
pdf_null = function(width = 7, height = 7, ...) {
  grDevices::pdf(NULL, width, height, ...)
}

fig_process = function(FUN, path) {
  if (is.function(FUN)) {
    path2 = FUN(path)
    if (!is.character(path2) || length(path2) != 1L)
      stop("'fig.process' must be a function that returns a character string")
    path = path2
  }
  path
}

#' Crop a plot (remove the edges) using PDFCrop or ImageMagick
#'
#' The command \command{pdfcrop x x} is executed on a PDF plot file, and
#' \command{convert x -trim x} is executed for other types of plot files, where
#' \code{x} is the plot filename.
#'
#' The utility \command{pdfcrop} is often shipped with a LaTeX distribution, and
#' \command{convert} is a command in ImageMagick (Windows users may have to put
#' the bin path of ImageMagick into the \var{PATH} variable).
#' @param x the plot filename
#' @param quiet whether to suppress standard output from the command line
#'   utility
#' @export
#' @references PDFCrop: \url{http://pdfcrop.sourceforge.net}; the
#'   \command{convert} command in ImageMagick:
#'   \url{http://www.imagemagick.org/script/convert.php}
#' @return The original filename.
plot_crop = function(x, quiet = !opts_knit$get('progress')) {
  ext = tolower(file_ext(x))
  if (ext == 'pdf') {
    if (!has_utility('pdfcrop')) return(x)
  } else if (!has_utility('convert', 'ImageMagick')) return(x)

  if (!quiet) message('cropping ', x)
  x = shQuote(x)
  if (ext == 'pdf') {
    cmd = 'pdfcrop'
    args = c(x, x)
  } else {
    cmd = 'convert'
    args = c(x, '-trim', x)
  }
  # see this post for why use shell() on Windoz:
  # http://comments.gmane.org/gmane.comp.lang.r.devel/38113
  if (is_windows()) {
    shell(paste(c(cmd, args), collapse = ' '))  # no way to quiet cmd output on Windoz
  } else {
    system2(cmd, args = args, stdout = if (quiet) FALSE else "")
  }
  x
}

# a wrapper of showtext::showtext.begin()
showtext = function(show) {
  if (isTRUE(show)) getFromNamespace('showtext.begin', 'showtext')()
}

#' Embed external images in \pkg{knitr} documents
#'
#' When plots are not generated from R code, there is no way for \pkg{knitr} to
#' capture plots automatically. In this case, you may generate the images
#' manually and pass their file paths to this function to include them in the
#' output. The major advantage of using this function is that it is portable in
#' the sense that it works for all document formats that \pkg{knitr} supports,
#' so you do not need to think if you have to use, for example, LaTeX or
#' Markdown syntax, to embed an external image. Chunk options related to
#' graphics output that work for normal R plots also work for these images, such
#' as \code{out.width} and \code{out.height}.
#' @param path a character vector of image paths
#' @param auto_pdf whether to use PDF images automatically when the output
#'   format is LaTeX, e.g. \file{foo/bar.png} will be replaced by
#'   \file{foo/bar.pdf} if the latter exists; this can be useful since normally
#'   PDF images are of higher qualities than raster images like PNG when the
#'   output is LaTeX/PDF
#' @note This function is supposed to be used in R code chunks or inline R code
#'   expressions. You are recommended to use forward slashes (\verb{/}) as path
#'   separators instead of backslashes in the image paths.
#' @return The same as the input character vector \code{path} but it is marked
#'   with special internal S3 classes so that \pkg{knitr} will convert the file
#'   paths to proper output code according to the output format.
#' @export
include_graphics = function(path, auto_pdf = TRUE) {
  if (auto_pdf && is_latex_output()) {
    path2 = sub_ext(path, 'pdf')
    i = file.exists(path2)
    path[i] = path2[i]
  }
  structure(path, class = c('knit_image_paths', 'knit_asis'))
}

need_screenshot = function(x) {
  fmt = pandoc_to()
  # not R Markdown v2, always screenshot htmlwidgets and shiny apps
  if (length(fmt) == 0) return(inherits(x, c('htmlwidget', 'shiny.appobj')))
  html_format = fmt %in% c('html', 'html5', 'revealjs', 's5', 'slideous', 'slidy')
  (inherits(x, 'htmlwidget') && !html_format) ||
    (inherits(x, 'shiny.appobj') && !(html_format && runtime_shiny()))
}

runtime_shiny = function() {
  identical(opts_knit$get('rmarkdown.runtime'), 'shiny')
}

html_screenshot = function(x, options = opts_current$get(), ...) {
  i1 = inherits(x, 'htmlwidget')
  i2 = inherits(x, 'shiny.appobj')
  if (!(i1 || i2))
    stop('Screenshotting for the class ', class(x)[1], ' is not supported.')

  # if user has specified the screenshot image, just use it
  if (!is.null(shots <- options$alt.screenshot)) {
    i = shot_counter()
    if (length(shots) < i) stop('Not enough number of screenshots provided')
    return(include_graphics(shots[i]))
  }

  if (!requireNamespace('webshot', quietly = TRUE)) stop(
    'Please install the webshot package ',
    '(if not on CRAN, try devtools::install_github("wch/webshot"))'
  )

  ext = switch(options$dev, pdf = '.pdf', jpeg = '.jpeg', '.png')
  wargs = options$webshot.args %n% list()
  if (is.null(wargs$vwidth)) wargs$vwidth = options$out.width.px
  if (is.null(wargs$vheight)) wargs$vheight = options$out.height.px
  if (is.null(wargs$delay)) wargs$delay = if (i1) 0.2 else 1
  d = tempfile()
  dir.create(d); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f = in_dir(d, {
    if (i1) {
      f1 = tempfile('widget', '.', '.html')
      htmlwidgets::saveWidget(x, f1, FALSE, knitrOptions = options)
      f2 = tempfile('webshot', '.', ext)
      do.call(webshot::webshot, c(list(f1, f2), wargs))
      normalizePath(f2)
    } else if (i2) {
      f = tempfile('webshot', '.', ext)
      do.call(webshot::appshot, c(list(x, f), wargs))
      normalizePath(f)
    }
  })
  res = readBin(f, 'raw', file.info(f)[, 'size'])
  structure(list(image = res, extension = ext), class = 'html_screenshot')
}
