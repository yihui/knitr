# graphics devices in base R, plus those in Cairo, tikzDevice
auto_exts = c(
  bmp = 'bmp', postscript = 'eps', pdf = 'pdf', png = 'png', svg = 'svg',
  jpeg = 'jpeg', pictex = 'tex', tiff = 'tiff', win.metafile = 'wmf',
  cairo_pdf = 'pdf', cairo_ps = 'eps',

  quartz_pdf = 'pdf', quartz_png = 'png', quartz_jpeg = 'jpeg',
  quartz_tiff = 'tiff', quartz_gif = 'gif', quartz_psd = 'psd',
  quartz_bmp = 'bmp',

  CairoJPEG = 'jpeg', CairoPNG = 'png', CairoPS = 'eps', CairoPDF = 'pdf',
  CairoSVG = 'svg', CairoTIFF = 'tiff',

  svglite = 'svg', gridSVG = 'svg',

  ragg_png = 'png',

  tikz = 'tex'
)

# choose a file extension according to the device, unless fig.ext is provided
dev2ext = function(options) {
  if (length(ext <- options$fig.ext)) return(ext)
  x = options$dev
  res = auto_exts[x]
  if (any(idx <- is.na(res))) {
    for (i in x[idx]) dev_get(i)
    stop2(
      'cannot find appropriate filename extensions for device ', x[idx], '; ',
      "please use chunk option 'fig.ext' (https://yihui.org/knitr/options)"
    )
  }
  unname(res)
}

# test if a device is available (and remember it), e.g., dev_available('png',
# png); capabilities() gives similar results for some devices but this function
# is more general
dev_available = local({
  res = list()
  function(name, fun = dev_get(name)) {
    if (!is.null(res[[name]])) return(res[[name]])
    res[[name]] <<- tryCatch({
      f = tempfile(); on.exit(unlink(f))
      fun(f, width = 5, height = 5)
      grDevices::dev.off()
      TRUE
    }, error = function(e) FALSE)
  }
})

dev_get = function(dev, options = opts_current$get(), dpi = options$dpi[1]) {
  if (is.null(dpi) || is.na(dpi)) dpi = 72
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
    gridSVG = function(filename, width, height, ...) {
      # use svg() only for redrawing the plot, and will use gridSVG::grid.export() later
      grDevices::svg(filename, width, height)
    },
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

    svglite = load_device('svglite', 'svglite'),

    # similar to load_device(), but the `dpi` argument is named `res`
    ragg_png = function(...) {
      ragg_png_dev(..., res = dpi, units = 'in')
    },

    tikz = function(...) {
      tikz_dev(..., sanitize = options$sanitize, standAlone = options$external)
    }
  )
  if (!is.null(device)) return(device)
  # custom device provided by user as a character string
  if (!exists(dev, mode = 'function', envir = knit_global()))
    stop('The graphical device ', sQuote(dev), ' was not found (as a function).')
  get(dev, mode = 'function', envir = knit_global())
}

# quartz devices under Mac
quartz_dev = function(type, dpi) {
  force(type); force(dpi)
  function(file, width, height, ...) {
    grDevices::quartz(file = file, width = width, height = height, type = type, dpi = dpi, ...)
  }
}

# a wrapper of the tikzDevice::tikz device
tikz_dev = function(..., engine = getOption('tikzDefaultEngine')) {
  loadNamespace('tikzDevice')
  packages = switch(
    engine,
    pdftex = getOption('tikzLatexPackages'),
    xetex = getOption('tikzXelatexPackages'),
    luatex = getOption('tikzLualatexPackages')
  )
  tikzDevice::tikz(..., packages = c('\n\\nonstopmode\n', packages, .knitEnv$tikzPackages))
}

# a wrapper of the ragg::agg_png device
ragg_png_dev = function(...) {
  loadNamespace('ragg')
  args = list(...)
  # handle bg -> background gracefully
  args$background = args$background %n% args$bg
  args$bg = NULL
  do.call(ragg::agg_png, args)
}

# save a recorded plot
save_plot = function(plot, name, dev, width, height, ext, dpi, options) {

  path = paste(name, ext, sep = '.')
  # when cache=2 and plot file exists, just return the filename
  if (options$cache == 2 && cache_exists(options)) {
    if (in_base_dir(!file.exists(path))) {
      purge_cache(options)
      stop('cannot find ', path, '; the cache has been purged; please re-compile')
    }
    return(paste(name, if (dev == 'tikz' && options$external) 'pdf' else ext, sep = '.'))
  }
  device = dev_get(dev, options, dpi)
  in_base_dir(plot2dev(plot, name, dev, device, path, width, height, options))
}

plot2dev = function(plot, name, dev, device, path, width, height, options) {
  dargs = get_dargs(options$dev.args, dev)
  # re-plot the recorded plot to an off-screen device
  do.call(device, c(list(path, width = width, height = height), dargs))
  showtext(options)  # maybe begin showtext and set options
  print(plot)
  # hack: if the device is gridSVG, save the plot to a temp path (with suffix ~)
  path2 = if (dev == 'gridSVG') paste0(path, '~')
  if (!is.null(path2)) do.call(gridSVG::grid.export, c(list(name = path2), dargs))
  dev.off()
  # move the temp svg file to `path`
  if (!is.null(path2)) file.rename(path2, path)

  # Cairo::CairoPS always adds the extension .ps, even if you have specified an
  # extension like .eps (https://github.com/yihui/knitr/issues/1364)
  if (dev == 'CairoPS') {
    path2 = paste0(path, '.ps')
    if (file.exists(path2)) file.rename(path2, path)
  }

  # compile tikz to pdf
  if (dev == 'tikz') {
    patch_tikz_tex(path)
    if (options$external) path = tinytex::latexmk(
      path, dargs$engine %n% getOption('tikzDefaultEngine')
    )
  }

  fig_process(options$fig.process, path, options)
}

# the tikz plot may contain raster legends, in which case we need to adjust the
# paths of these PNG files: https://stackoverflow.com/a/58410965/559676
patch_tikz_tex = function(path) {
  d = dirname(path)
  p = list.files(d, r <- '_ras[0-9]+[.]png$')
  b = sans_ext(basename(path)); bs = sub(r, '', p)
  # for foo.tex, there must be foo_ras[N].png, otherwise no need to patch foo.tex
  if (!(b %in% bs)) return()
  x = read_utf8(path)
  r = '^(\\s*\\\\pgfimage\\[.+?]\\{)(.+?_ras[0-9]+}};\\s*)$'
  i = grep(r, x)
  i = i[grep(b, x[i], fixed = TRUE)]
  x1 = sub(r, '\\1', x[i])
  x2 = sub(r, '\\2', x[i])
  if (is_windows()) d = gsub('\\\\', '/', d)
  x[i] = paste0(x1, d, '/', x2)
  write_utf8(x, path)
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

# this is mainly for Cairo
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

#' Compare two recorded plots
#'
#' Check if one plot only contains a low-level update of another plot.
#' @param p1,p2 Plot objects.
#' @return Logical value indicating whether \code{p2} is a low-level update of
#'   \code{p1}.
#' @export
#' @examples
#' pdf(NULL)
#' dev.control('enable')  # enable plot recording
#' plot(1:10)
#' p1 = recordPlot()
#' abline(0, 1)  # add a line (a low-level change)
#' p2 = recordPlot()
#' plot(rnorm(100))
#' p3 = recordPlot()  # draw a completely new plot
#' dev.off()
#' knitr::is_low_change(p1, p2)  # true
#' knitr::is_low_change(p1, p3)  # false
is_low_change = function(p1, p2) {
  UseMethod('is_low_change')
}

#' @export
is_low_change.default = function(p1, p2) {
  p1 = p1[[1]]; p2 = p2[[1]]  # real plot info is in [[1]]
  if (length(p2) < (n1 <- length(p1))) return(FALSE)  # length must increase
  identical(p1[1:n1], p2[1:n1])
}

# recycle some plot options such as fig.cap, out.width/height, etc when there
# are multiple plots per chunk
.recyle.opts = c('fig.cap', 'fig.scap', 'fig.alt', 'fig.env', 'fig.pos', 'fig.subcap',
                 'out.width', 'out.height', 'out.extra', 'fig.link')

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
  if (!is.list(x) || level >= 3) return(digest(x))
  lapply(x, digest_plot, level = level + 1)
}

# a null device
pdf_null = function(width = 7, height = 7, ...) {
  grDevices::pdf(NULL, width, height, ...)
}

fig_process = function(FUN, path, options) {
  if (is.function(FUN)) {
    ARG = intersect(c("options", names(options)), names(formals(FUN)))
    path2 = do.call(FUN, c(path, c(options = list(options), options)[ARG]))
    if (!is.character(path2) || length(path2) != 1L)
      stop("'fig.process' must be a function that returns a character string")
    path = path2
  }
  path
}

#' Crop a plot (remove the edges) using PDFCrop or ImageMagick
#'
#' The program \command{pdfcrop} (often shipped with a LaTeX distribution) is
#' executed on a PDF plot file, and
#' \code{magick::\link[magick:transform]{image_trim}()} is executed for other
#' types of plot files.
#'
#' The program \command{pdfcrop} can crop the extra white margins when the plot
#' format is PDF, to make better use of the space in the output document,
#' otherwise we often have to struggle with \code{graphics::\link{par}()} to set
#' appropriate margins. Note \command{pdfcrop} often comes with a LaTeX
#' distribution such as TinyTeX, MiKTeX, or TeX Live, and you may not need to
#' install it separately (use \code{Sys.which('pdfcrop')} to check it; if it not
#' empty, you are able to use it). Note that \command{pdfcrop} depends on
#' GhostScript. You can check if GhostScript is installed via
#' \code{tools::find_gs_cmd()}.
#' @param x Filename of the plot.
#' @param quiet Whether to suppress standard output from the command.
#' @export
#' @references PDFCrop: \url{https://www.ctan.org/pkg/pdfcrop}. If you use
#'   TinyTeX, you may install \command{pdfcrop} with
#'   \code{tinytex::tlmgr_install('pdfcrop')}.
#' @return The original filename.
plot_crop = function(x, quiet = TRUE) {
  is_pdf = grepl('[.]pdf$', x, ignore.case = TRUE)
  x2 = x
  x = path.expand(x)
  if (is_pdf && !has_utility('pdfcrop') && !has_utility('ghostscript')) return(x2)

  if (!quiet) message('cropping ', x)
  if (is_pdf) {
    system2('pdfcrop', shQuote(c(x, x)), stdout = if (quiet) FALSE else "")
  } else if (loadable('magick')) {
    img = magick::image_read(x)
    magick::image_write(magick::image_trim(img), x)
  } else message(
    'The magick package is required to crop "', x2, '" but not available.'
  )
  x2
}

showtext = function(options) {
  if (!isTRUE(options$fig.showtext)) return()
  showtext::showtext_opts(dpi = options$dpi)
  showtext::showtext_begin()
}

# handle some special cases of par()
par2 = function(x) {
  if (length(x) == 0) return()
  # this may not be correct, but there is no way to tell if the user set mfrow
  # or mfcol in par() (either setting will change mfrow/mfcol simultaneously),
  # and I just assume it was mfrow
  if (!is.null(x$mfrow)) {
    # do this before the rest of pars because setting mfrow/mfcol will reset cex
    par(mfrow = x$mfrow)
    x$mfrow = x$mfcol = NULL
    # do not restore mfg (#1215); this is unlikely to make sense, e.g. for
    # par(mfrow = c(1, 2)), mfg will be c(1, 2, 1, 2) when the second plot is
    # completed, but when the user generate a new plot, it should typically be
    # drawn at (1, 1) instead of (1, 2)
    x$mfg = NULL
  }
  if (!is.null(x$fg)) {
    # set fg before the rest of the par because
    # it resets col to the same value #1603
    par(fg = x$fg)
    x$fg = NULL
  }
  # you are unlikely to want to reset these pars
  x$fig = x$fin = x$pin = x$plt = x$usr = NULL
  x$ask = NULL  # does not make sense for typical non-interactive R sessions
  par(x)
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
#' @param path A character vector of image paths. Both local file paths and web
#'   paths are supported. Note that the \code{auto_pdf} and \code{dpi} arguments
#'   are not supported for web paths.
#' @param auto_pdf Whether to use PDF images automatically when the output
#'   format is LaTeX. If \code{TRUE}, then e.g. \file{foo/bar.png} will be
#'   replaced by \file{foo/bar.pdf} if the latter exists. This can be useful
#'   since normally PDF images are of higher quality than raster images like
#'   PNG, when the output is LaTeX/PDF.
#' @param dpi DPI (dots per inch) value. Used to calculate the output width (in
#'   inches) of the images. This will be their actual width in pixels, divided
#'   by \code{dpi}. If not provided, the chunk option \code{dpi} is used; if
#'   \code{NA}, the output width will not be calculated.
#' @param rel_path Whether to automatically convert absolute paths to relative
#'   paths. If you know for sure that absolute paths work, you may set this
#'   argument or the global option \code{knitr.graphics.rel_path} to
#'   \code{FALSE}.
#' @param error Whether to signal an error if any files specified in the
#'   \code{path} argument do not exist and are not web resources.
#' @note This function is supposed to be used in R code chunks or inline R code
#'   expressions. For local images, you are recommended to use relative paths
#'   with forward slashes instead of backslashes (e.g., \file{images/fig1.png}
#'   instead of \file{/Users/me/code/images/fig1.png}).
#'
#'   The automatic calculation of the output width requires the \pkg{png}
#'   package (for PNG images) or the \pkg{jpeg} package (for JPEG images). The
#'   width will not be calculated if the chunk option \code{out.width} is
#'   already provided or \code{dpi = NA}.
#' @return The same as the input character vector \code{path} but it is marked
#'   with special internal S3 classes so that \pkg{knitr} will convert the file
#'   paths to proper output code according to the output format.
#' @export
include_graphics = function(
  path, auto_pdf = getOption('knitr.graphics.auto_pdf', FALSE), dpi = NULL,
  rel_path = getOption('knitr.graphics.rel_path', TRUE),
  error = getOption('knitr.graphics.error', TRUE)
) {
  path = native_encode(path)  # https://d.cosx.org/d/420524
  if (any(i <- xfun::is_abs_path(path)) && rel_path && !is.null(d <- opts_knit$get('output.dir'))) {
    path[i] = xfun::relative_path(path[i], d, error = FALSE)
    if (any(j <- xfun::is_abs_path(path[i]))) warning(
      'It is highly recommended to use relative paths for images. ',
      'You had absolute paths: ', quote_vec(path[i][j])
    )
  }
  path = path.expand(path) # https://github.com/rstudio/rmarkdown/issues/1053
  if (auto_pdf && is_latex_output()) {
    path2 = with_ext(path, 'pdf')
    i = file.exists(path2)
    path[i] = path2[i]
  }
  # relative paths can be tricky in child documents, so don't error (#1957)
  if (child_mode()) error = FALSE
  if (error && length(p <- path[!xfun::is_web_path(path) & !file.exists(path)])) stop(
    'Cannot find the file(s): ', quote_vec(p)
  )
  structure(path, class = c('knit_image_paths', 'knit_asis'), dpi = dpi)
}

# calculate the width in inches for PNG/JPEG images given a DPI
raster_dpi_width = function(path, dpi) {
  if (!file.exists(path) || is.na(dpi)) return()
  is_png = grepl('[.]png$', path, ignore.case = TRUE)
  is_jpg = grepl('[.]jpe?g$', path, ignore.case = TRUE)
  if (!is_png && !is_jpg) return()
  if (is_png) {
    if (!loadable('png')) return()
    meta = attr(png::readPNG(path, native = TRUE, info = TRUE), 'info')
    w = meta$dim[1]
    if (!is.numeric(dpi)) dpi = meta$dpi[1]
    if (!is.numeric(dpi)) return()  # cannot calculate the desired width
  } else if (is_jpg) {
    if (!loadable('jpeg')) return()
    if (!is.numeric(dpi)) return()  # there is no dpi info in JPEG
    w = ncol(jpeg::readJPEG(path, native = TRUE))
  }
  if (is_latex_output()) {
    paste0(round(w / dpi, 2), 'in')
  } else if (is_html_output()) {
    round(w / (dpi / 96))
  }
}

#' Embed a URL as an HTML iframe or a screenshot in \pkg{knitr} documents
#'
#' When the output format is HTML, \code{include_url()} inserts an iframe in the
#' output; otherwise it takes a screenshot of the URL and insert the image in
#' the output. \code{include_app()} takes the URL of a Shiny app and adds
#' \samp{?showcase=0} to it (to disable the showcase mode), then passes the URL
#' to \code{include_url()}.
#' @param url A character vector of URLs.
#' @param height A character vector to specify the height of iframes.
#' @return An R object with a special class that \pkg{knitr} recognizes
#'   internally to generate the iframes or screenshots.
#' @seealso \code{\link{include_graphics}}
#' @export
include_url = function(url, height = '400px') {
  include_url2(url, height)
}

include_url2 = function(url, height = '400px', orig = url) {
  structure(
    list(url = url, height = height, url.orig = orig),
    class = c('knit_embed_url', 'knit_asis_url')
  )
}

#' @rdname include_url
#' @export
include_app = function(url, height = '400px') {
  orig = url  # store the original URL
  i = !grepl('?', url, fixed = TRUE)
  url[i] = paste0(url[i], '?showcase=0')
  include_url2(url, height, orig)
}

need_screenshot = function(x, ...) {
  options = list(...)[['options']]
  # if users specify screenshot.force = FALSE, skip screenshot and render HTML
  if (isFALSE(options$screenshot.force)) return(FALSE)
  # force screenshotting even if the output format support HTML
  force = is.list(options) && isTRUE(options$screenshot.force)
  fmt = pandoc_to()
  i1 = inherits(x, 'htmlwidget')
  i2 = inherits(x, 'shiny.appobj')
  i3 = inherits(x, 'knit_embed_url')
  # not R Markdown v2, always screenshot htmlwidgets and shiny apps
  if (length(fmt) == 0 || force) return(i1 || i2 || i3)
  html_format = fmt %in% c('html', 'html4', 'html5', 'revealjs', 's5', 'slideous', 'slidy')
  res = ((i1 || i3) && !html_format) || (i2 && !(html_format && runtime_shiny()))
  res && any(webshot_available())
}

runtime_shiny = function() {
  identical(opts_knit$get('rmarkdown.runtime'), 'shiny')
}

webshot_available = local({
  res = NULL  # cache the availability of webshot2/Chrome and webshot/PhantomJS
  test = function(pkg, fun, pkg2 = pkg) {
    loadable(pkg) && tryCatch(
      file.exists(getFromNamespace(fun, pkg2)()),
      error = function(e) FALSE
    )
  }
  function() {
    if (is.null(res)) res <<- c(
      webshot2 = test('webshot2', 'find_chrome', 'chromote'),
      webshot  = test('webshot',  'find_phantom')
    )
    res
  }
})

html_screenshot = function(x, options = opts_current$get(), ...) {
  i1 = inherits(x, 'htmlwidget')
  i2 = inherits(x, 'shiny.appobj')
  i3 = inherits(x, 'knit_embed_url')
  if (!(i1 || i2 || i3))
    stop('Screenshotting for the class ', class(x)[1], ' is not supported.')

  # if user has specified the screenshot image, just use it
  if (!is.null(shots <- options$screenshot.alt)) {
    i = shot_counter()
    if (length(shots) < i) stop('Not enough number of screenshots provided')
    return(structure(list(file = shots[i]), class = 'html_screenshot'))
  }

  ext = if (length(options$dev)) {
    switch(options$dev[1], pdf = '.pdf', jpeg = '.jpeg', '.png')
  } else '.png'
  wargs = options$screenshot.opts %n% list()
  if (is.null(wargs$vwidth)) wargs$vwidth = options$out.width.px
  if (is.null(wargs$vheight)) wargs$vheight = options$out.height.px
  if (is.null(wargs$delay)) wargs$delay = if (i1) 0.2 else 1
  d = tempfile()
  dir.create(d); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  w = webshot_available()
  webshot = c(options$webshot, names(w)[w])
  webshot = if (length(webshot) == 0) 'webshot' else webshot[[1L]]
  f = in_dir(d, {
    if (i1 || i3) {
      if (i1) {
        f1 = wd_tempfile('widget', '.html')
        save_widget(x, f1, FALSE, options = options)
      } else f1 = x$url
      f2 = wd_tempfile('webshot', ext)
      f3 = do.call(getFromNamespace('webshot', webshot), c(list(f1, f2), wargs))
      normalizePath(f3)
    } else if (i2) {
      f1 = wd_tempfile('webshot', ext)
      f2 = do.call(getFromNamespace('appshot', webshot), c(list(x, f1), wargs))
      normalizePath(f2)
    }
  })
  lapply(f, function(filename) {
    res = xfun::read_bin(filename)
    structure(
      list(image = res, extension = ext, url = if (i3) x$url.orig[filename == f]),
      class = 'html_screenshot'
    )
  })
}

save_widget = function(..., options) {
  FUN = htmlwidgets::saveWidget
  if ('knitrOptions' %in% names(formals(FUN))) {
    FUN(..., knitrOptions = options)
  } else FUN(...)
}
