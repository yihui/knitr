# Wrappers to use in vignette building for R 3.0.0

vweave = vtangle = function(file, driver, syntax, encoding = '', quiet = FALSE, ...) {
  opts_knit$set(stop_on_error = 2L)  # should not hide errors
  options(markdown.HTML.header = NULL)
  (if (grepl('\\.[Rr]md$', file)) knit2html else if (grepl('\\.[Rr]rst$', file)) knit2pdf else knit)(
    file, encoding = encoding, quiet = quiet, envir = globalenv()
  )
}

body(vtangle)[4L] = expression(purl(file, encoding = encoding, quiet = quiet))

vweave_docco_linear = vweave
body(vweave_docco_linear)[4L] = expression(knit2html(
  file, encoding = encoding, quiet = quiet, envir = globalenv(),
  template = system.file('misc', 'docco-template.html', package = 'knitr')
))

vweave_docco_classic = vweave
body(vweave_docco_classic)[4L] = expression(rocco(
  file, encoding = encoding, quiet = quiet, envir = globalenv()
))

Rversion = getRversion()

register_vignette_engines = function(pkg) {
  if (Rversion < '3.0.0') return()
  # the default engine
  tools::vignetteEngine(
    'knitr', weave = vweave, tangle = vtangle,
    pattern = '[.]([rRsS](nw|tex)|[Rr](md|html|rst))$', package = pkg
  )
  tools::vignetteEngine(
    'docco_linear', weave = vweave_docco_linear, tangle = vtangle,
    pattern = '[.][Rr]markdown$', package = pkg
  )
  tools::vignetteEngine(
    'docco_classic', weave = vweave_docco_classic, tangle = vtangle,
    pattern = '[.][Rr]mkd$', package = pkg
  )
}
