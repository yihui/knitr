# Wrappers to use in vignette building for R 3.0.0

vweave = vtangle = function(file, driver, syntax, encoding = '', quiet = FALSE, ...) {
  opts_chunk$set(error = FALSE)  # should not hide errors
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
  vig_engine('knitr', vweave, '[.]([rRsS](nw|tex)|[Rr](md|html|rst))$')
  vig_engine('docco_linear', vweave_docco_linear, '[.][Rr](md|markdown)$')
  vig_engine('docco_classic', vweave_docco_classic, '[.][Rr]mk?d$')
}
# all engines use the same tangle and package arguments, so factor them out
vig_engine = function(...) {
  tools::vignetteEngine(..., tangle = vtangle, package = 'knitr')
}
