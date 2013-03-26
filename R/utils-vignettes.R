# Wrappers to use in vignette building for R 3.0.0

vweave = vtangle = function(file, driver, syntax, encoding = '', quiet = FALSE, ...) {
  (if (grepl('\\.[Rr]md$', file)) knit2html else knit)(
    file, encoding = encoding, quiet = quiet
  )
}

body(vtangle)[2L] = expression(purl(file, encoding = encoding, quiet = quiet))
