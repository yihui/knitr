.onLoad = function(lib, pkg) {
  if (getRversion() >= '3.0.0') {
    tools::vignetteEngine(
      'knitr', weave = vweave, tangle = vtangle,
      pattern = c('[.][rRsS](nw|tex)$', '[.][Rr](md|html)$'), package = pkg
    )
  }
  # use the option KNITR_PROGRESS to control the progress bar
  opts_knit$set(progress = getOption('KNITR_PROGRESS', TRUE))
  # add /usr/texbin to PATH for Mac OS
  if (Sys.info()['sysname'] != 'Darwin') return()
  if (!file.exists(texbin <- '/usr/texbin')) return()
  path = strsplit(Sys.getenv('PATH'), ':')[[1L]]
  if (texbin %in% path) return()
  Sys.setenv(PATH = paste(c(path, texbin), collapse = ':'))
}
