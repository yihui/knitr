.onLoad = function(lib, pkg) {
  if (getRversion() >= '3.0.0') {
    tools::vignetteEngine(
      'knitr', weave = vweave, tangle = vtangle,
      pattern = '[.]([rRsS](nw|tex)|[Rr](md|html|rst))$', package = pkg
    )
  }
  # add /usr/texbin to PATH for Mac OS
  if (Sys.info()['sysname'] != 'Darwin') return()
  if (!file_test('-d', texbin <- '/usr/texbin')) return()
  path = strsplit(Sys.getenv('PATH'), ':')[[1L]]
  if (texbin %in% path) return()
  Sys.setenv(PATH = paste(c(path, texbin), collapse = ':'))
}
