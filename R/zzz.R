.onLoad = function(lib, pkg) {
  if (R.version$`svn rev` >= 61843)
    tools::vignetteEngine('knitr', weave = vweave, tangle = vtangle)
  # use the env var R_KNITR_PROGRESS to control the progress bar
  opts_knit$set(progress = isTRUE(as.logical(Sys.getenv('R_KNITR_PROGRESS', TRUE))))
  # add /usr/texbin to PATH for Mac OS
  if (Sys.info()['sysname'] != 'Darwin') return()
  if (!file.exists(texbin <- '/usr/texbin')) return()
  path = strsplit(Sys.getenv('PATH'), ':')[[1L]]
  if (texbin %in% path) return()
  Sys.setenv(PATH = paste(c(path, texbin), collapse = ':'))
}
