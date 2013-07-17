.onLoad = function(lib, pkg) {
  register_vignette_engines(pkg)
  # add /usr/texbin to PATH for Mac OS
  if (Sys.info()['sysname'] != 'Darwin') return()
  if (!file_test('-d', texbin <- '/usr/texbin')) return()
  path = strsplit(Sys.getenv('PATH'), ':')[[1L]]
  if (texbin %in% path) return()
  Sys.setenv(PATH = paste(c(path, texbin), collapse = ':'))
}
