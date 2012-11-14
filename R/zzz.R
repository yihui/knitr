.onLoad = function(lib, pkg) {
  # add /usr/texbin to PATH for Mac OS
  if (Sys.info()['sysname'] != 'Darwin') return()
  if (!file.exists(texbin <- '/usr/texbin')) return()
  path = strsplit(Sys.getenv('PATH'), ':')[[1L]]
  if (texbin %in% path) return()
  Sys.setenv(PATH = paste(c(path, texbin), collapse = ':'))
}
