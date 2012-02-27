#!/usr/bin/env Rscript

if (!nzchar(Sys.which('lyx')) || system('lyx -version') != 0L) q()

call_lyx = function(file) {
  res = sapply(sprintf('lyx -e %s %s', c('knitr', 'r', 'pdf2'), file), 
               system, USE.NAMES = FALSE)
  unlink(sub('\\.lyx$', '.R', file))
  stopifnot(identical(res, integer(3L)))
}

for (i in list.files(pattern = '\\.lyx$')) {
  message(i)
  call_lyx(i)
  flush.console()
}

for (i in list.files(pattern = '_knit_\\.')) {
  message(i)
  stopifnot(identical(system(sprintf('knit %s', i)), 0L))
  flush.console()
}

