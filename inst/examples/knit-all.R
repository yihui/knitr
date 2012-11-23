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

call_knit = function(cmd) {
  stopifnot(identical(system(cmd), 0L))
}
for (i in list.files(pattern = '\\.Rmd')) {
  message(i)
  cmd = if (i == 'knitr-minimal.Rmd') {
    sprintf("Rscript -e 'library(knitr);opts_knit$set(base.url=\"http://animation.r-forge.r-project.org/ideas/\");knit(\"%s\")'", i)
  } else {
    sprintf('knit %s', i)
  }
  call_knit(cmd)
  flush.console()
}

call_knit(sprintf("Rscript -e 'library(knitr);spin(\"knitr-spin.R\")'"))

setwd('child')
for (i in c('knitr-main.Rnw', 'knitr-parent.Rnw')) {
  call_knit(sprintf('knit %s --pdf', i))
}
unlink('*.tex')
call_knit(sprintf('knit %s', 'knitr-main.Rmd'))
setwd('..')

