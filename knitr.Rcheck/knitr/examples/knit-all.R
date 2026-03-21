#!/usr/bin/env Rscript

if (!nzchar(Sys.which('lyx')) || system('lyx -version') != 0L) q()

call_lyx = function(file) {
  res = sapply(sprintf('lyx -e %s %s', c('knitr', 'r', 'pdf2'), file),
               system, ignore.stdout = TRUE, USE.NAMES = FALSE)
  unlink(sub('\\.lyx$', '.R', file))
  stopifnot(identical(res, integer(3L)))
}

for (i in list.files(pattern = '\\.lyx$')) {
  message(i)
  call_lyx(i)
  flush.console()
}

knit_script = normalizePath('../bin/knit', mustWork = TRUE)
knit_cmd = function(file) {
  paste('Rscript', shQuote(knit_script), shQuote(file))
}
test_cmd = function(cmd) {
  stopifnot(identical(system(cmd, ignore.stdout = TRUE), 0L))
}

for (i in list.files(pattern = '\\.Rmd')) {
  message(i)
  cmd = if (i == 'knitr-minimal.Rmd') {
    sprintf("Rscript -e 'library(knitr);opts_knit$set(base.url=\"http://animation.r-forge.r-project.org/ideas/\");knit(\"%s\")'", i)
  } else knit_cmd(i)
  test_cmd(cmd)
  flush.console()
}

test_cmd(sprintf("Rscript -e 'library(knitr);spin(\"knitr-spin.R\", precious=TRUE)'"))

setwd('child')
for (i in c('knitr-main.Rnw', 'knitr-parent.Rnw')) {
  test_cmd(knit_cmd(i))
}
unlink('*.tex')
test_cmd(knit_cmd('knitr-main.Rmd'))
setwd('..')

file.remove('child/knitr-main.html')
