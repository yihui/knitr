#!/usr/bin/env Rscript

library(tweakr)

## automatic bib generation
cite = write_citation(c('animation', 'cacheSweave', 'evaluate', 'formatR', 'highlight', 'knitr', 'pgfSweave', 'rgl', 'tikzDevice'), NULL)
tweak_citation(cite, file = 'knitr-packages.bib')

## cheat Sweave by using a tex document which was already compiled by knitr
clean_lyx('knitr-manual.tex', prefix = '\\{.*knitr_vignettes_')
