library(testit)

pkgs = c(rownames(installed.packages(priority = 'high')), 'evaluate', 'knitr')
write_bib(pkgs, tempfile(), tweak = FALSE)

assert(
  '& is escaped in title when write_bib(tweak = TRUE)',
  length(grep(' & ', grep(
    '^  title =', capture.output(write_bib(pkgs, tweak = TRUE)), value = TRUE
  ))) == 0
)
