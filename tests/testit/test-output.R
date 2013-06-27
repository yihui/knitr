library(testit)

res = mapply(
  auto_out_name,
  c('abc.Rnw', 'abc.rnw', 'abc.rtex', 'abc.Rmd', 'abc.rhtm', 'abc.Rhtml', 'foo.abc.rhtml'),
  USE.NAMES = FALSE
)
assert(
  'auto_out_name() converts .Rfoo to .foo',
  identical(res, c('abc.tex', 'abc.tex', 'abc.tex', 'abc.md', 'abc.htm', 'abc.html', 'foo.abc.html'))
)

res = mapply(
  auto_out_name,
  c('abc.tex', '_knit_abc.tex', '_knit_abc.md', 'foo_knit_.html'),
  USE.NAMES = FALSE
)
assert(
  'auto_out_name() converts .tex/.unknown to .txt, and removes _knit_',
  identical(res, c('abc.txt', 'abc.tex', 'abc.md', 'foo.html')),
  identical(auto_out_name('foo.bar'), 'foo.txt')
)

assert(
  'chunks with include=FALSE should stop on error',
  has_error(knit(text = c('<<include=F>>=', '1+"a"', '@'), quiet = TRUE))
)

rm(res)
