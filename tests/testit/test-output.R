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

# a shortcut
k = function(text) {
  on.exit(opts_chunk$restore())
  knit(text = c('```{r}', text, '```'), quiet = TRUE, envir = parent.frame())
}
k(c(
  'knit_child(text = "```{r}\nopts_chunk$set(echo=FALSE)\n```", options=list())',
  'x1 = opts_chunk$get("echo")'
))
k(c(
  'knit_child(text = "```{r}\nopts_chunk$set(dpi=200)\n```", options=list(dpi=100))',
  'x2 = opts_chunk$get("dpi")'
))

assert(
  'using knit_child() does not reset global chunk options set in child documents',
  x1 == FALSE, x2 == 200
)
