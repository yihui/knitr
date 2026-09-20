library(testit)

# see http://stackoverflow.com/q/18992260/559676 for the bug
assert('inline_exec only accept character result', {
  block = list(code = "function() 1", input = "inline `r function() 1`")
  res = xfun::try_silent(inline_exec(block, new.env()))
  (inherits(res, 'try-error'))
  block = list(code = "(function() 1)()", input = "inline `r (function() 1)()`")
  block$location = matrix(c(8,27), ncol = 2, byrow = TRUE)
  res = inline_exec(block, new.env())
  (res %==% "inline 1")
  block = list(code = character(0), input = "no inline")
  res = inline_exec(block, new.env())
  (res %==% "no inline")
})

assert('label_code correct adds comment on code for yaml block or parsed param', {
  oldW = getOption('width')
  options(width = 20)
  (label_code("1+1", list(params.src = "test, eval=TRUE")) %==% "## ----test, eval=TRUE----\n1+1\n")
  options(width = oldW)
  (label_code("1+1", list(params.chunk = c("#| label: test", "#| eval: true"))) %==%
      "## --------\n#| label: test\n#| eval: true\n1+1\n")
})

assert('the chunk option log.echo streams executing code to stderr (#2222)', {
  code = c('```{r log.echo=TRUE}', 'x <- 1', 'x + 1', '```')
  err = capture.output(out <- knit(text = code, quiet = TRUE), type = 'message')
  (all(c('x <- 1', 'x + 1') %in% err))
  # off by default: no code is logged to stderr
  code = c('```{r}', 'y <- 2', 'y + 2', '```')
  err = capture.output(out <- knit(text = code, quiet = TRUE), type = 'message')
  (!any(c('y <- 2', 'y + 2') %in% err))
})

assert('a numeric eval echoes non-evaluated lines without a comment mask (#2129)', {
  # line 1 is echoed but not evaluated: it must appear verbatim (no `## ` prefix),
  # and only line 2 is run (so `42` is in the output)
  out = knit(text = c('```{r, eval=2}', 'mtcars', '1 + 41', '```'), quiet = TRUE)
  (grepl('\n mtcars\n', out) || grepl('\nmtcars\n', out))
  (!grepl('## mtcars', out))
  (grepl('42', out))
  # a multi-line masked expression keeps all its lines unmasked
  out = knit(text = c('```{r, eval=2}', 'f <- function() {', '  1', '}', '1 + 41', '```'), quiet = TRUE)
  (!grepl('#<knitr>', out))
  (!grepl('## f <- function', out))
  (grepl('f <- function\\(\\) \\{', out))
  # a real comment on a masked line is preserved
  out = knit(text = c('```{r, eval=2}', 'x <- 1  # keep me', '1 + 41', '```'), quiet = TRUE)
  (grepl('# keep me', out))
  (!grepl('#<knitr>', out))
})
