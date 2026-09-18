library(testit)

tangle_text = function(text, out.format = 'markdown') {
  optk = opts_knit$get()
  opts_knit$set(out.format = out.format)
  on.exit(opts_knit$set(optk), add = TRUE)
  purl(text = text)
}

# Test that when there is no pattern specified, no pattern found, and the file
# is tangled, purl() returns an empty string rather than the original text.
# https://github.com/yihui/knitr/pull/1660
assert('when LP pattern not found in the input, purl() returns an empty string', {
  (tangle_text('There is no code.') %==% '')
})

# https://github.com/yihui/knitr/issues/1753 read_chunk() is detected by parsing
# the code rather than string-matching, so calls that only look like read_chunk()
# no longer break purl()
assert('purl() does not choke on read_chunk-like code that is not a real call', {
  # a function named *read_chunk() and a commented-out call with an unbalanced
  # parenthesis inside a string used to crash purl() (parse error)
  code = c(
    '```{r}', "fake_read_chunk <- function(lines) 'Hi!'",
    "# fake_read_chunk(lines = ')')", '```'
  )
  (!has_error(tangle_text(code)))
})

assert('purl() reads external code via a genuine read_chunk() call', {
  code = c(
    '```{r setup}',
    "read_chunk(lines = c('## @knitr foo', 'x <- 1 + 1'))",
    '```', '', '```{r foo}', '```'
  )
  out = tangle_text(code)
  (any(grepl('x <- 1 + 1', out, fixed = TRUE)))
})

# https://github.com/yihui/knitr/issues/1938 YAML params are made available to
# chunk options during tangling, so a chunk with eval = params$foo is not dropped
assert('purl() resolves params referenced in chunk options (#1938)', {
  code = c(
    '---', 'params:', '  paramcd: TRUE', '---', '',
    '```{r, eval = params$paramcd}', 'print(params$paramcd)', '```'
  )
  out = tangle_text(code)
  # the params list is emitted, and the code chunk is kept (not dropped)
  (any(grepl('params <-', out, fixed = TRUE)))
  (any(grepl('print(params$paramcd)', out, fixed = TRUE)))

  # a chunk with eval = params$foo evaluating to FALSE is commented out but kept
  code2 = c(
    '---', 'params:', '  run: FALSE', '---', '',
    '```{r, eval = params$run}', 'x <- 1', '```'
  )
  out2 = tangle_text(code2)
  (any(grepl('# x <- 1', out2, fixed = TRUE)))

  # purl = params$foo (FALSE) drops the chunk entirely
  code3 = c(
    '---', 'params:', '  p: FALSE', '---', '',
    '```{r, purl = params$p}', 'y <- 2', '```', '', '```{r}', 'z <- 3', '```'
  )
  out3 = tangle_text(code3)
  (!any(grepl('y <- 2', out3, fixed = TRUE)))
  (any(grepl('z <- 3', out3, fixed = TRUE)))
})
