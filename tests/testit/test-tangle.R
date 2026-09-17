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
