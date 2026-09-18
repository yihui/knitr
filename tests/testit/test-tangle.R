library(testit)

tangle_text = function(text, out.format = 'markdown') {
  optk = opts_knit$get()
  opts_knit$set(out.format = out.format)
  on.exit(opts_knit$set(optk), add = TRUE)
  purl(text = text)
}

# tangle to a bare R script (no roxygen documentation), for testing how chunk
# options such as `eval` and `comment` affect the code written to the script
purl0 = function(text) purl(text = text, documentation = 0L)

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

# an eval=FALSE chunk is commented out by default, but comment='' (or NA) keeps
# the code uncommented so it remains runnable from the script (#2425)
assert('purl() can keep eval=FALSE code uncommented via comment=""', {
  (purl0(c('```{r, eval=FALSE}', 'x <- 1', '```')) %==% '# x <- 1')
  (purl0(c('```{r, eval=FALSE, comment=""}', 'x <- 1', '```')) %==% 'x <- 1')
  (purl0(c('```{r, eval=FALSE, comment=NA}', 'x <- 1', '```')) %==% 'x <- 1')
})

# conversely, an explicit comment prefix comments out the code even when the
# chunk is evaluated (eval=TRUE), e.g. to keep a record of code that was run but
# should not be re-run from the tangled script (#1352)
assert('purl() comments out evaluated code when comment prefix is set', {
  # default: evaluated code is kept uncommented
  (purl0(c('```{r}', 'x <- 1', '```')) %==% 'x <- 1')
  # comment='#' comments it out despite eval=TRUE
  (purl0(c('```{r, comment="#"}', 'x <- 1', '```')) %==% '# x <- 1')
})

# option hooks are now applied during tangling, so a hook can decide which chunks
# to keep in the tangled script based on other options such as the label (#1903)
assert('purl() runs option hooks so a label hook can set purl', {
  opts_hooks$set(label = function(options) {
    options$purl = grepl('-solution$', options$label)
    options
  })
  on.exit(opts_hooks$restore(), add = TRUE)
  out = purl(text = c(
    '```{r iris}', 'plot(iris)', '```', '',
    '```{r car-solution}', 'plot(cars)', '```'
  ), documentation = 0L)
  (out %==% 'plot(cars)')
})
