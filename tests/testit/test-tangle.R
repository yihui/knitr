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
  (tangle_text('There is no code.')  %==% '')
})
