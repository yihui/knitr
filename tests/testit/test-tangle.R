library(testit)

tangle_text = function(text, out.format = "markdown") {
  optk = opts_knit$get()
  opts_knit$set(out.format = out.format)
  on.exit(opts_knit$set(optk), add = TRUE)
  return(knit(text = text, tangle = TRUE))
}

# Test that when there is no pattern specified, no pattern found, and the
# file is tangled, knit() returns an empty string rather than the original text.
assert(
  identical(tangle_text("There is no code."), "")
)
