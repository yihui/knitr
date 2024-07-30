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
