library(testit)

rm(list = ls())
z = 5
assert(
  'a list can be used as the parent frame for knit()',
  # evaluate in a new environment; should create an object in current envir
  !has_error(with(list(y = 4:8), knit('knit-envir.Rmd', quiet = TRUE)))
)

env = new.env()
env$y = 1:3
knit('knit-envir.Rmd', envir = env, quiet = TRUE)
assert(
  'knit() creates objects in its envir argument',
  !exists('asdfqwerzxcv'), exists('asdfqwerzxcv', envir = env)
)

assert(
  'undefined external objects should cause errors',
  suppressMessages(has_error(knit('knit-envir.Rmd', quiet = TRUE))),  # y is not found
  !has_error(with(list(y = letters), knit('knit-envir.Rmd', quiet = TRUE)))
)

file.remove('knit-envir.md')
