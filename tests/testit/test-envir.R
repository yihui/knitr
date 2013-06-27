library(testit)

rm(list = ls())
z = 5
env = new.env()

assert(
  'a list can be used as the parent frame for knit()',
  # evaluate in a new environment; should create an object in current envir
  !has_error(with(list(y = 4:8), knit('knit-envir.Rmd', envir = env, quiet = TRUE)))
)

assert(
  'knit() creates objects in its envir argument',
  !exists('asdfqwerzxcv'), exists('asdfqwerzxcv', envir = env)
)

assert(
  'undefined external objects should cause errors',
  has_error(knit('knit-envir.Rmd')),  # y is not found
  !has_error(with(list(y = letters), knit('knit-envir.Rmd')))
)

rm(env); rm(z)
