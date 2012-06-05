context('knitr environments')

test_that('use a list as parent frame', {
  opts_knit$set(progress = FALSE)
  rm(list = ls())
  z = 5
  # evaluate in a new environment; should create an object in current envir
  with(list(y = 4:8), knit('knit-envir.Rmd', envir = new.env()))
  expect_false(exists('asdfqwerzxcv'))
  
  expect_error(knit('knit-envir.Rmd'))  # y is not found
  with(list(y = letters), knit('knit-envir.Rmd'))
  opts_knit$set(progress = TRUE)
})
