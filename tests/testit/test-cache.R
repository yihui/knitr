library(testit)

assert('find_symbols() identifies all symbols', {
  (find_symbols('x = x + 1; rnorm(1, std = z)') %==% c('x', 'rnorm', 'z'))
})

knit_lazy = function(lazy = TRUE) {
  in_dir(tempdir(), {
    txt = c(sprintf('```{r test, cache=TRUE, cache.lazy=%s}', lazy),
            'x1 = Sys.time()', '```')
    knit(text = txt, quiet = TRUE)
    x2 = x1
    Sys.sleep(0.1)
    knit(text = txt, quiet = TRUE)
    x1 == x2  # x1 should not be updated
  })
}
assert('cache.lazy = TRUE/FALSE works', {
  (knit_lazy(TRUE))
  (knit_lazy(FALSE))
})

knit_code$set(a = 1, b = 2, c = 3)
assert('dep_prev() sets dependencies on previous chunks', {
  # dependency is empty now
  (dep_list$get() %==% list())
  # b/c depend on a, c depends on b
  dep_prev()
  (dep_list$get() %==% list(a = c('b', 'c'), b = 'c'))
})
dep_list$restore()
knit_code$restore()
