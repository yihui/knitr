library(testit)

assert(
  'find_globals() identifies global variables',
  # nothing from outside environment
  identical(find_globals('x=1'), character(0)),
  # qwer must be created outside somewhere
  identical(find_globals('a=1; b=a; d=qwer'), 'qwer'),
  identical(find_globals('a=function(){f=2;g}'), 'g'),
  # y was assigned locally in z, but there is another y outside from nowhere
  identical(find_globals('z=function(){y=1};y'), 'y'),
  # more complicated cases: operators, subscripts, ...
  identical(find_globals(c('a=1%*%1%o%2 %in% d', 'b=d%%10+3%/%2-z[1:3]')), c('d', 'z'))
)

assert(
  'find_symbols() identifies all symbols',
  find_symbols('x = x + 1; rnorm(1, std = z)') %==% c('x', 'rnorm', 'z')
)

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
assert(
  'cache.lazy = TRUE/FALSE works',
  knit_lazy(TRUE), knit_lazy(FALSE)
)

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

mock_cache = (function() {
  noop_false = function(...) FALSE
  noop_true = function(...) TRUE
  list(
    available = noop_true, exists = noop_false, load = noop_false,
    save = noop_false, purge = noop_false
  )  # may return anything
})()
knit_engines$set(mock = function(...) "\n\nmock result\n\n")
cache_engines$set(mock = function(...) mock_cache)
knit_engine_cache = function() {
  in_dir(tempdir(), {
    txt = c(
      '```{mock test, cache=TRUE, cache.path=""}',
      'mock code',
      '```'
    )
    knit(text = txt, quiet = TRUE)
    R_cache_file = list.files(pattern = "RData$")
    t1 = file.mtime(R_cache_file)
    knit(text = txt, quiet = TRUE)
    t2 = file.mtime(R_cache_file)
    t1 != t2  # missing "mock" cache should invalidate R cache
  })
}
assert("missing external engine's cache invalidates R cache", {
  (knit_engine_cache())
})
knit_engines$delete('mock')
cache_engines$delete('mock')
