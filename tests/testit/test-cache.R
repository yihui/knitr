library(testit)

dep_list$restore()
knit_code$restore()


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

# knit_cache_pack()/knit_cache_unpack() customize how objects are cached; by
# default they leave the object unchanged
assert('knit_cache_pack()/knit_cache_unpack() default to identity', {
  (knit_cache_pack(1:5) %==% 1:5)
  (knit_cache_unpack('abc') %==% 'abc')
})

# register pack/unpack methods for a fake class that mimics an external-pointer
# object (packed to a plain value on save, restored on load)
registerS3method(
  'knit_cache_pack', 'refobj',
  function(x, ...) structure(list(v = unclass(x)$v), class = 'packed_refobj'),
  envir = asNamespace('knitr')
)
registerS3method(
  'knit_cache_unpack', 'packed_refobj',
  function(x, ...) structure(list(v = x$v), class = 'refobj'),
  envir = asNamespace('knitr')
)

assert('objects are cached and restored via knit_cache_pack()/knit_cache_unpack()', {
  d = tempfile('cache-pack'); dir.create(d, showWarnings = FALSE, recursive = TRUE)
  in_dir(d, {
    txt = c(
      '```{r test, cache=TRUE}', 'x1 = Sys.time()',
      'r = structure(list(v = 42), class = "refobj")', '```',
      'value: `r r$v`; class: `r class(r)`'
    )
    o1 = knit(text = txt, quiet = TRUE)
    x2 = x1
    Sys.sleep(0.1)
    o2 = knit(text = txt, quiet = TRUE)
    # second run is served from the cache (x1 not re-evaluated)
    (x1 == x2)
    # the restored object keeps its original class and value
    (grepl('value: 42; class: refobj', o2))
    (o1 %==% o2)
  })
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

assert('dep_auto() solves dependencies', {
  # dependency is empty now
  (dep_list$get() %==% list())

  # base rmd text
  rmd0 = c(
    '```{r, autodep=TRUE, cache=TRUE}',
    'x = %s',
    '```',
    '```{r, autodep=TRUE, cache=TRUE}',
    'print(x)',
    '```'
  )

  td = tempfile()
  dir.create(td, showWarnings = FALSE, recursive = TRUE)

  rmd1 = sprintf(rmd0, 'runif(1)')
  rmd2 = sprintf(rmd0, '"a"')

  # without child document
  in_dir(td, {
    # with cache, the result should reproduce
    knit1 = knit(text = rmd1, quiet = TRUE)
    (knit(text = rmd1, quiet = TRUE) %==% knit1)

    # on updating `x`, the printed result should change
    knit2 = knit(text = rmd2, quiet = TRUE)
    print2 = gsub('\n.*', '', gsub('.*\n##', '##', knit2))
    (print2 %==% '## [1] "a"')
  })
})
dep_list$restore()
knit_code$restore()

assert('dep_auto() solves dependencies of child documents', {
  # dependency is empty now
  (dep_list$get() %==% list())

  # base rmd text
  rmd0 = c(
    '```{r, autodep=TRUE, cache=TRUE}',
    'x = %s',
    '```',
    '```{r, autodep=TRUE, cache=TRUE}',
    'print(x)',
    '```'
  )
  rmd1 = sprintf(rmd0, 'runif(1)')
  rmd2 = sprintf(rmd0, '"a"')

  td = tempfile()
  dir.create(td, showWarnings = FALSE, recursive = TRUE)

  # with child document
  parent = c(
    '```{r, child="child.Rmd"}',
    '```'
  )
  in_dir(td, {
    # with cache, the result should reproduce
    writeLines(rmd1, 'child.Rmd')
    knit1 = knit(text = parent, quiet = TRUE)
    (knit(text = parent, quiet = TRUE) %==% knit1)

    # on updating `x`, the printed result should change
    writeLines(rmd2, 'child.Rmd')
    knit2 = knit(text = parent, quiet = TRUE)
    print2 = gsub('\n.*', '', gsub('.*\n##', '##', knit2))
    (print2 %==% '## [1] "a"')
  })
})
dep_list$restore()
knit_code$restore()
