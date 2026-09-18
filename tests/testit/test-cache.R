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

# process_cache() customizes how objects are cached; by default it leaves the
# object unchanged in both directions
assert('process_cache() defaults to identity', {
  (process_cache(1:5, pack = TRUE) %==% 1:5)
  (process_cache('abc', pack = FALSE) %==% 'abc')
})

# register a method for a fake class that mimics an external-pointer object
# (packed to a plain value on save, restored on load); the same generic handles
# both directions via the pack flag, dispatching on the object's class
registerS3method(
  'process_cache', 'refobj',
  function(x, pack = TRUE, ...) structure(list(v = unclass(x)$v), class = 'packed_refobj'),
  envir = asNamespace('knitr')
)
registerS3method(
  'process_cache', 'packed_refobj',
  function(x, pack = TRUE, ...) structure(list(v = x$v), class = 'refobj'),
  envir = asNamespace('knitr')
)

assert('objects are cached and restored via process_cache()', {
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
  # ancestors are the transitive closure of what a chunk depends on
  (sort(dep_ancestors('c')) %==% c('a', 'b'))
  (dep_ancestors('b') %==% 'a')
  (dep_ancestors('a') %==% character())
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

assert('a cached chunk can depend on an uncached chunk (#1039)', {
  # chunk A is not cached; B is cached and depends on A. Changing A's code must
  # invalidate B's cache, even though A leaves no cache of its own. B appends to
  # a log file whenever it is actually evaluated, so the number of lines in the
  # log tells us whether B ran or was served from the cache (the printed value
  # of `x` alone cannot distinguish a cache hit from a re-run when x is unchanged)
  rmd = function(a) c(
    '```{r A}', sprintf('x <- %s', a), '```',
    '```{r B, cache=TRUE, dependson="A"}',
    'cat("ran\\n", file = "runs.log", append = TRUE)', 'print(x)', '```'
  )
  runs = function() if (file.exists('runs.log')) length(readLines('runs.log')) else 0L
  out = function(o) gsub('.*## ', '', gsub('\n.*', '', gsub('.*\n##', '##', o)))

  td = tempfile(); dir.create(td, showWarnings = FALSE, recursive = TRUE)
  # capture the results inside in_dir(), but do the checks at the top level of
  # the assert() body: assert() only recognizes checks written as (...) that are
  # not nested inside a function call such as in_dir()
  res = in_dir(td, {
    o1 = out(knit(text = rmd('1'), quiet = TRUE)); r1 = runs()
    o2 = out(knit(text = rmd('1'), quiet = TRUE)); r2 = runs()
    o3 = out(knit(text = rmd('99'), quiet = TRUE)); r3 = runs()
    list(o1 = o1, r1 = r1, o2 = o2, r2 = r2, o3 = o3, r3 = r3)
  })
  (res$o1 %==% '[1] 1')
  (res$r1 %==% 1L)  # B evaluated once
  (res$o2 %==% '[1] 1')
  (res$r2 %==% 1L)  # A unchanged: B served from cache, not re-evaluated
  (res$o3 %==% '[1] 99')
  (res$r3 %==% 2L)  # A changed: cache invalidated, B re-evaluated
})
dep_list$restore()
knit_code$restore()
