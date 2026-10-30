library(testit)

knit_md = function(text, ...) knit(text = text, quiet = TRUE, ...)

# a mock executor that records its arguments, then delegates to do.call()
new_recorder = function() {
  calls = list()
  ex = function(fun, args, profile) {
    calls[[length(calls) + 1L]] <<- list(fun = fun, args = args, profile = profile)
    do.call(fun, args)
  }
  list(ex = ex, calls = function() calls)
}

assert('compute: true forwards profile = NULL; compute: gpu forwards the profile name', {
  rec = new_recorder()
  options(compute.executor = rec$ex)
  knit_md(c("```{r}", "#| compute: true", "1 + 1", "```"))
  knit_md(c("```{r}", "#| compute: gpu", "1 + 1", "```"))
  options(compute.executor = NULL)
  calls = rec$calls()
  (length(calls) == 2L)
  (identical(calls[[1L]]$fun, eng_r_remote))
  (is.null(calls[[1L]]$profile))
  (calls[[2L]]$profile == 'gpu')
  # the worker half receives the code, the evaluate() arguments, and the options
  (identical(names(calls[[1L]]$args), c('code', 'ev_args', 'options')))
})

assert('untagged chunks do not reach the executor', {
  rec = new_recorder()
  options(compute.executor = rec$ex)
  out = knit_md(c("```{r}", "1 + 1", "```"))
  options(compute.executor = NULL)
  (length(rec$calls()) == 0L)
  (grepl('2', out))
})

assert('custom knit_print works through a routed chunk', {
  assign('knit_print.kn_test', function(x, ...) paste0('KN-TEST: ', x), envir = globalenv())
  options(compute.executor = function(fun, args, profile) do.call(fun, args))
  out = knit_md(c("```{r}", "#| compute: true", 'structure("hello", class = "kn_test")', "```"))
  options(compute.executor = NULL)
  rm('knit_print.kn_test', envir = globalenv())
  (grepl('KN-TEST: hello', out, fixed = TRUE))
})

assert('log_echo is forwarded in ev_args for a routed log.echo chunk', {
  rec = new_recorder()
  options(compute.executor = rec$ex)
  knit_md(c("```{r}", "#| compute: true", "#| log.echo: true", "1 + 1", "```"))
  knit_md(c("```{r}", "#| compute: true", "1 + 1", "```"))
  options(compute.executor = NULL)
  calls = rec$calls()
  (calls[[1L]]$args$ev_args$log_echo == TRUE)
  (calls[[2L]]$args$ev_args$log_echo == FALSE)
})

assert('cache = TRUE still applies to routed chunks', {
  rec = new_recorder()
  options(compute.executor = rec$ex)
  src = c(
    "```{r}", "#| compute: true", "#| cache: true", "#| cache.path: 'kn-test-cache/'",
    "1 + 1", "```"
  )
  invisible(knit_md(src))
  invisible(knit_md(src))
  options(compute.executor = NULL)
  unlink('kn-test-cache', recursive = TRUE)
  # on the cache hit the chunk is not re-sent to the worker
  (length(rec$calls()) == 1L)
})

assert('an invalid compute value warns and evaluates locally', {
  rec = new_recorder()
  options(compute.executor = rec$ex)
  w = NULL
  out = withCallingHandlers(
    knit_md(c("```{r}", "#| compute: 123", "1 + 1", "```")),
    warning = function(x) {
      w <<- conditionMessage(x)
      invokeRestart('muffleWarning')
    }
  )
  options(compute.executor = NULL)
  (grepl("invalid 'compute' chunk option", w, fixed = TRUE))
  (length(rec$calls()) == 0L)
  (grepl('2', out))
})

assert('a tagged chunk with no registered executor errors informatively', {
  options(compute.executor = NULL)
  e = tryCatch(knit_md(c("```{r}", "#| compute: true", "1 + 1", "```")), error = identity)
  (inherits(e, 'error'))
  (grepl('requires a registered executor', e$message, fixed = TRUE))
})

if (loadable('ggplot2')) assert('automatic fig.alt survives a routed chunk', {
  options(compute.executor = function(fun, args, profile) do.call(fun, args))
  out = knit_md(c(
    "```{r}", "#| compute: true", "#| fig.path: 'kn-test-fig/'", "#| label: altfig",
    "library(ggplot2)",
    "ggplot(mtcars, aes(wt, mpg)) + geom_point() + labs(alt = 'my alt text')",
    "```"
  ))
  options(compute.executor = NULL)
  unlink('kn-test-fig', recursive = TRUE)
  (grepl('alt="my alt text"', out, fixed = TRUE))
})

assert('the worker records plots on a device sized to fig.dim', {
  options(compute.executor = function(fun, args, profile) do.call(fun, args))
  out = knit_md(c(
    "```{r}", "#| compute: true", "#| fig.dim: [4, 3]",
    "cat(par('din'))", "```"
  ))
  options(compute.executor = NULL)
  # the recording device matches the chunk's fig.dim rather than a device default
  (grepl('4 3', out, fixed = TRUE))
})

# end-to-end contract test against a real mirai backend; sync daemons evaluate
# in-process (mirai >= 2.5.1), so no daemon processes, ports or sleeps
if (requireNamespace('mirai', quietly = TRUE)) {

  # a mirai-backed executor. Per the executor contract it raises on error
  # rather than returning error-classed objects
  ex = function(fun, args, profile) {
    res = mirai::mirai(do.call(fun, args), .args = list(fun = fun, args = args), .compute = profile)[]
    if (mirai::is_mirai_error(res)) stop(res)
    res
  }
  mirai::daemons(sync = TRUE)
  options(compute.executor = ex)

  assert('a tagged chunk evaluates via the mirai executor', {
    out = knit_md(c("```{r}", "#| compute: true", "1 + 1", "```"))
    (grepl('2', out))
  })

  assert('figures from a routed chunk are saved by the host plot hook', {
    out = knit_md(c(
      "```{r}", "#| compute: true", "#| fig.path: 'kn-test-fig/'", "#| label: multi",
      "#| fig.dim: [4, 3]",
      "plot(1:10)", "plot(10:1)", "```"
    ))
    files = file.path('kn-test-fig', c('multi-1.png', 'multi-2.png'))
    exists = file.exists(files)
    # the worker records at fig.dim, so the host replays without rescaling
    dims = if (loadable('png')) dim(png::readPNG(files[1L]))[1:2] else c(216L, 288L)
    unlink('kn-test-fig', recursive = TRUE)
    (all(exists))
    (grepl('multi-1.png', out, fixed = TRUE))
    (grepl('multi-2.png', out, fixed = TRUE))
    (dims == c(216L, 288L))
  })

  assert('a routed error: false chunk aborts the render', {
    e = tryCatch(
      knit_md(c("```{r}", "#| compute: true", "#| error: false", "stop('boom')", "```")),
      error = identity
    )
    (inherits(e, 'error'))
    (grepl('boom', e$message, fixed = TRUE))
  })

  options(compute.executor = NULL)
  mirai::daemons(0)

}
