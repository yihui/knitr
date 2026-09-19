library(testit)

# the command line that an 'exec'-based plot engine would run
eng_plot_cmd = function(engine) {
  options = opts_chunk$merge(list(
    engine = engine, code = '', label = 'test', eval = TRUE, message = TRUE,
    error = TRUE
  ))
  msg = character()
  withCallingHandlers(
    try(eng_plot(options), silent = TRUE),
    message = function(m) {
      msg <<- c(msg, conditionMessage(m)); invokeRestart('muffleMessage')
    }
  )
  gsub('^running: |\n$', '', grep('^running: ', msg, value = TRUE))
}

assert('the ditaa engine passes its default arguments (#2092)', {
  (grepl('^ditaa -s 2 -T -S -E ', eng_plot_cmd('ditaa')))
})

# run PowerShell only if it is both found and actually executable here (avoid
# assuming it works on CRAN/Windows machines: try a trivial command first and
# skip the test unless it produces the expected output)
local({
  if (!nzchar(ps_cmd <- Sys.which('powershell')) && !nzchar(ps_cmd <- Sys.which('pwsh')))
    return()
  probe = tryCatch(
    system2(ps_cmd, c('-Command', 'Write-Output ok'), stdout = TRUE, stderr = TRUE),
    error = function(e) ''
  )
  if (!any(grepl('^ok$', probe))) return()
  assert('the ps engine runs PowerShell code (#1932)', {
    out = knit(text = c(
      sprintf('```{ps, engine.opts=list(command="%s")}', basename(ps_cmd)),
      'Write-Output "hello ps"', '```'
    ), quiet = TRUE)
    (grepl('hello ps', out))
  })
})

# Rcpp can be loaded on a machine without a C++ compiler (it ships as a binary),
# so first probe that a trivial chunk actually compiles; skip only if it doesn't
# (no toolchain). The real assertion then runs unguarded, so a regression in the
# multi-file path surfaces as a failure instead of being masked as a skip
local({
  if (!loadable('Rcpp')) return()
  probe = tryCatch(knit(text = c(
    '```{Rcpp}', '// [[Rcpp::export]]', 'int knitr_probe() { return 1; }', '```'
  ), quiet = TRUE), error = function(e) NULL)
  if (is.null(probe) || !exists('knitr_probe')) return()

  d = tempfile(); dir.create(d); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  writeLines(c('#include <Rcpp.h>', 'int addone(int x);'), h <- file.path(d, 'inc.h'))
  writeLines(c(
    '#include "inc.h"', 'int addone(int x) { return x + 1; }',
    '// [[Rcpp::export]]', 'int add_one(int y) { return addone(y); }'
  ), cpp <- file.path(d, 'src.cpp'))
  # a source file that #includes a sibling header must be compiled from files,
  # not from the concatenated code (the header would be missing at build time)
  assert('an Rcpp chunk compiles a source file with its sibling header (#2367)', {
    knit(text = sprintf('```{Rcpp, file=c("%s", "%s")}\n```', h, cpp), quiet = TRUE)
    (add_one(41L) == 42L)
  })
})

assert('other plot engines do not take the ditaa arguments', {
  (!grepl('-s 2 -T -S -E', eng_plot_cmd('dot')))
  (!grepl('-s 2 -T -S -E', eng_plot_cmd('asy')))
})

# an engine that modifies chunk options (e.g. results = 'asis') should have those
# options visible to the 'chunk' hook as well as the 'output' hook (#2333)
assert('engines can modify chunk options seen by the chunk hook (#2333)', {
  engines = knit_engines$get(); hooks = knit_hooks$get()
  fmt = opts_knit$get('out.format')
  on.exit({
    knit_engines$restore(engines); knit_hooks$restore(hooks)
    opts_knit$set(out.format = fmt); knit_code$restore()
  }, add = TRUE)

  render_markdown()
  knit_engines$set(demo = function(options) {
    options$results = 'asis'
    engine_output(options, options$code, out = 'ASIS')
  })
  seen = new.env()
  knit_hooks$set(chunk = function(x, options) {
    seen$results = options$results
    x
  })

  out = knit(text = c('```{demo}', 'code', '```'), quiet = TRUE)
  # the chunk hook saw the engine-modified option, not the source-declared default
  (seen$results %==% 'asis')
})

# an option explicitly set by opts_hooks is the user's final word and must win
# over an engine's later internal change to the same option, as seen by the
# 'chunk' hook (#2488); this is what Quarto relies on to wrap annotated output
assert('opts_hooks wins over engine-modified options at the chunk hook (#2488)', {
  engines = knit_engines$get(); hooks = knit_hooks$get(); ohooks = opts_hooks$get()
  fmt = opts_knit$get('out.format')
  on.exit({
    knit_engines$restore(engines); knit_hooks$restore(hooks)
    opts_hooks$restore(ohooks)
    opts_knit$set(out.format = fmt); knit_code$restore()
  }, add = TRUE)

  render_markdown()
  # the engine forces results = 'asis' internally (like css/js/sass/scss)
  knit_engines$set(demo = function(options) {
    options$results = 'asis'
    engine_output(options, options$code, out = 'ASIS')
  })
  # but opts_hooks set it to 'hold' first
  opts_hooks$set(results = function(options) {
    options$results = 'hold'
    options
  })
  seen = new.env()
  knit_hooks$set(chunk = function(x, options) {
    seen$results = options$results
    x
  })

  out = knit(text = c('```{demo}', 'code', '```'), quiet = TRUE)
  # opts_hooks value wins at the chunk hook, not the engine's 'asis'
  (seen$results %==% 'hold')
})
