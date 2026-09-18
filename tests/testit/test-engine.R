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

# the css/js/sass/scss engines set results = 'asis' internally only to write raw
# HTML verbatim; this must not leak to the 'chunk' hook and override a user's
# opts_hooks setting (#2488)
assert('css/js engines do not force results = asis onto the chunk hook (#2488)', {
  engines = knit_engines$get(); hooks = knit_hooks$get(); ohooks = opts_hooks$get()
  fmt = opts_knit$get('out.format')
  on.exit({
    knit_engines$restore(engines); knit_hooks$restore(hooks)
    opts_hooks$restore(ohooks)
    opts_knit$set(out.format = fmt); knit_code$restore()
  }, add = TRUE)

  render_markdown()
  opts_hooks$set(results = function(options) {
    options$results = 'hold'
    options
  })
  seen = new.env()
  knit_hooks$set(chunk = function(x, options) {
    seen$results = options$results
    x
  })

  out = knit(text = c('```{css}', 'body { color: red; }', '```'), quiet = TRUE)
  # the chunk hook sees the opts_hooks value, not the engine's internal 'asis'
  (seen$results %==% 'hold')
  # the raw CSS is still written verbatim (not commented out)
  (grepl('<style type="text/css">', out, fixed = TRUE))
})
