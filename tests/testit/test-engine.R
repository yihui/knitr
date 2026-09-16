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
