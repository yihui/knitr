has_rlang = FALSE

.onLoad = function(lib, pkg) {
  register_vignette_engines(pkg)

  # opt-in internal debug logging: the `!DEBUG` strings scattered in the source
  # are turned into log messages by debugme, but only when the user has set the
  # DEBUGME environment variable (e.g. DEBUGME=knitr) and has debugme installed
  if (Sys.getenv('DEBUGME') != '' && requireNamespace('debugme', quietly = TRUE))
    debugme::debugme()

  default_handlers <<- evaluate::new_output_handler()

  otel_cache_tracer()

  has_rlang <<- requireNamespace("rlang", quietly = TRUE)

  if (has_rlang)
    rlang_entrace_handler <<- list(error = function(e) rlang::entrace(e))
}
