has_rlang = FALSE

.onLoad = function(lib, pkg) {
  register_vignette_engines(pkg)

  default_handlers <<- evaluate::new_output_handler()

  otel_cache_tracer()

  has_rlang <<- requireNamespace("rlang", quietly = TRUE)

  if (has_rlang)
    rlang_entrace_handler <<- list(
      error = function(e) rlang::entrace(e),
      # attach a backtrace to warnings, too: rlang::entrace() turns the warning
      # into an rlang_warning whose conditionMessage() then includes the backtrace.
      # This is opt-in via options(rlang_backtrace_on_warning_report=) (the same
      # option rlang uses at the top level), because computing a backtrace for
      # every warning is wasteful and entrace() rewrites the condition otherwise.
      warning = function(w) {
        opt = getOption('rlang_backtrace_on_warning_report')
        if (!is.null(opt) && !identical(opt, 'none')) rlang::entrace(w)
      }
    )
}
