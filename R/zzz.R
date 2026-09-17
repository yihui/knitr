has_rlang = FALSE

.onLoad = function(lib, pkg) {
  register_vignette_engines(pkg)

  default_handlers <<- evaluate::new_output_handler()

  otel_cache_tracer()

  has_rlang <<- requireNamespace("rlang", quietly = TRUE)

  if (has_rlang)
    rlang_entrace_handler <<- list(error = function(e) rlang::entrace(e))

  # pick up alt text set via ggplot2::labs(alt=) as the default fig.alt (#2001);
  # get_alt_text() was added in ggplot2 3.4.0
  if (getRversion() >= '3.6.0') {
    registerS3method('fig_alt', 'ggplot', function(x, ...) {
      if (is.function(f <- tryCatch(
        getExportedValue('ggplot2', 'get_alt_text'), error = function(e) NULL
      ))) f(x)
    }, envir = asNamespace('knitr'))
  }
}
