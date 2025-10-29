otel_tracer_name = "org.yihui.knitr"
otel_tracer = NULL
otel_is_tracing = FALSE

otel_local_active_span = function(options, scope = parent.frame()) {
  otel_is_tracing || return()
  otel::start_local_active_span(
    name = sprintf("knit %s", options$label),
    attributes = otel::as_attributes(
      list(
        kintr.device = options$dev,
        knitr.echo = options$echo,
        knitr.engine = options$engine,
        knitr.eval = options$eval
      )
    ),
    tracer = otel_tracer,
    activation_scope = scope
  )
}

otel_cache_tracer = function() {
  requireNamespace("otel", quietly = TRUE) || return()
  otel_tracer <<- otel::get_tracer(otel_tracer_name)
  otel_is_tracing <<- tracer_enabled(otel_tracer)
}

tracer_enabled = function(tracer) {
  .subset2(tracer, "is_enabled")()
}
