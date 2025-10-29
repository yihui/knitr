otel_tracer_name = "org.yihui.knitr"
otel_tracer = NULL
otel_is_tracing = FALSE

otel_knitr_span = function(input, output, format, starting = TRUE) {
  otel_is_tracing || return()
  if (is.null(input)) input = ""
  if (is.null(output)) output = ""
  otel::start_local_active_span(
    name = if (starting) {
      sprintf("knitr processing %s", input)
    } else {
      sprintf("knitr output %s", output)
    },
    attributes = otel::as_attributes(
      list(
        knitr.format = format,
        knitr.input = input,
        knitr.output = output
      )
    ),
    tracer = otel_tracer
  )
}

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
