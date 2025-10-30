otel_tracer_name = 'org.yihui.knitr'
otel_tracer = NULL
otel_is_tracing = FALSE

# generic otel helpers:

# - without specifying `scope`, the span ends when this function returns;
#   to make this a local span (last as long as the function it is called from),
#   specify `scope = environment()`
# - arguments remain unevaluated on early return
otel_active_span = function(
  name,
  label,
  attributes = list(),
  scope = environment()
) {
  otel_is_tracing || return()
  otel::start_local_active_span(
    name = sprintf("%s %s", name, label),
    attributes = otel::as_attributes(attributes),
    tracer = otel_tracer,
    activation_scope = scope
  )
}

otel_cache_tracer = function() {
  requireNamespace('otel', quietly = TRUE) || return()
  otel_tracer <<- otel::get_tracer(otel_tracer_name)
  otel_is_tracing <<- tracer_enabled(otel_tracer)
}

tracer_enabled = function(tracer) {
  .subset2(tracer, 'is_enabled')()
}

otel_refresh_tracer <- function(pkgname) {
  requireNamespace('otel', quietly = TRUE) || return()
  ns <- getNamespace(pkgname)
  do.call(unlockBinding, list('otel_is_tracing', ns)) # do.call for R CMD Check
  do.call(unlockBinding, list('otel_tracer', ns))
  otel_tracer <- otel::get_tracer()
  ns[['otel_is_tracing']] <- tracer_enabled(otel_tracer)
  ns[['otel_tracer']] <- otel_tracer
  lockBinding('otel_is_tracing', ns)
  lockBinding('otel_tracer', ns)
}

# knitr-specific helpers:

make_chunk_attributes <- function(options) {
  list(
    knitr.chunk.device = options$dev,
    knitr.chunk.echo = options$echo,
    knitr.chunk.engine = options$engine,
    knitr.chunk.eval = options$eval,
    knitr.chunk.label = options$label
  )
}

make_knitr_attributes <- function() {
  list(
    knitr.format = out_format(),
    knitr.input = get_knitr_concord('infile'),
    knitr.output = get_knitr_concord('outfile')
  )
}

# safe version that always returns a string
get_knitr_concord <- function(name) {
  item = knit_concord$get(name)
  is.null(item) && return('')
  item
}
