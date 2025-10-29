library(testit)

if (requireNamespace("otelsdk", quietly = TRUE)) {

  record = otelsdk::with_otel_record({
    knitr:::otel_refresh_tracer("knitr")
    knit(
      text = c('<<tidy=FALSE, eval=1:2, echo=FALSE, results="asis">>=', '1', '1+', '1', '1', '@'),
      quiet = TRUE
    )
  })

  traces = record$traces

  assert('otel tracing works', {
    (length(traces) %==% 3L)
    (startsWith(traces[[1L]]$name, 'knitr processing'))
    (traces[[1L]]$attributes$knitr.format %==% 'latex')
    (traces[[1L]]$attributes$knitr.input %==% '')
    (traces[[1L]]$attributes$knitr.output %==% '')
    (traces[[2L]]$name %==% 'knit unnamed-chunk-1')
    (traces[[2L]]$attributes$knitr.device %==% 'pdf')
    (traces[[2L]]$attributes$knitr.echo %==% FALSE)
    (traces[[2L]]$attributes$knitr.engine %==% 'R')
    (traces[[2L]]$attributes$knitr.eval %==% c(1, 2))
    (startsWith(traces[[3L]]$name, 'knitr output'))
    (traces[[3L]]$attributes$knitr.format %==% 'latex')
    (traces[[3L]]$attributes$knitr.input %==% '')
    (traces[[3L]]$attributes$knitr.output %==% '')
  })

}
