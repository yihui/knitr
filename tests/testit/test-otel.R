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

  assert('otel tracing works on text input', {
    (length(traces) %==% 3L)
    (startsWith(traces[[1L]]$name, 'knitr processing'))
    (traces[[1L]]$attributes$knitr.format %==% 'latex')
    (traces[[1L]]$attributes$knitr.input %==% '')
    (traces[[1L]]$attributes$knitr.output %==% '')
    (traces[[2L]]$name %==% 'knit unnamed-chunk-1')
    (traces[[2L]]$attributes$knitr.chunk.device %==% 'pdf')
    (traces[[2L]]$attributes$knitr.chunk.echo %==% FALSE)
    (traces[[2L]]$attributes$knitr.chunk.engine %==% 'R')
    (traces[[2L]]$attributes$knitr.chunk.eval %==% c(1, 2))
    (startsWith(traces[[3L]]$name, 'knitr output'))
    (traces[[3L]]$attributes$knitr.format %==% 'latex')
    (traces[[3L]]$attributes$knitr.input %==% '')
    (traces[[3L]]$attributes$knitr.output %==% '')
  })

  record = otelsdk::with_otel_record({
    knitr:::otel_refresh_tracer("knitr")

    local({
      env = new.env()
      env$y = 1:3
      z = 5
      on.exit(file.remove('knit-envir.md'))
      knit('knit-envir.Rmd', envir = env, quiet = TRUE)
    })
  })

  traces = record$traces

  assert('otel tracing works when knitting files', {
    (length(traces) %==% 3L)
    (traces[[1L]]$name %==% 'knitr processing knit-envir.Rmd')
    (traces[[1L]]$attributes$knitr.format %==% 'markdown')
    (traces[[1L]]$attributes$knitr.input %==% 'knit-envir.Rmd')
    (traces[[1L]]$attributes$knitr.output %==% 'knit-envir.md')
    (traces[[2L]]$name %==% 'knit test')
    (traces[[2L]]$attributes$knitr.chunk.device %==% 'png')
    (traces[[2L]]$attributes$knitr.chunk.echo %==% TRUE)
    (traces[[2L]]$attributes$knitr.chunk.engine %==% 'R')
    (traces[[2L]]$attributes$knitr.chunk.eval %==% TRUE)
    (traces[[3L]]$name %==% 'knitr output knit-envir.md')
    (traces[[3L]]$attributes$knitr.format %==% 'markdown')
    (traces[[3L]]$attributes$knitr.input %==% 'knit-envir.Rmd')
    (traces[[3L]]$attributes$knitr.output %==% 'knit-envir.md')
  })

}
