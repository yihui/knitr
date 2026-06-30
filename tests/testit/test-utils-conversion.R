library(testit)

if (requireNamespace('markdown', quietly = TRUE)) assert(
  'knit2html() embeds local figure resources when output is written elsewhere', {
    td = tempfile('knit2html-')
    dir.create(td)
    old = setwd(td)
    on.exit(setwd(old), add = TRUE)

    writeLines(c(
      '---',
      'title: "test"',
      'output: html_document',
      '---',
      '',
      '```{r Graph2, fig.width=7.7, fig.height=6.8}',
      'plot(1, 1)',
      '```'
    ), 'test.Rmd')

    out_dir = tempfile('knit2html-out-')
    dir.create(out_dir)
    warnings = character()
    out = withCallingHandlers(
      knit2html(
        'test.Rmd',
        output = file.path(out_dir, 'OUT.html'),
        quiet = TRUE,
        force_v1 = TRUE
      ),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart('muffleWarning')
      }
    )

    html = read_utf8(out)
    (!any(grepl('not found \\(hence cannot be embedded\\)', warnings)))
    (any(grepl('data:image/png;base64,', html, fixed = TRUE)))
    (!any(grepl('figure/Graph2-1.png', html, fixed = TRUE)))
    (!file.exists(file.path(out_dir, 'figure', 'Graph2-1.png')))
  }
)
