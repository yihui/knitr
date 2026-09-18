library(testit)

res = mapply(
  auto_out_name,
  c('abc.Rnw', 'abc.rnw', 'abc.rtex', 'abc.Rmd', 'abc.rhtm', 'abc.Rhtml', 'foo.abc.rhtml'),
  USE.NAMES = FALSE
)
assert('auto_out_name() converts .Rfoo to .foo', {
  (res %==% c('abc.tex', 'abc.tex', 'abc.tex', 'abc.md', 'abc.htm', 'abc.html', 'foo.abc.html'))
})

res = mapply(
  auto_out_name,
  c('abc.tex', '_knit_abc.tex', '_knit_abc.md', 'foo_knit_.html'),
  USE.NAMES = FALSE
)
assert('auto_out_name() converts .tex/.unknown to .txt, and removes _knit_', {
  (res %==% c('abc.txt', 'abc.tex', 'abc.md', 'foo.html'))
  (auto_out_name('foo.bar') %==% 'foo.txt')
})

assert('chunks with include=FALSE should stop on error', {
  (suppressMessages(
    has_error(knit(text = c('<<include=F>>=', '1+"a"', '@'), quiet = TRUE))
  ))
})

assert('tidy=FALSE + eval=numeric should work', {
  ('[1] 1\n[1] 2\n' %==% knit(
    text = c('<<tidy=FALSE, eval=1:2, echo=FALSE, results="asis">>=', '1', '1+', '1', '1', '@'),
    quiet = TRUE
  ))
})

assert('opts_template options are used', {
  ("\n[1] 2\n" %==% knit(
    text = c(
      "<<include=FALSE>>=","opts_template$set(quiet = list(echo=FALSE))", "@",
      "<<results='asis', opts.label='quiet'>>=", "1+1", "@"
    ),
    quiet = TRUE
  ))
})

assert('local chunk options override opts_template', {
  ("\n\\begin{kframe}\n\\begin{alltt}\n\\hlnum{1}\\hlopt{+}\\hlnum{1}\n\\end{alltt}\n\\end{kframe}[1] 2\n" %==% knit(
    text = c(
      "<<include=FALSE>>=","opts_template$set(quiet = list(echo=FALSE))", "@",
      "<<results='asis', opts.label='quiet', echo=TRUE, tidy=FALSE>>=", "1+1", "@"
    ),
    quiet = TRUE
  ))
})

# a shortcut
k = function(text) {
  on.exit(opts_chunk$restore())
  knit(text = c('```{r}', text, '```'), quiet = TRUE, envir = parent.frame())
}
k(c(
  'knit_child(text = "```{r}\nopts_chunk$set(echo=FALSE)\n```", options=list())',
  'x1 = opts_chunk$get("echo")'
))
k(c(
  'knit_child(text = "```{r}\nopts_chunk$set(dpi=200)\n```", options=list(dpi=100))',
  'x2 = opts_chunk$get("dpi")'
))

assert('using knit_child() does not reset global chunk options set in child documents', {
  (x1 %==% FALSE)
  (x2 %==% 200)
})

txt = '%\\documentclass{article}
\\documentclass{article}
\\begin{document}
\\Sexpr{pi}
\\end{document}'
res = strsplit(knit(text = txt, quiet = TRUE), '\n')[[1]]

assert('insert_header_latex() finds the correct \\documentclass{}', {
  (res[1] %==% '%\\documentclass{article}')
})

txt = '\\documentclass{article}
\\begin{document}
\\begin{verbatim}
\\documentclass{article}
\\end{verbatim}
\\Sexpr{pi}
\\end{document}'
res = strsplit(knit(text = txt, quiet = TRUE), '\n')[[1]]

assert('insert_header_latex() finds the correct \\documentclass{}', {
  (res[length(res) - 3] %==% '\\documentclass{article}')
})

assert('knit_meta_add() adds meta objects with the correct number of labels', {
  knit_meta(clean = TRUE)
  knit_meta_add(list(1, 2))
  m = attr(knit_meta_add(list(3, 4), label = c('a', 'b')), 'knit_meta_id')
  knit_meta(clean = TRUE)
  (m %==% c('', '', 'a', 'b'))
})

local({
  out = NULL
  hnd = function(cnd) out <<- cnd

  on.exit(file.remove('knit-handlers.md'))
  knit('knit-handlers.Rmd', quiet = TRUE)

  assert('knit() passes calling handlers to evaluate()', {
    (inherits(out, 'error'))
  })
})

assert('knit_exit() does not leave knitr in a non-functioning state (#2283)', {
  res1 = knit(text = c('```{r}', 'knitr::knit_exit()', '```'), quiet = TRUE)
  res2 = knit(text = c('```{r}', 'cat("hello")', '```'), quiet = TRUE)
  (grepl('hello', res2))
})

assert('merge_fig_alt() lets the user value win over the collected default (#2001)', {
  # no collected alt text: user value is returned unchanged
  (merge_fig_alt('u', character(0)) %==% 'u')
  (merge_fig_alt(NULL, NA_character_) %==% NULL)
  # no user value: the collected default is used
  (merge_fig_alt(NULL, c('a', 'b')) %==% c('a', 'b'))
  # element-wise: NA/'' in the user value falls back to the collected default
  (merge_fig_alt(c(NA, 'x'), c('a', 'b')) %==% c('a', 'x'))
  (merge_fig_alt(c('', 'x'), c('a', 'b')) %==% c('a', 'x'))
  # a scalar user value is recycled (as fig.alt already recycles)
  (merge_fig_alt('x', c('a', 'b')) %==% c('x', 'x'))
})

assert('record_fig_alt() collects alt text only for recognized plot objects (#2001)', {
  on.exit(reset_fig_alt(), add = TRUE)
  reset_fig_alt()
  # ordinary values are not recognized as plots and do not occupy a slot
  record_fig_alt(1:10)
  record_fig_alt('a')
  (length(.knitEnv$fig.alt) == 0)
})

if (requireNamespace('ggplot2', quietly = TRUE) && packageVersion('ggplot2') >= '3.4.0') {
  library(ggplot2)
  gg = function(alt) {
    p = ggplot(data.frame(x = 1, y = 1), aes(x, y)) + geom_point()
    if (!missing(alt)) p = p + labs(alt = alt)
    p
  }

  assert('record_fig_alt() reads ggplot alt text, with NA for plots lacking it (#2001)', {
    on.exit(reset_fig_alt(), add = TRUE)
    reset_fig_alt()
    record_fig_alt(gg('first'))
    record_fig_alt(gg())          # no alt -> NA placeholder
    record_fig_alt(gg('third'))
    (.knitEnv$fig.alt %==% c('first', NA, 'third'))
  })

  assert('ggplot2::labs(alt=) provides the default fig.alt for figures (#2001)', {
    # alt text from labs(alt=) is used when fig.alt is not set
    res = knit(text = c(
      '```{r}', 'library(ggplot2)',
      'ggplot(mtcars, aes(wt, mpg)) + geom_point() + labs(alt = "from labs")',
      '```'
    ), quiet = TRUE)
    (grepl('alt="from labs"', res, fixed = TRUE))

    # an explicit fig.alt still wins over labs(alt=)
    res = knit(text = c(
      '```{r fig.alt="explicit", echo=FALSE}', 'library(ggplot2)',
      'ggplot(mtcars, aes(wt, mpg)) + geom_point() + labs(alt = "from labs")',
      '```'
    ), quiet = TRUE)
    (grepl('alt="explicit"', res, fixed = TRUE))
    (!grepl('from labs', res, fixed = TRUE))  # not the labs value (echo off)
  })
}
