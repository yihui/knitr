library(testit)

spin_text = function(..., format = "Rmd", engine = NULL) {
  x = spin(text = c(...), knit = FALSE, format = format, engine = engine)
  xfun::split_lines(x)
}

assert("spin() detects lines for documentation", {
  (spin_text("#' test", "1 * 1", "#' test") %==%
     c("test", "", "```{r}", "1 * 1", "```", "", "test"))
  # a multiline string literal contains the pattern of doc or inline
  (spin_text("code <- \"", "#' test\"") %==%
    c("", "```{r}", "code <- \"", "#' test\"", "```", ""))
  (spin_text("code <- \"", "{{ 1 + 1 }}", "\"") %==%
    c("", "```{r}", "code <- \"", "{{ 1 + 1 }}", "\"", "```", ""))
  # a multiline symbol contains the pattern of doc or inline
  (spin_text("`", "#' test", "`") %==%
    c("", "```{r}", "`", "#' test", "`", "```", ""))
  (spin_text("`", "{{ 1 + 1 }}", "`") %==%
    c("", "```{r}", "`", "{{ 1 + 1 }}", "`", "```", ""))
})

assert("spin() uses proper number of backticks", {
  (spin_text("{{ '`' }}") %==% c("``r  '`'  ``"))
  (spin_text("{{`x`}}") %==% c("``r `x` ``"))
  (spin_text("x <- '", "```", "'") %==%
    c("", "````{r}", "x <- '", "```", "'", "````", ""))
})

assert("spin() inserts a space before the chunk label from `----` tokens (#2014)", {
  # purl(documentation = 2) uses `## ----label----`, which leaves no space
  # before the label; spin() must not produce ```{rlabel} (an invalid header
  # that would be parsed as an engine name on the next round-trip)
  (spin_text("## ----chunk----", "1 + 2") %==%
     c("", "```{r chunk}", "1 + 2", "```", ""))
  (spin_text("## @knitr lbl", "1 + 2") %==%
     c("", "```{r lbl}", "1 + 2", "```", ""))
  # no label: no trailing space after r
  (spin_text("## ----", "1 + 2") %==%
     c("", "```{r}", "1 + 2", "```", ""))
})

assert("spin() removes correctly paired comment delimiters", {
  # lines between `# /*` and `# */` are dropped; the rest is kept
  (spin_text("#' A", "# /*", "#' hidden", "# */", "#' B") %==% c("A", "B"))
})

assert("spin() errors on mis-ordered or unmatched comment delimiters", {
  # end delimiter before start delimiter (equal counts): used to silently drop
  # the lines in between; now an error is signaled
  (has_error(spin_text("#' A", "# */", "#' keep", "# /*", "#' B")))
  # more starts than ends
  (has_error(spin_text("#' a", "# /*", "#' b", "# /*", "#' c", "# */", "#' d")))
  # a lone end delimiter
  (has_error(spin_text("#' a", "# */", "#' b")))
})

assert("spin() generates code chunks with pipe comments `#|`", {
  (
    spin_text("", "#| echo: false", "#| message: false", "#| include: false", "1+1", "#| eval: false", "2 + 2", "", "#' Text") %==%
    c('', '```{r}', '#| echo: false', '#| message: false', '#| include: false', '1+1', '```', '```{r}', '#| eval: false', '2 + 2', '```', '', 'Text')
  )

  # https://github.com/yihui/knitr/issues/2314
  (
    spin_text('#| echo: false', '1+1', '#| label: test', '1+1') %==%
    c('', '```{r}', '#| echo: false', '1+1', '```', '```{r}', '#| label: test', '1+1', '```', '')
  )

  # Has a `# %%` already
  (
    spin_text('# %%', '#| echo: false', '1+1', '#| label: test', '1+1') %==%
    c('', '```{r}', '#| echo: false', '1+1', '```', '```{r}', '#| label: test', '1+1', '```', '')
  )
})

assert("spin() can set a default chunk engine for non-R scripts", {
  (spin_text("#' Doc", "print(1)", engine = "python") %==%
     c('Doc', '', '```{python}', 'print(1)', '```', ''))
  # existing chunk options are kept
  (spin_text("#+ label=foo, echo=FALSE", "print(1)", engine = "python") %==%
     c('', '```{python label=foo, echo=FALSE}', 'print(1)', '```', ''))
  # a chunk's own engine option overrides the fence engine at knit time
  (spin_text('#+ engine="R"', "y <- 2", engine = "python") %==%
     c('', '```{python engine="R"}', 'y <- 2', '```', ''))
  (spin_text("#+ foo", "x <- 1") %==% c('', '```{r foo}', 'x <- 1', '```', ''))
})

assert("spin() guesses the chunk engine from the file extension", {
  f = tempfile(fileext = '.py')
  xfun::write_utf8(c("#' Doc", "print(1)"), f)
  out = spin(f, knit = FALSE, format = "Rmd")
  (xfun::read_utf8(out) %==% c('Doc', '', '```{python}', 'print(1)', '```', ''))
  file.remove(f, out)
})

assert("spin(roxygen = TRUE) keeps roxygen blocks as code but plain #' as prose (#2317)", {
  rox = c("#' @param x a number", "#' @export", "f <- function(x) x")
  # by default a roxygen block is mangled into prose (@tags lose their meaning)
  (spin_text(rox) %==%
     c('@param x a number', '@export', '', '```{r}', 'f <- function(x) x', '```', ''))
  # with roxygen = TRUE, a #' block containing an @tag is kept verbatim as code
  (xfun::split_lines(spin(text = rox, knit = FALSE, roxygen = TRUE)) %==%
     c('', '```{r}', rox, '```', ''))
  # a #' block without any @tag is still treated as documentation
  (xfun::split_lines(spin(text = c("#' just prose", "1 + 1"), knit = FALSE, roxygen = TRUE)) %==%
     c('just prose', '', '```{r}', '1 + 1', '```', ''))
})
