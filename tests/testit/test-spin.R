library(testit)

spin_text = function(..., format = "Rmd") {
  x = spin(text = c(...), knit = FALSE, format = format)
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
