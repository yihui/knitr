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
