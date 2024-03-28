library(testit)

spin_w_tempfile = function(..., format = "Rmd") {
  tmp = tempfile(fileext = ".R")
  writeLines(c(...), tmp)
  spinned = spin(tmp, knit = FALSE, format = format)
  result = readLines(spinned)
  file.remove(c(tmp, spinned))
  result
}

assert("spin() detects lines for documentation", {
  (spin_w_tempfile("#' test", "1 * 1", "#' test") %==%
     c("test", "", "```{r}", "1 * 1", "```", "", "test"))
  # a multiline string literal contains the pattern of doc or inline
  (spin_w_tempfile("code <- \"", "#' test\"") %==%
    c("", "```{r}", "code <- \"", "#' test\"", "```", ""))
  (spin_w_tempfile("code <- \"", "{{ 1 + 1 }}", "\"") %==%
    c("", "```{r}", "code <- \"", "{{ 1 + 1 }}", "\"", "```", ""))
  # a multiline symbol contains the pattern of doc or inline
  (spin_w_tempfile("`", "#' test", "`") %==%
    c("", "```{r}", "`", "#' test", "`", "```", ""))
  (spin_w_tempfile("`", "{{ 1 + 1 }}", "`") %==%
    c("", "```{r}", "`", "{{ 1 + 1 }}", "`", "```", ""))
})

assert("spin() uses proper number of backticks", {
  (spin_w_tempfile("{{ '`' }}") %==% c("``r  '`'  ``"))
  (spin_w_tempfile("{{`x`}}") %==% c("``r `x` ``"))
  (spin_w_tempfile("x <- '", "```", "'") %==%
    c("", "````{r}", "x <- '", "```", "'", "````", ""))
})

assert("spin() works properly with quarto `#|`", {
  (
    spin_w_tempfile("", "#| echo: false", "#| message: false", "#| include: false", "1+1", "#| eval: false", "2 + 2", "", "#' Text", format = "qmd") %==% 
    c('', '```{r}', '#| echo: false', '#| message: false', '#| include: false', '1+1', '```', '```{r}', '#| eval: false', '2 + 2', '```', '', 'Text')
  ) 

  # https://github.com/yihui/knitr/issues/2314
  (
    spin_w_tempfile('#| echo: false', '1+1', '#| label: test', '1+1', format = "qmd") %==% 
    c('', '```{r}', '#| echo: false', '1+1', '```', '```{r}', '#| label: test', '1+1', '```', '')
  ) 
  
  # Has a `# %%` already
  (
    spin_w_tempfile('# %%', '#| echo: false', '1+1', '#| label: test', '1+1', format = "qmd") %==% 
    c('', '```{r}', '#| echo: false', '1+1', '```', '```{r}', '#| label: test', '1+1', '```', '')
  ) 
})
