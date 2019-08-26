library(testit)

assert('detect_pattern() automatically detects syntax patterns', {
  (detect_pattern('<<>>=') %==% 'rnw')
  (detect_pattern('<<foo, bar=TRUE>>=') %==% 'rnw')
  (detect_pattern('% begin.rcode') %==% 'tex')
  (detect_pattern('<!--begin.rcode') %==% 'html')
  (detect_pattern('``` {r}') %==% 'md')
  (detect_pattern('asdf', 'rnw') %==% 'rnw')
  (detect_pattern('foo') %==% NULL)
})

assert('group_pattern() checks if a pattern contains a group', {
  (group_pattern('(.*)') %==% TRUE)
  (group_pattern('()') %==% FALSE)
  (group_pattern('abc') %==% FALSE)
  (group_pattern(NULL) %==% FALSE)
})

ce_rnw = all_patterns$rnw$chunk.end
assert('patterns for Rnw', {
  (grep(ce_rnw, '  @') %==% 1L) # spaces before @
  (grep(ce_rnw, '@  ') %==% 1L) # spaces after @
  (grep(ce_rnw, '@ %asdf') %==% 1L) # comments after %
  (grep(ce_rnw, '@ asdf') %==% integer()) # only spaces/comments allowed
  (grep(ce_rnw, ' @ a% sdf') %==% integer())
})

cb_md = all_patterns$md$chunk.begin
assert('patterns for md', {
  # TRUE for chunk options
  (grepl(cb_md, '```{r}') %==% TRUE)
  (grepl(cb_md, '```{r label}') %==% TRUE)
  (grepl(cb_md, '```{r, eval=FALSE}') %==% TRUE)
  (grepl(cb_md, '```{awk}') %==% TRUE)
  # FALSE for Pandoc's fenced code attributes
  (grepl(cb_md, '```{.class}') %==% FALSE)
  (grepl(cb_md, '```{#id}') %==% FALSE)
  (grepl(cb_md, '```{style="color: red"}') %==% FALSE)
  # FALSE for Pandoc's raw attribute
  (grepl(cb_md, '```{=latex}') %==% FALSE)
})
