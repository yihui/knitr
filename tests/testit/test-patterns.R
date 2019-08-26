library(testit)

assert(
  'detect_pattern() automatically detects syntax patterns',
  identical(detect_pattern('<<>>='), 'rnw'),
  identical(detect_pattern('<<foo, bar=TRUE>>='), 'rnw'),
  identical(detect_pattern('% begin.rcode'), 'tex'),
  identical(detect_pattern('<!--begin.rcode'), 'html'),
  identical(detect_pattern('``` {r}'), 'md'),
  identical(detect_pattern('asdf', 'rnw'), 'rnw'),
  detect_pattern('foo') %==% NULL
)

assert(
  'group_pattern() checks if a pattern contains a group',
  group_pattern('(.*)'), !group_pattern('()'), !group_pattern('abc'), !group_pattern(NULL)
)

ce = all_patterns$rnw$chunk.end
assert(
  'patterns for Rnw',
  identical(grep(ce, '  @'), 1L), # spaces before @
  identical(grep(ce, '@  '), 1L), # spaces after @
  identical(grep(ce, '@ %asdf'), 1L), # comments after %
  identical(grep(ce, '@ asdf'), integer()), # only spaces/comments allowed
  identical(grep(ce, ' @ a% sdf'), integer())
)

cb_md = all_patterns$md$chunk.begin
assert(
  'patterns for md',
  # Are they chunk options?
  isTRUE(grepl(cb_md, '```{r}')),
  isTRUE(grepl(cb_md, '```{r label}')),
  isTRUE(grepl(cb_md, '```{r, eval=FALSE}')),
  isTRUE(grepl(cb_md, '```{awk}')),
  # Are they Pandoc's fenced code attributes?
  !isTRUE(grepl(cb_md, '```{.class}')),
  !isTRUE(grepl(cb_md, '```{#id}')),
  !isTRUE(grepl(cb_md, '```{style="color: red"}')),
  # Is it Pandoc's raw attribute?
  !isTRUE(grepl(cb_md, '```{=latex}'))
)
