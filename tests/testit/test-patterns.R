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
