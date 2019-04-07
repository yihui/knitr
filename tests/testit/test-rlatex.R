library(testit)

assert(
  'detect_pattern() automatically detects syntax patterns',
  identical(detect_pattern('<<>>='), 'rnw'),
  identical(detect_pattern('<<foo, bar=TRUE>>='), 'rnw'),
##  identical(detect_pattern('\begin{Rcode}'), 'rlatex'),
##  identical(detect_pattern('\begin{Rcode}[foo, bar=TRUE]'), 'rlatex'),
##  identical(detect_pattern('asdf', 'rlatex'), 'rlatex'),
  detect_pattern('foo') %==% NULL
)


ce = all_patterns$rlatex$chunk.end
assert(
  'patterns for rlatex',
  identical(grep(ce, '  \\end{Rcode}'), 1L), # spaces before @
  identical(grep(ce, '\\end{Rcode}   '), 1L), # spaces after @
  identical(grep(ce, '\\end{Rcode} %asdf'), 1L), # comments after %
  identical(grep(ce, '\\end{Rcode} asdf'), integer()), # only spaces/comments allowed
  identical(grep(ce, ' \\end{Rcode} a% sdf'), integer())
)
