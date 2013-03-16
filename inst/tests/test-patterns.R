context('Patterns')

test_that('auto detect patterns', {
  expect_identical(detect_pattern('<<>>='), 'rnw')
  expect_identical(detect_pattern('<<foo, bar=TRUE>>='), 'rnw')
  expect_identical(detect_pattern('% begin.rcode'), 'tex')
  expect_identical(detect_pattern('<!--begin.rcode'), 'html')
  expect_identical(detect_pattern('``` {r}'), 'md')
  expect_identical(detect_pattern('asdf', 'rnw'), 'rnw')
})

test_that('does a pattern contain a group?', {
  expect_true(group_pattern('(.*)'))
  expect_false(group_pattern('()'))
  expect_false(group_pattern('abc'))
  expect_false(group_pattern(NULL))
})

test_that('patterns for Rnw', {
  ce = all_patterns$rnw$chunk.end
  expect_identical(grep(ce, '  @'), 1L) # spaces before @
  expect_identical(grep(ce, '@  '), 1L) # spaces after @
  expect_identical(grep(ce, '@ %asdf'), 1L) # comments after %
  expect_identical(grep(ce, '@ asdf'), integer()) # only spaces/comments allowed
  expect_identical(grep(ce, ' @ a% sdf'), integer())
})
