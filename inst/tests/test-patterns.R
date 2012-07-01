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
