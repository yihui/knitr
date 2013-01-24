context('Parser')

test_that('parsing options', {
  ## new syntax
  expect_identical(parse_params('a-s-d,b=TRUE,c=def'), alist(label='a-s-d',b=TRUE,c=def))
  expect_error(parse_params('a,b'))
  expect_error(parse_params('a,b,c=qwer'))
  expect_identical(parse_params('a,opt=c(1,3,5)'),alist(label='a',opt=c(1,3,5)))
  expect_identical(parse_params('label="xx",opt=zz'),alist(label='xx',opt=zz))
  expect_identical(parse_params('label=foo'),alist(label='foo'))
  expect_identical(parse_params('a,b=2,c="qwer",asdf="efg"'),
                   alist(label='a', b=2, c='qwer',asdf='efg'))
  expect_identical(parse_params('2a'), alist(label='2a'))
  expect_identical(parse_params('abc-function,fig.path="foo/bar-"'),
                   alist(label='abc-function', fig.path="foo/bar-"))
})


test_that('parsing inline texts', {
  res = parse_inline(c('aaa \\Sexpr{x}', 'bbb \\Sexpr{NA} and \\Sexpr{1+2}',
                       'another expression \\Sexpr{rnorm(10)}'), all_patterns$rnw)
  expect_identical(res$code, c('x', 'NA', '1+2', 'rnorm(10)'))
  expect_identical(nchar(res$input), 81L)
  # empty inline code is not recognized
  expect_identical(parse_inline('\\Sexpr{}', all_patterns$rnw)$code,
                   character(0))
  # can use > in HTML inline code
  expect_identical(parse_inline('<!--rinline "<a>" -->', all_patterns$html)$code,
                   ' "<a>" ')
})
