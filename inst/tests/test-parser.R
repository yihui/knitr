context('Parser')

test_that('parsing options', {
  opts_knit$set(sweave.penalty = 0)
  ## new syntax
  expect_identical(parse_params('a-s-d,b=TRUE,c=def'), alist(label='a-s-d',b=TRUE,c=def))
  expect_error(parse_params('a,b,c=qwer'))
  expect_identical(parse_params('a,opt=c(1,3,5)'),alist(label='a',opt=c(1,3,5)))
  expect_identical(parse_params('label="xx",opt=zz'),alist(label='xx',opt=zz))
  expect_identical(parse_params('label=foo'),alist(label='foo'))
  expect_identical(parse_params('a,b=2,c="qwer",asdf="efg"'),
                   alist(label='a', b=2, c='qwer',asdf='efg'))
  ## back-compatibility with Sweave
  expect_identical(parse_params('abc,fig.path=foo/bar-'),
                   list(label='abc',fig.path='foo/bar-'))
  # 'function' is a reserved keyword; you should quote 'abc-function' in this case
  expect_identical(parse_params('abc-function,fig.path="foo/bar-"'),
                   list(label='abc-function',fig.path='"foo/bar-"'))
  expect_true(valid_opts('a, results="hide"'))
  expect_false(valid_opts('a, results=hide'))
  expect_true(valid_opts('a, fig.show="asis"'))
  expect_false(valid_opts('a, fig.show=animate'))
  opts_knit$set(sweave.penalty = 10)
})


test_that('parsing inline texts', {
  pat_rnw()

  res = parse_inline(c('aaa \\Sexpr{x}', 'bbb \\Sexpr{NA} and \\Sexpr{1+2}',
                       'another expression \\Sexpr{rnorm(10)}'))
  expect_identical(res$code, c('x', 'NA', '1+2', 'rnorm(10)'))
  expect_identical(nchar(res$input), 81L)

  knit_patterns$restore()
})
