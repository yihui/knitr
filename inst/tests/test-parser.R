context('Parser')

test_that('parsing options', {
  ## new syntax
  expect_identical(parse_params('a-s-d,b=TRUE,c=def'), alist(label='a-s-d',b=TRUE,c=def))
  expect_error(parse_params('a,b,c=qwer'))
  expect_error(parse_params('a,c=qwer', label=FALSE))
  expect_identical(parse_params('a,opt=c(1,3,5)'),alist(label='a',opt=c(1,3,5)))
  expect_identical(parse_params('label="xx",opt=zz'),alist(label='xx',opt=zz))
  expect_identical(parse_params('a,b=2,c="qwer",asdf="efg"'),
                   alist(label='a', b=2, c='qwer',asdf='efg'))
  ## back-compatibility with Sweave
  expect_identical(parse_params('abc,fig.path=foo/bar-'),
                   list(label='abc',fig.path='foo/bar-'))
})


test_that('parsing inline texts', {
  pat_rnw()
  
  res =
    parse_inline(c('aaa \\SweaveOpts{eval=TRUE,dev=png,animate=T}',
                   'bbb \\SweaveOpts{eval=F,dev=tikz,abc=1,comment=} and \\Sexpr{1+2}',
                   'another expression \\Sexpr{rnorm(10)}'))
  
  expect_identical(res$params, list(eval=FALSE, dev='tikz', abc=1L, comment=NA))
  expect_identical(res$code, c('1+2', 'rnorm(10)'))
  expect_identical(nchar(res$input), 62L)
  
  res = parse_inline('\\SweaveOpts{a=1}\\SweaveOpts{b=FALSE}')
  expect_identical(res$params, list(b=FALSE))
  expect_identical(nchar(res$input), 0L)
  
  res = parse_inline('blabla \\SweaveInput{abc.Rnw} foobar \\Sexpr{1+1}')
  expect_identical(res$code, c("knit_child('abc.Rnw')", "1+1"))
  
  knit_patterns$restore()
})
