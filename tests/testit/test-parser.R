library(testit)

assert(
  'parse_params() parses chunk options to a list',
  identical(parse_params('a-s-d,b=TRUE,c=def'), alist(label='a-s-d',b=TRUE,c=def)),
  has_error(parse_params('a,b')),
  has_error(parse_params('a,b,c=qwer')),
  identical(parse_params('a,opt=c(1,3,5)'),alist(label='a',opt=c(1,3,5))),
  identical(parse_params('label="xx",opt=zz'),alist(label='xx',opt=zz)),
  identical(parse_params('label=foo'),alist(label='foo')),
  identical(parse_params('a,b=2,c="qwer",asdf="efg"'),
            alist(label='a', b=2, c='qwer',asdf='efg')),
  identical(parse_params('2a'), alist(label='2a')),
  identical(parse_params('abc-function,fig.path="foo/bar-"'),
            alist(label='abc-function', fig.path="foo/bar-"))
)


res = parse_inline(c('aaa \\Sexpr{x}', 'bbb \\Sexpr{NA} and \\Sexpr{1+2}',
                     'another expression \\Sexpr{rnorm(10)}'), all_patterns$rnw)
assert(
  'parse_inline() parses inline text',
  identical(res$code, c('x', 'NA', '1+2', 'rnorm(10)')),
  identical(nchar(res$input), 81L),
  # empty inline code is not recognized
  identical(parse_inline('\\Sexpr{}', all_patterns$rnw)$code, character(0)),
  # can use > in HTML inline code
  identical(parse_inline('<!--rinline "<a>" -->', all_patterns$html)$code, ' "<a>" ')
)
rm(res)
