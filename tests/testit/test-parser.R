library(testit)

assert(
  'parse_params() parses chunk options to a list',
  identical(parse_params('a-s-d,b=TRUE,c=def'), alist(label = 'a-s-d', b = TRUE, c = def)),
  has_error(parse_params('a,b')),
  has_error(parse_params('a,b,c=qwer')),
  identical(parse_params('a,opt=c(1,3,5)'), alist(label = 'a', opt = c(1, 3, 5))),
  identical(parse_params('label="xx",opt=zz'), alist(label = 'xx', opt = zz)),
  identical(parse_params('label=foo'), alist(label = 'foo')),
  identical(parse_params('a,b=2,c="qwer",asdf="efg"'),
            alist(label = 'a', b = 2, c = 'qwer', asdf = 'efg')),
  identical(parse_params('2a'), alist(label = '2a')),
  identical(parse_params('abc-function,fig.path="foo/bar-"'),
            alist(label = 'abc-function', fig.path = "foo/bar-"))
)

opts_knit$set(out.format = 'markdown')
assert(
  'parse_params() parses the language engine from ```{lang}',
  identical(
    parse_block(NULL, '', 'r, foo, a=1,')$params,
    alist(label = 'foo', a = 1)
  ),
  identical(
    parse_block(NULL, '', 'Rcpp, foo, a=1,')$params,
    alist(label = 'foo', a = 1, engine = 'Rcpp')
  )
)

res = split_file(
  c('abc', '```{r foo}', '1+1', '```{r bar}', '2+2', '```', 'def'),
  patterns = all_patterns$md
)
assert(
  'split_file() treats ``` as part of code chunk instead of beginning of text chunk',
  # the foo chunk does not have a closing mark
  knit_code$get('foo') == '1+1',
  knit_code$get('bar') == '2+2',
  # before knitr v1.6, the text chunk was c('', 'def')
  identical(res[[4]][['input']], 'def')
)
opts_knit$restore()
knit_code$restore(); knit_concord$restore()

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

res = parse_inline('inline expressions `r pi+1`, +r cos(0)+ in AsciiDoc',
                   all_patterns$asciidoc)
assert(
  'both `r expression` and +r expression+ work for AsciiDoc',
  identical(res$code, c('pi+1', 'cos(0)'))
)

knit_code$restore()

read_chunk(lines = c('1+1'))
assert(
  'read_chunk() does not discard code without chunk headers',
  identical(knit_code$get(), list('unnamed-chunk-1' = '1+1'))
)

knit_code$restore()

read_chunk(lines = c('# ---- foo ----', '1+1'))
assert(
  'read_chunk() can identify chunk labels',
   identical(knit_code$get(), list(foo = '1+1'))
)

knit_code$restore()

# chunk references with <<>> --------------------------------------------------

knit_code$restore(list(
  a = '1+1', b = '2-2', c = c('if (T)', '  <<a>>'), d = c('function() {', '  <<c>>', '}')
))
pc = function(x) parse_chunk(x, all_patterns$rnw$ref.chunk)

assert(
  'parse_chunk() preserves indentation',
  identical(pc(c('3*3', '<<a>>', ' <<b>>', 'if (T)', '  <<a>>')),
            c("3*3", "1+1", " 2-2", "if (T)", "  1+1" )),
  identical(pc('<<d>>'), "function() {\n  if (T)\n    1+1\n}")
)

knit_code$restore()
