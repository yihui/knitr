library(testit)

opts_knit$set(out.format = 'markdown')
assert('parse_block() parses the language engine from ```{lang}', {
  (parse_block(NULL, '', 'r, foo, a=1,')$params %==% alist(label = 'foo', a = 1))
  (parse_block(NULL, '', 'Rcpp, foo, a=1,')$params %==% alist(label = 'foo', a = 1, engine = 'Rcpp'))
})

res = split_file(
  c('abc', '```{r foo}', '1+1', '```{r bar}', '2+2', '```', 'def'),
  patterns = all_patterns$md
)
assert('split_file() treats ``` as part of code chunk instead of beginning of text chunk', {
  # the foo chunk does not have a closing mark
  (c(knit_code$get('foo')) %==% '1+1')  # use c() to drop attributes
  (c(knit_code$get('bar')) %==% '2+2')
  # before knitr v1.6, the text chunk was c('', 'def')
  (res[[4]][['input']] %==% 'def')
})
opts_knit$restore()
knit_code$restore(); knit_concord$restore()

res = parse_inline(c('aaa \\Sexpr{x}', 'bbb \\Sexpr{NA} and \\Sexpr{1+2}',
                     'another expression \\Sexpr{rnorm(10)}'), all_patterns$rnw)
assert('parse_inline() parses inline text', {
  (res$code %==% c('x', 'NA', '1+2', 'rnorm(10)'))
  (nchar(res$input) %==% 81L)
  # empty inline code is not recognized
  (parse_inline('\\Sexpr{}', all_patterns$rnw)$code %==% character(0))
  # can use > in HTML inline code
  (parse_inline('<!--rinline "<a>" -->', all_patterns$html)$code %==% ' "<a>" ')
})

res = parse_inline('inline expressions `r pi+1`, +r cos(0)+ in AsciiDoc',
                   all_patterns$asciidoc)
assert('both `r expression` and +r expression+ work for AsciiDoc', {
  (res$code %==% c('pi+1', 'cos(0)'))
})

knit_code$restore()

read_chunk(lines = c('1+1'))
assert('read_chunk() does not discard code without chunk headers', {
  (knit_code$get() %==% list('unnamed-chunk-1' = '1+1'))
})

knit_code$restore()

read_chunk(lines = c('# ---- foo ----', '1+1'))
assert('read_chunk() can identify chunk labels', {
  (knit_code$get() %==% list(foo = '1+1'))
})

knit_code$restore()

# chunk references with <<>> --------------------------------------------------

knit_code$restore(list(
  a = '1+1', b = '2-2', c = c('if (T)', '  <<a>>'), d = c('function() {', '  <<c>>', '}')
))
pc = function(x) parse_chunk(x, all_patterns$rnw$ref.chunk)

assert('parse_chunk() preserves indentation', {
  (pc(c('3*3', '<<a>>', ' <<b>>', 'if (T)', '  <<a>>')) %==% c("3*3", "1+1", " 2-2", "if (T)", "  1+1" ))
  (pc('<<d>>') %==% c('function() {', '  if (T)', '    1+1', '}'))
})

knit_code$restore()

# duplication of labels

knit_code$restore(list(a = '1+1'))

assert('duplicated labels are not allowed by default', {
  (!has_error(parse_block(NULL, '', 'label = "a"')))
  (has_error(parse_block('2+2', '', 'label = "a"')))
  (has_error(parse_block(NULL, '','label = "a", code = "2+2"')))
  (has_error(parse_block(NULL, '','label = "a", file = "dummy.R"')))
})
op = options(knitr.duplicate.label = 'allow')
assert('duplicated labels are allowed after setting an option', {
  (!has_error(parse_block('2+2', '', 'label = "a"')))
  (!has_error(parse_block(NULL, '','label = "a", code = "2+2"')))
  (!has_error(parse_block(NULL, '','label = "a", file = "dummy.R"')))
})
options(op)

knit_code$restore()
