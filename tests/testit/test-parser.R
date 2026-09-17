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

read_chunk(lines = c('```{r foo}', '1+1', '```', '', 'text', '',
                     '```{r bar, echo=FALSE}', 'x <- 2', 'x * 3', '```'),
           path = 'a.Rmd')
assert('read_chunk() reads code chunks from an R Markdown document (#2041)', {
  (knit_code$get() %==% list(foo = '1+1', bar = c('x <- 2', 'x * 3')))
})

knit_code$restore()

read_chunk(lines = c('<<baz>>=', 'y <- 1', '@'), path = 'a.Rnw')
assert('read_chunk() reads code chunks from an R Sweave document (#2041)', {
  (knit_code$get() %==% list(baz = 'y <- 1'))
})

knit_code$restore()

read_chunk(lines = c('```{r}', '#| label: foo', '#| echo: false', '1+1', '```'),
           path = 'a.Rmd')
assert('read_chunk() reads the label from YAML chunk options and strips them (#2041)', {
  (knit_code$get() %==% list(foo = '1+1'))
})

knit_code$restore()

# an unlabeled chunk gets an automatic label; text and inline code are ignored
read_chunk(lines = c('```{r}', '1+1', '```'), path = 'a.Rmd')
assert('read_chunk() gives unlabeled document chunks an automatic label (#2041)', {
  (length(nm <- names(knit_code$get())) == 1L)
  (grepl('^unnamed-chunk-', nm))
})

knit_code$restore()

# a plain R script is still parsed as a script, not misdetected as a document
read_chunk(lines = c('# ---- foo ----', 'x <- 1  # ```{r} in a comment'), path = 'a.R')
assert('read_chunk() still parses .R scripts with @knitr markers (#2041)', {
  (knit_code$get() %==% list(foo = 'x <- 1  # ```{r} in a comment'))
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

assert('parse_chunk() ignores labels not found in knit_code', {
  # chunk 'e' doesn't exist
  (pc(c('3*3', '<<a>>', '  <<e>>', '<<b>>  ')) %==% c("3*3", "1+1", "  <<e>>", "2-2"))
})

assert('parse_chunk() expands references embedded in a line (#2034)', {
  # a reference followed by other code on the same line
  (pc(c('mtcars %>%', '  <<a>> %>%', '  <<b>>')) %==% c('mtcars %>%', '  1+1 %>%', '  2-2'))
  # multiple references on one line
  (pc('<<a>> + <<b>>') %==% '1+1 + 2-2')
  # unknown labels are left untouched
  (pc('foo(<<e>>)') %==% 'foo(<<e>>)')
})

assert('parse_chunk() splices multi-line chunks embedded in a line', {
  # chunk 'd' expands to multiple lines; a trailing reference splices cleanly
  (pc('x %>% <<d>>') %==% c('x %>% function() {', '  if (T)', '    1+1', '}'))
  # continuation lines are indented to the leading whitespace of the host line
  (pc('  y <- <<d>>') %==% c('  y <- function() {', '    if (T)', '      1+1', '  }'))
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
