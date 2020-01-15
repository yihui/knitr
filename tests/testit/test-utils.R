library(testit)

assert('abs_path() recognizes absolute paths under Windows and *nix', {
  (!is_abs_path('abc/def'))
  (is_abs_path(if (.Platform$OS.type == 'windows') {
    c('D:\\abc', '\\\\netdrive\\somewhere')
  } else '/abc/def'))
})

op = options(digits = 3, scipen = 0, knitr.digits.signif = TRUE)

assert(
  'format_sci() uses correct number of significant digits',
  format_sci(1) %==% '1',
  format_sci(0) %==% '0',
  format_sci(3.1415e2) %==% '314',
  format_sci(3.1415) %==% '3.14'
)

options(op)

op = options(digits = 14, scipen = 0, knitr.digits.signif = TRUE)
assert(
  'format_sci() prints numerics at maximum number of significant digits',
  format_sci(3.14159265358979) %==% '3.1415926535898'
)
options(op)

op = options(digits = 4, scipen = 0)

assert(
  'format_sci() turns numbers into scientific notations',
  identical(format_sci(c(1.84e8, 1e5, 2.34e3)),
            c('1.84\\times 10^{8}', '10^{5}', '2340')),
  identical(format_sci(1.23456789 * 10^-5), '1.2346\\times 10^{-5}'),
  identical(format_sci(9.87654e6, 'html'), '9.8765 &times; 10<sup>6</sup>'),
  identical(format_sci(9.87654e6, 'rst'), '9.8765 |times| 10 :sup:`6`'),
  identical(format_sci(letters), letters),
  identical(format_sci(NA_real_), NA_character_)
)

assert(
  'format_sci() coerces non-numeric and non-double values to characters',
  format_sci(Sys.Date()) == as.character(Sys.Date()),
  format_sci(1000000L) == '1000000'
)

# https://github.com/yihui/knitr/issues/1625
assert('format_sci() does not convert roman numerals to arabic numerals', {
  format_sci(as.roman(c(1, 4, 7, 33, 100))) %==% c('I', 'IV', 'VII', 'XXXIII', 'C')
})

assert(
  'format_sci() for Rnw does not add \\ensuremath{} at all',
  !grepl('[\\]ensuremath', format_sci(c(1e4, 1.2345e10, 2 * pnorm(-(3:4)), -Inf)))
)

assert(
  'the inline hook for Rnw applies \\ensuremath{} correctly',
  .inline.hook.tex(1e4) == '\\ensuremath{10^{4}}',
  .inline.hook.tex(-Inf) == '\\ensuremath{-\\infty{}}',
  .inline.hook.tex(-1.23) == '-1.23',
  .inline.hook.tex(c(1.2345e10, 2 * pnorm(-(3:4)))) ==
    "\\ensuremath{1.2345\\times 10^{10}}, 0.0027, \\ensuremath{6.3342\\times 10^{-5}}"
)

assert(
  'Infinity and NaN are formatted correctly',
  identical(format_sci(-Inf), '-\\infty{}'),
  identical(format_sci(-Inf, 'html'), '-&infin;'),
  identical(format_sci(-Inf, 'rst'), '-Inf'),
  identical(format_sci(NaN), 'NaN')
)

assert(
  'sanitize_fn() warns against spaces in filenames',
  has_warning(sanitize_fn('figure/a b'))
)

options(op)

assert(
  'fig_path() sanitizes paths',
  identical(sanitize_fn('fig/foo', '.png'), 'fig/foo.png'),
  suppressWarnings(c(
    identical(sanitize_fn('figure/a b'), 'figure/a_b'),
    identical(sanitize_fn('fig space/a.b'), 'fig_space/a_b'),
    identical(sanitize_fn('../c.d'), '../c_d'),
    identical(sanitize_fn('./../c..d'), './../c__d')
  )),
  identical(sanitize_fn('C:/foo/bar'), 'C:/foo/bar')
)

assert(
  'fig_chunk() generates figure filenames for a code chunk',
  identical(fig_chunk('foo'), 'figure/foo-1'),
  identical(fig_chunk('foo', 'pdf'), 'figure/foo-1.pdf'),
  identical(fig_chunk('foo', 'png', 2), 'figure/foo-2.png'),
  identical(fig_chunk('foo', 'svg', 1:5), sprintf('figure/foo-%d.svg', 1:5)),
  identical(fig_chunk('foo', fig.path = 'my_figure/'), 'my_figure/foo-1'),
  identical(fig_chunk('foo', '.pdf'), 'figure/foo-1.pdf')
)

assert('all_figs() generates all figure paths for a code chunk', {
  opts = list(fig.path = 'abc/', label = 'foo', fig.num = 3)
  (all_figs(opts, '.svg') %==% sprintf('abc/foo-%d.svg', 1:3))
  (all_figs(opts, c('png', 'pdf'))  %==% apply(
    expand.grid(1:3, c('.png', '.pdf')), 1, function(x) {
      paste0(c('abc/foo-', x), collapse = '')
    }
  ))
})

f = file.path(R.home('doc'), 'html', 'logo.jpg')
assert(
  'base64_encode() gets the same result as markdown:::.b64EncodeFile',
  identical(strsplit(markdown:::.b64EncodeFile(f), 'base64,')[[1]][2],
            base64_encode(readBin(f, what = 'raw', n = file.info(f)$size)))
)

assert(
  'escape_latex() escapes special LaTeX characters',
  identical(escape_latex('# $ % & ~ _ ^ \\ { }'),
            '\\# \\$ \\% \\& \\textasciitilde{} \\_ \\textasciicircum{} \\textbackslash{} \\{ \\}')
)

assert(
  'indent_block() works when the first element is empty (#790)',
  identical(indent_block(c('', 'a')), c('    ', '    a')),
  identical(indent_block(c('', '')),  c('    ', '    '))
)

assert(
  'current_input() returns NULL by default',
  is.null(current_input()), suppressWarnings(is.null(current_input(TRUE)))
)

assert(
  'color_def() generates LaTeX code to define a color variable',
  color_def(NA) == '',
  color_def('red') == '\\definecolor{shadecolor}{rgb}{1, 0, 0}',
  color_def('#00ff00') == '\\definecolor{shadecolor}{rgb}{0, 1, 0}',
  color_def('.5,.6,.7', 'fgcolor') == '\\definecolor{fgcolor}{rgb}{.5, .6, .7}'
)

cw = function(...) unclass(combine_words(...))
assert(
  'combine_words() combines multiple words into a single string',
  cw(NULL) %==% NULL,
  cw(c('a')) %==% 'a',
  cw(c('a', 'b')) %==% 'a and b',
  cw(c('a', 'b', 'c')) %==% 'a, b, and c',
  cw(c('a', 'b', 'c'), and = '') %==% 'a, b, c',
  cw(c('a', 'b', 'c'), ' / ', '') %==% 'a / b / c',
  cw(c('a', 'b', 'c'), before = '"') %==% '"a", "b", and "c"',
  cw(c('a', 'b', 'c'), before = '``', after = "''") %==% "``a'', ``b'', and ``c''"
)
rm(list = 'cw')

opts = list(fig.cap = 'Figure "caption" <>.', fig.lp = 'Fig:', label = 'foo')
assert(
  '.img.cap() generates the figure caption and alt attribute',
  .img.cap(opts, FALSE) %==% opts$fig.cap,
  .img.cap(opts, TRUE)  %==% 'Figure &quot;caption&quot; &lt;&gt;.'
)

z = as.strict_list(list(a = 1, aa = 2, bbb = 3))
assert(
  'as.strict_list() does not allow partial matching',
  z$b %==% NULL, z$bbb %==% 3
)

out = c('*hello*', raw_output('<special>content</special> *protect* me!'), '*world*')
pre = extract_raw_output(out)
pre$value = gsub('[*]([^*]+)[*]', '<em>\\1</em>', pre$value)  # think this as Pandoc conversion
# raw output was protected from the conversion (e.g. *protect* was not converted)
assert(
  'restore_raw_output() restores raw output',
  restore_raw_output(pre$value, pre$chunks) %==%
    '<em>hello</em>\n<special>content</special> *protect* me!\n<em>world</em>'
)

assert('raw_block() returns a raw attribute block for Pandoc', {
  (raw_latex('\\emph{x}') == '\n```{=latex}\n\\emph{x}\n```\n')
  (raw_html('<i>foo</i>') == '\n```{=html}\n<i>foo</i>\n```\n')
})

assert('block_class() turns a character vector into Pandoc attributes for code block classes', {
  (block_class(NULL) %==% NULL)
  (block_class('a') %==% '.a')
  (block_class('a b') %==% c('.a', '.b'))
  (block_class(c('a', 'b')) %==% c('.a', '.b'))
})

assert('block_attr(x) turns a character vector into Pandoc attributes', {
  (block_attr(NULL) %==% NULL)
  (block_attr(NULL, lang = 'r') %==% 'r')
  (block_attr('.a') %==% '{.a}')
  (block_attr('.a b="11"') %==% '{.a b="11"}')
  (block_attr(c('.a', 'b="11"')) %==% '{.a b="11"}')
})
