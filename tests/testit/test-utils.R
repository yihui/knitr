library(testit)

assert(
  'abs_path() recognizes absolute paths under Windows and *nix',
  !is_abs_path('abc/def'),
  is_abs_path(if (.Platform$OS.type == 'windows') {
    c('D:\\abc', '\\\\netdrive\\somewhere')
  } else '/abc/def')
)

op = options(digits = 4, scipen = 0)

assert(
  'format_sci() turns numbers into scientific notations',
  identical(format_sci(1), '1'),
  identical(format_sci(0), '0'),
  identical(format_sci(c(1.84e8, 1e5, 2.34e3)),
            c('1.84\\times 10^{8}', '10^{5}', '2340')),
  identical(format_sci(1.23456789*10^-5), '1.2346\\times 10^{-5}'),
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

assert(
  'the inline hook for Rnw applies \\ensuremath{} correctly',
  .inline.hook.tex(1e4) == '\\ensuremath{10^{4}}',
  .inline.hook.tex(c(1.2345e10,2* pnorm(-(3:4)))) ==
    "\\ensuremath{1.2345\\times 10^{10}}, 0.0027, \\ensuremath{6.3342\\times 10^{-5}}"
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
