context('Utils')

test_that('abs_path() is working', {
  expect_false(is_abs_path('abc/def'))
  if (.Platform$OS.type == 'windows') {
    expect_true(is_abs_path('D:\\abc'))
    expect_true(is_abs_path('\\\\netdrive\\somewhere'))
  } else {
    expect_true(is_abs_path('/abc/def'))
  }
})

test_that('scientific notation with format_sci()',{
  options(digits = 4, scipen = 0)
  expect_identical(format_sci(1), '1')
  expect_identical(format_sci(0), '0')
  expect_identical(format_sci(c(1.84e8, 1e5, 2.34e3)),
                   c(sprintf("\\ensuremath{%s}", c("1.84\\times 10^{8}", "10^{5}")), "2340"))
  expect_identical(format_sci(1.23456789*10^-5), "\\ensuremath{1.2346\\times 10^{-5}}")
  expect_identical(format_sci(9.87654e6, 'html'), "9.8765 &times; 10<sup>6</sup>")
  expect_identical(format_sci(9.87654e6, 'rst'), "9.8765 |times| 10 :sup:`6`")
  expect_identical(format_sci(letters), letters)
  expect_identical(format_sci(NA_real_), NA_character_)
})

test_that('fig_path() sanitizes paths', {
  opts_knit$set(out.format = 'latex')
  expect_identical(fig_path('.png', list(fig.path = 'fig/', label = 'foo')), 'fig/foo.png')
  opts = list(fig.path = 'figure/', label = 'a b')
  expect_warning(fig_path(, opts))
  suppressWarnings({
    expect_identical(fig_path(, opts), 'figure/a_b')
    expect_identical(fig_path(, list(fig.path = 'fig space/', label = 'a.b')), 'fig_space/a_b')
    expect_identical(fig_path(, list(fig.path = '../', label = 'c.d')), '../c_d')
    expect_identical(fig_path(, list(fig.path = './../', label = 'c..d')), './../c__d')
  })
  opts_knit$set(out.format = NULL)
})

test_that('base64_encode() gets the same result as markdown:::.b64EncodeFile', {
  f = file.path(R.home('doc'), "html", "logo.jpg")
  expect_identical(markdown:::.b64EncodeFile(f), image_uri(f))
})

test_that('escape special LaTeX characters', {
  expect_identical(escape_latex('# $ % & ~ _ ^ \\ { }'),
                   '\\# \\$ \\% \\& \\textasciitilde{} \\_ \\textasciicircum{} \\textbackslash{} \\{ \\}')
})
