context('Output functions')

test_that('automatic output file name works as expected', {

  res = unlist(lapply(c('abc.Rnw', 'abc.rnw', 'abc.tex', '_knit_abc.tex', '_knit_abc.md', 'foo_knit_.html'),
                      auto_out_name))
  expect_identical(res, c('abc.tex', 'abc.tex', 'abc-out.tex', 'abc.tex', 'abc.md', 'foo.html'))

  res = unlist(lapply(c('abc.rtex', 'abc.Rmd', 'abc.rhtm', 'abc.Rhtml', 'foo.abc.rhtml'),
                      auto_out_name))
  expect_identical(res, c('abc.tex', 'abc.md', 'abc.htm', 'abc.html', 'foo.abc.html'))

  expect_error(auto_out_name('foo.bar'))
})
