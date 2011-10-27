context('Output functions')

test_that('automatic output file name works as expected', {
    expect_identical(auto_out_name('abc.Rnw'), 'abc.tex')
    expect_identical(auto_out_name('abc.rnw'), 'abc.tex')
    expect_identical(auto_out_name('abc.tex'), 'abc-out.tex')
    expect_identical(auto_out_name('_knit_abc.tex'), 'abc.tex')
    expect_identical(auto_out_name('_knit_abc.md'), 'abc.md')
})
