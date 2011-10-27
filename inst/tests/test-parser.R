context('Parser')

test_that('parsing options', {
    expect_identical(parse_params('a,b=T,c=def'), list(label='a',b=TRUE,c='def'))
    expect_error(parse_params('a,b,c=qwer'))
    expect_identical(parse_params('label=xx,opt=zz'),list(label='xx',opt='zz'))
})
