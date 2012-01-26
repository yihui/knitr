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

test_that('echo_index() returns expected indices', {
    expect_identical(echo_index(c(1,4,5),2,6), 4L)
    expect_identical(echo_index(c(1,4,5),-1,6), 4:6)
    expect_identical(echo_index(c(1,4,5),NULL,6), NULL)
    expect_identical(echo_index(c(1,3,4,6),c(2,4),8), c(3L,6:8))
})
