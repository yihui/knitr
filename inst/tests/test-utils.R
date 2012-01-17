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
