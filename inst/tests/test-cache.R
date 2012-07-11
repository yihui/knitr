context('Cache')

test_that('find_globals() identifies global variables', {
  # nothing from outside environment
  expect_identical(find_globals('x=1'), character(0))
  # qwer must be created outside somewhere
  expect_identical(find_globals('a=1; b=a; d=qwer'), 'qwer')
  expect_identical(find_globals('a=function(){f=2;g}'), 'g')
  # y was assigned locally in z, but there is another y outside from nowhere
  expect_identical(find_globals('z=function(){y=1};y'), 'y')
  # more complicated cases: operators, subscripts, ...
  expect_identical(find_globals(c('a=1%*%1%o%2 %in% d','b=d%%10+3%/%2-z[1:3]')), c('d', 'z'))
})

test_that('dep_prev() sets dependencies on previous chunks', {
  knit_code$set(a=1, b=2, c=3)
  # dependency is empty now
  expect_identical(dep_list$get(), list())
  dep_prev()
  # b/c depend on a, c depends on b
  expect_identical(dep_list$get(), list(a=c('b', 'c'), b='c'))
  dep_list$restore()
  knit_code$restore()
})
