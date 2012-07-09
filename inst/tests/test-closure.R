context('new_defaults()')

test_that('set(), get(), merge() and restore() methods', {
  z = new_defaults(list(a = 1))
  expect_identical(z$get(), list(a = 1))
  z$set(a = 2)
  expect_identical(z$get('a'), 2)
  z$set(b = 'f')
  expect_identical(z$get(c('a', 'b')), list(a=2, b='f'))
  # merge() does not change z
  z$merge(list(b = 'g'))
  expect_identical(z$get('b'), 'f')
  # but the returned value contains the merge
  expect_identical(z$merge(list(b = 'g'))[['b']], 'g')
  expect_identical(z$get(default = TRUE), list(a=1))
  z$restore()
  expect_identical(z$get(), list(a=1))
})

test_that('a named argument of list in $set() method is not treated as a list of options', {
  z = new_defaults()
  # named argument in set()
  z$set(a = list(b=2, c='qwer'))
  expect_identical(length(z$get()), 1L)
  z$restore()
  # unnamed argument in set()
  z$set(list(b=2, c='qwer'))
  expect_identical(z$get(), list(b=2, c='qwer'))
})

