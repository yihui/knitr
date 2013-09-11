library(testit)
message('testing closures created in new_defaults()')

z = new_defaults(list(a = 1))

assert(
  '$set() and $get() methods set/get values',
  identical(z$get(), list(a = 1)),
  identical({z$set(a = 2); z$get('a')}, 2),
  identical({z$set(b = 'f'); z$get(c('a', 'b'))}, list(a=2, b='f'))
)

assert(
  '$merge() does not change the object but returns a list of new values',
  identical({z$merge(list(b = 'g')); z$get('b')}, 'f'),
  identical(z$merge(list(b = 'g')), list(a=2, b='g'))
)

assert(
  '$get(default=TRUE) returns the initial value',
  identical(z$get(default = TRUE), list(a=1))
)

assert(
  '$restore() restores to the initial value',
  identical({z$restore(); z$get()}, list(a=1))
)


z = new_defaults()

# named argument in set()
z$set(a = list(b=2, c='qwer'))

assert(
  'a named argument of list in $set() method is not treated as a list of options',
  identical(length(z$get()), 1L),
  # unnamed argument in set()
  identical({z$restore(); z$set(list(b=2, c='qwer')); z$get()}, list(b=2, c='qwer'))
)
