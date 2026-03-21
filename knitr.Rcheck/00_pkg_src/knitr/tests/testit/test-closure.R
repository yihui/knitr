library(testit)

z = new_defaults(list(a = 1))

assert('$set() and $get() methods set/get values', {
  (z$get() %==% list(a = 1))
  ({z$set(a = 2); z$get('a')} %==% 2)
  ({z$set(b = 'f'); z$get(c('a', 'b'))} %==% list(a = 2, b = 'f'))
})

assert('$merge() does not change the object but returns a list of new values', {
  ({z$merge(list(b = 'g')); z$get('b')} %==% 'f')
  (z$merge(list(b = 'g')) %==% list(a = 2, b = 'g'))
})

assert('$get(default=TRUE) returns the initial value', {
  (z$get(default = TRUE) %==% list(a = 1))
})

assert('$get(names) keeps the names if drop = FALSE and one or more names do not exist', {
  (z$get(c('a', 'c')) %==% list(a = 2, c = NULL))
  (z$get('c') %==% NULL)
  (z$get('c', drop = FALSE) %==% list(c = NULL))
})

assert('$delete() deletes keys from the list', {
  z$set(b1 = TRUE, b2 = FALSE); z$delete(c('b1', 'b2'))
  (intersect(c('b1', 'b2'), names(z$get())) %==% character())
})

assert('$restore() restores to the initial value', {
  ({z$restore(); z$get()} %==% list(a = 1))
})

assert('$append() returns appended the chunk option', {
  z$set(d = 1)
  z$append(d = 2)
  (z$get('d') %==% c(1, 2))
})

z = new_defaults()

# named argument in set()
z$set(a = list(b = 2, c = 'qwer'))

assert('a named argument of list in $set() method is not treated as a list of options', {
  (length(z$get()) %==% 1L)
  # unnamed argument in set()
  ({z$restore(); z$set(list(b = 2, c = 'qwer')); z$get()} %==% list(b = 2, c = 'qwer'))
})
