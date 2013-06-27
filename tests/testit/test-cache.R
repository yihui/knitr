library(testit)

assert(
  'find_globals() identifies global variables',
  # nothing from outside environment
  identical(find_globals('x=1'), character(0)),
  # qwer must be created outside somewhere
  identical(find_globals('a=1; b=a; d=qwer'), 'qwer'),
  identical(find_globals('a=function(){f=2;g}'), 'g'),
  # y was assigned locally in z, but there is another y outside from nowhere
  identical(find_globals('z=function(){y=1};y'), 'y'),
  # more complicated cases: operators, subscripts, ...
  identical(find_globals(c('a=1%*%1%o%2 %in% d','b=d%%10+3%/%2-z[1:3]')), c('d', 'z'))
)

knit_code$set(a=1, b=2, c=3)
assert(
  'dep_prev() sets dependencies on previous chunks',
  # dependency is empty now
  identical(dep_list$get(), list()),
  # b/c depend on a, c depends on b
  identical({dep_prev(); dep_list$get()}, list(a=c('b', 'c'), b='c'))
)
dep_list$restore()
knit_code$restore()
