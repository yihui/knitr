library(testit)

assert(
  'kable() works on data frames/matrices of one row',
  kable(data.frame(x=1, y=1), format = 'markdown') == c('|  x|  y|', '|--:|--:|', '|  1|  1|')
)

m = matrix(1:2, nrow = 1, dimnames = list('a', c('x', 'y')))
assert(
  'kable() does not discard row names when there is only one row',
  identical(kable(m), c('|   |  x|  y|', '|:--|--:|--:|', '|a  |  1|  2|'))
)

assert(
  'kable() recycles the align argument correctly',
  identical(kable(m, align = 'c'), c('|   | x | y |', '|:--|:-:|:-:|', '|a  | 1 | 2 |'))
)

assert(
  'kable() works on character data frames',
  identical(kable(data.frame(x = 'a')), c("|x  |", "|:--|", "|a  |"))
)
