library(testit)

kable2 = function(...) as.character(kable(...))

assert(
  'kable() works on data frames/matrices of one row',
  kable(data.frame(x=1, y=1), format = 'markdown') == c('|  x|  y|', '|--:|--:|', '|  1|  1|')
)

m = matrix(1:2, nrow = 1, dimnames = list('a', c('x', 'y')))
assert(
  'kable() does not discard row names when there is only one row',
  identical(kable2(m), c('|   |  x|  y|', '|:--|--:|--:|', '|a  |  1|  2|'))
)

assert(
  'kable() recycles the align argument correctly',
  identical(kable2(m, align = 'c'), c('|   | x | y |', '|:--|:-:|:-:|', '|a  | 1 | 2 |'))
)

assert(
  'kable() works on character data frames',
  identical(kable2(data.frame(x = 'a')), c('|x  |', '|:--|', '|a  |'))
)

assert(
  "kable() works on NA's",
  identical(kable2(data.frame(x=c(NA, FALSE))), c('|x     |', '|:-----|', '|NA    |', '|FALSE |'))
)

assert(
  'kable() does not add extra spaces to character columns',
  identical(
    kable2(data.frame(x = c(1.2, 4.87), y = c('fooooo', 'bar')), 'latex'),
    '
\\begin{tabular}{r|l}
\\hline
x & y\\\\
\\hline
1.20 & fooooo\\\\
\\hline
4.87 & bar\\\\
\\hline
\\end{tabular}'
  )
)
