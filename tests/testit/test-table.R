library(testit)

assert(
  'kable() works on data frames/matrices of one row',
  kable(data.frame(x=1, y=1), format = 'markdown') == c('|  x|  y|', '|--:|--:|', '|  1|  1|')
)
