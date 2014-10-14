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

assert(
  'kable() escapes LaTeX special characters by default',
  identical(
    kable2(data.frame(x=c("10%","5%"),col_name=c("3_8","40_6")), 'latex'),
    '
\\begin{tabular}{l|l}
\\hline
x & col\\_name\\\\
\\hline
10\\% & 3\\_8\\\\
\\hline
5\\% & 40\\_6\\\\
\\hline
\\end{tabular}'
  )
)

assert(
  'kable() doesn\'t escape LaTeX special characters when escape = FALSE',
  identical(
    kable2(data.frame(x=c("10%","5%"),col_name=c("3_8","40_6")), 'latex', escape = FALSE),
    '
\\begin{tabular}{l|l}
\\hline
x & col_name\\\\
\\hline
10% & 3_8\\\\
\\hline
5% & 40_6\\\\
\\hline
\\end{tabular}'
  )
)

assert(
  'kable() escapes HTML special characters by default',
  identical(
    kable2(data.frame(x=c("10<>","5&2"),"y"=c("3>8","\"40\"")), 'html'),
    "<table>\n <thead>\n  <tr>\n   <th style=\"text-align:left;\"> x </th>\n   <th style=\"text-align:left;\"> y </th>\n  </tr>\n </thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:left;\"> 10&lt;&gt; </td>\n   <td style=\"text-align:left;\"> 3&gt;8 </td>\n  </tr>\n  <tr>\n   <td style=\"text-align:left;\"> 5&amp;2 </td>\n   <td style=\"text-align:left;\"> &quot;40&quot; </td>\n  </tr>\n</tbody>\n</table>"
  )
)

assert(
  'kable() doesn\'t escape HTML special characters when escape = FALSE',
  identical(
    kable2(data.frame(x=c("10<>","5&2"),"y"=c("3>8","\"40\"")), 'html', escape = FALSE),
    "<table>\n <thead>\n  <tr>\n   <th style=\"text-align:left;\"> x </th>\n   <th style=\"text-align:left;\"> y </th>\n  </tr>\n </thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:left;\"> 10<> </td>\n   <td style=\"text-align:left;\"> 3>8 </td>\n  </tr>\n  <tr>\n   <td style=\"text-align:left;\"> 5&2 </td>\n   <td style=\"text-align:left;\"> \"40\" </td>\n  </tr>\n</tbody>\n</table>"
  )
)


assert(
  'kable(digits = vector) works on numeric matrices',
  identical(
    kable2(matrix(c(1.1, 1.2, 2.3, 2.4), 2, dimnames = list(NULL, c('a', 'b'))),
           digits = c(0, 1)),
    c('|  a|   b|', '|--:|---:|', '|  1| 2.3|', '|  1| 2.4|')
  )
)
