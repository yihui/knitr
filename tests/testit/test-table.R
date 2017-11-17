library(testit)

kable2 = function(...) as.character(kable(...))

assert(
  'kable() works on data frames/matrices of one row',
  kable(data.frame(x = 1, y = 1), format = 'markdown') == c('|  x|  y|', '|--:|--:|', '|  1|  1|')
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
  'kable() align with strings correctly',
  identical(kable2(m, align = c('c', 'r')), kable2(m, align = 'cr'))
)

assert(
  'kable() works on character data frames',
  identical(kable2(data.frame(x = 'a')), c('|x  |', '|:--|', '|a  |'))
)

assert(
  "kable() works on NA's",
  identical(kable2(data.frame(x = c(NA, FALSE))), c('|x     |', '|:-----|', '|NA    |', '|FALSE |'))
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
    kable2(data.frame(x = c("10%", "5%"), col_name = c("3_8", "40_6")), 'latex'),
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
    kable2(data.frame(x = c("10%", "5%"), col_name = c("3_8", "40_6")), 'latex', escape = FALSE),
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
    kable2(data.frame(x = c("10<>", "5&2"), "y" = c("3>8", "\"40\"")), 'html'),
    "<table>\n <thead>\n  <tr>\n   <th style=\"text-align:left;\"> x </th>\n   <th style=\"text-align:left;\"> y </th>\n  </tr>\n </thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:left;\"> 10&lt;&gt; </td>\n   <td style=\"text-align:left;\"> 3&gt;8 </td>\n  </tr>\n  <tr>\n   <td style=\"text-align:left;\"> 5&amp;2 </td>\n   <td style=\"text-align:left;\"> &quot;40&quot; </td>\n  </tr>\n</tbody>\n</table>"
  )
)

assert(
  'kable() doesn\'t escape HTML special characters when escape = FALSE',
  identical(
    kable2(data.frame(x = c("10<>", "5&2"), "y" = c("3>8", "\"40\"")), 'html', escape = FALSE),
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

assert(
  'kable() works on matrices with duplicate row names',
  identical(
    kable2(matrix(c(1, 1, 1, 1), ncol = 2, dimnames = list(c('A', 'A'), c('B', 'B')))),
    c('|   |  B|  B|', '|:--|--:|--:|', '|A  |  1|  1|', '|A  |  1|  1|')
  )
)

assert(
  'kable() works on matrices with NA colname',
  identical(
    kable2(matrix(c(1, 1, 1, 1), ncol = 2, dimnames = list(c('A', NA), c('B', NA)))),
    c("|   |  B| NA|", "|:--|--:|--:|", "|A  |  1|  1|", "|NA |  1|  1|")
  )
)

# edge cases (should not error)
x1 = matrix(NA, 0, 0)
x2 = matrix(NA, 0, 1)
x3 = matrix(NA, 1, 0)
for (f in c('pandoc', 'html', 'latex', 'rst')) {
  kable(x1, f)
  kable(x2, f)
  kable(x3, f)
}

colnames(x2) = 'a'
assert(
  'kable(, "markdown") works for a 0 row 1 column matrix',
  identical(kable2(x2, 'markdown'), c('|a  |', '|:--|'))
)

assert('kable(, "pandoc", caption = "Table Caption") works for a 1-column matrix', {
  x4 = matrix(1:2, ncol = 1, dimnames = list(NULL, 'a'))
  kable2(x4, 'pandoc', caption = 'Table Caption') %==%
    c('Table: Table Caption', '', '|  a|', '|--:|', '|  1|', '|  2|')
})

assert(
  'kable() works on an irregular matrix',
  identical(
    kable2(matrix(list('a', 2, 3, 4), nrow = 2), col.names = c('a', 'b')),
    c('|a  |b  |', '|:--|:--|', '|a  |3  |', '|2  |4  |')
  )
)

assert(
  'has_rownames() works',
  !has_rownames(matrix(1:4, 2)), !has_rownames(iris), has_rownames(mtcars),
  !has_rownames(as.data.frame(matrix(nrow = 0, ncol = 3)))
)

op = options(knitr.kable.NA = '')
assert(
  'kable() can display NA as emtpy strings',
  kable2(matrix(c(1, NA, 3, 4), nrow = 2), col.names = c('a', 'b')) %==%
    c('|  a|  b|', '|--:|--:|', '|  1|  3|', '|   |  4|')
)
options(op)
