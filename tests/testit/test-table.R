library(testit)

kable2 = function(...) as.character(kable(...))

assert('kable() works on data frames/matrices of one row', {
  (kable2(data.frame(x = 1, y = 1), format = 'pipe') %==%
     c('|  x|  y|', '|--:|--:|', '|  1|  1|'))
})

m = matrix(1:2, nrow = 1, dimnames = list('a', c('x', 'y')))
assert('kable() does not discard row names when there is only one row', {
  (kable2(m) %==% c('|   |  x|  y|', '|:--|--:|--:|', '|a  |  1|  2|'))
})

assert('kable() can assign a column name for row names', {
  (kable2(m, col.names = c('z', colnames(m))) %==%
     c('|z  |  x|  y|', '|:--|--:|--:|', '|a  |  1|  2|'))
})

assert('kable() recycles the align argument correctly', {
  (kable2(m, align = 'c') %==%
     c('|   | x | y |', '|:--|:-:|:-:|', '|a  | 1 | 2 |'))
})

assert('kable() align with strings correctly', {
  (kable2(m, align = c('c', 'r')) %==% kable2(m, align = 'cr'))
})

assert('kable() works on character data frames', {
  (kable2(data.frame(x = 'a')) %==% c('|x  |', '|:--|', '|a  |'))
})

assert("kable() works on NA's", {
  (kable2(data.frame(x = c(NA, FALSE))) %==%
     c('|x     |', '|:-----|', '|NA    |', '|FALSE |'))
})

assert('kable() works with the jira format', {
  (kable2(m, 'jira') %==% c('||   ||  x||  y||', '|a  |  1|  2|'))
})

assert('kable() does not add extra spaces to character columns', {
  (kable2(data.frame(x = c(1.2, 4.87), y = c('fooooo', 'bar')), 'latex') %==% '
\\begin{tabular}{r|l}
\\hline
x & y\\\\
\\hline
1.20 & fooooo\\\\
\\hline
4.87 & bar\\\\
\\hline
\\end{tabular}')
})

assert('kable() escapes LaTeX special characters by default', {
  (kable2(data.frame(x = c('10%', '5%'), col_name = c('3_8', '40_6')), 'latex') %==% '
\\begin{tabular}{l|l}
\\hline
x & col\\_name\\\\
\\hline
10\\% & 3\\_8\\\\
\\hline
5\\% & 40\\_6\\\\
\\hline
\\end{tabular}')
})

assert("kable() doesn't escape LaTeX special characters when escape = FALSE", {
  (kable2(data.frame(x = c('10%', '5%'), col_name = c('3_8', '40_6')), 'latex', escape = FALSE) %==% '
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
})

assert('kable() adds {} before [] when booktabs = TRUE', {
  (kable2(data.frame(x = c('[0, 1]', '(1, 2]'), y = c(35, 62)), 'latex', booktabs = TRUE) %==% '
\\begin{tabular}{lr}
\\toprule
x & y\\\\
\\midrule
{}[0, 1] & 35\\\\
(1, 2] & 62\\\\
\\bottomrule
\\end{tabular}'
   )
})

assert('kable(format = "latex", linesep = ...) works', {
  (kable2(data.frame(x = 1:4), 'latex', linesep  = c('', '', '\\midrule')) %==% '
\\begin{tabular}{r}
\\hline
x\\\\
\\hline
1\\\\
2\\\\
3\\\\
\\midrule
4\\\\
\\hline
\\end{tabular}')
})

assert('kable() escapes HTML special characters by default', {
  (kable2(data.frame(x = c('10<>', '5&2'), y = c('3>8', '"40"')), 'html') %==%
     '<table>\n <thead>\n  <tr>\n   <th style="text-align:left;"> x </th>\n   <th style="text-align:left;"> y </th>\n  </tr>\n </thead>\n<tbody>\n  <tr>\n   <td style="text-align:left;"> 10&lt;&gt; </td>\n   <td style="text-align:left;"> 3&gt;8 </td>\n  </tr>\n  <tr>\n   <td style="text-align:left;"> 5&amp;2 </td>\n   <td style="text-align:left;"> &quot;40&quot; </td>\n  </tr>\n</tbody>\n</table>')
})

assert('kable() doesn\'t escape HTML special characters when escape = FALSE', {
  (kable2(data.frame(x = c('10<>', '5&2'), y = c('3>8', '"40"')), 'html', escape = FALSE) %==%
     '<table>\n <thead>\n  <tr>\n   <th style="text-align:left;"> x </th>\n   <th style="text-align:left;"> y </th>\n  </tr>\n </thead>\n<tbody>\n  <tr>\n   <td style="text-align:left;"> 10<> </td>\n   <td style="text-align:left;"> 3>8 </td>\n  </tr>\n  <tr>\n   <td style="text-align:left;"> 5&2 </td>\n   <td style="text-align:left;"> "40" </td>\n  </tr>\n</tbody>\n</table>')
})

assert('kable(digits = vector) works on numeric matrices', {
  (kable2(matrix(c(1.1, 1.2, 2.3, 2.4), 2, dimnames = list(NULL, c('a', 'b'))), digits = c(0, 1)) %==%
     c('|  a|   b|', '|--:|---:|', '|  1| 2.3|', '|  1| 2.4|'))
})

assert('kable() works on matrices with duplicate row names', {
  (kable2(matrix(c(1, 1, 1, 1), ncol = 2, dimnames = list(c('A', 'A'), c('B', 'B')))) %==%
     c('|   |  B|  B|', '|:--|--:|--:|', '|A  |  1|  1|', '|A  |  1|  1|'))
})

assert('kable() works on matrices with NA colname', {
  (kable2(matrix(c(1, 1, 1, 1), ncol = 2, dimnames = list(c('A', NA), c('B', NA)))) %==%
     c('|   |  B| NA|', '|:--|--:|--:|', '|A  |  1|  1|', '|NA |  1|  1|'))
})

# edge cases (should not error)
x1 = matrix(NA, 0, 0)
x2 = matrix(NA, 0, 1)
x3 = matrix(NA, 1, 0)
for (f in c('simple', 'html', 'latex', 'rst', 'jira')) {
  kable(x1, f)
  kable(x2, f)
  kable(x3, f)
}

colnames(x2) = 'a'
assert('kable(, "pipe") works for a 0 row 1 column matrix', {
  (kable2(x2, 'pipe') %==% c('|a  |', '|:--|'))
})

assert('kable(, "pipe") works for a 1-column matrix without column names', {
  (kable2(matrix(1), 'pipe') %==% c('|   |', '|--:|', '|  1|'))
})

assert('kable(, "simple") generates a pipe table for a 0 row data.frame', {
  (kable2(data.frame(x = character(0), y = integer(0)), 'simple') %==%
     c('|x  |  y|', '|:--|--:|'))
})

assert('kable(, "simple") generates a simple table for a 1-column object', {
  # if the object has column names, indent the table by one space
  (kable2(data.frame(x = 1), 'simple') %==% c('   x', ' ---', '   1'))
  # if it doesn't have column names, generate the usual table (without indent)
  (kable2(matrix(1), 'simple') %==% c('---', '  1', '---'))
})

assert('kable(, "simple", caption = "Table Caption") works for a 1-column matrix', {
  x4 = matrix(1:2, ncol = 1, dimnames = list(NULL, 'a'))
  (kable2(x4, 'simple', caption = 'Table Caption') %==%
    c('Table: Table Caption', '', '   a', ' ---', '   1', '   2'))
})

assert('kable() works on an irregular matrix', {
  (kable2(matrix(list('a', 2, 3, 4), nrow = 2), col.names = c('a', 'b')) %==%
     c('|a  |b  |', '|:--|:--|', '|a  |3  |', '|2  |4  |'))
})

assert('has_rownames() works', {
  (!has_rownames(matrix(1:4, 2)))
  (!has_rownames(iris))
  (has_rownames(mtcars))
  (!has_rownames(as.data.frame(matrix(nrow = 0, ncol = 3))))
})

op = options(knitr.kable.NA = '')
assert('kable() can display NA as empty strings', {
  (kable2(matrix(c(1, NA, 3, 4), nrow = 2), col.names = c('a', 'b')) %==%
     c('|  a|  b|', '|--:|--:|', '|  1|  3|', '|   |  4|'))
})
options(op)

assert('kable() can apply formatting to custom objects', {
  d = tibble::tibble(x = structure(
    'print_me', .to_upper = TRUE, class = 'make_upper'
  ))

  format.make_upper = function(x, ...) {
    xout = unclass(x)
    if (isTRUE(attr(x, '.to_upper'))) {
      xout = toupper(xout)
    }
    as.character(xout)
  }
  registerS3method('format', 'make_upper', format.make_upper, environment(format))

  (kable2(d) %==% c('|x        |', '|:--------|', '|PRINT_ME |'))
})
