Snapshot tests for `kable()`. Each block prints the rendered table (or a
logical check) and the following block records the expected output.

kable() works on data frames/matrices of one row.

```{r}
kable(data.frame(x = 1, y = 1), format = 'pipe')
```
```


|  x|  y|
|--:|--:|
|  1|  1|
```

kable() does not discard row names when there is only one row.

```{r}
m = matrix(1:2, nrow = 1, dimnames = list('a', c('x', 'y')))
kable(m)
```
```


|   |  x|  y|
|:--|--:|--:|
|a  |  1|  2|
```

kable() can assign a column name for row names.

```{r}
kable(m, col.names = c('z', colnames(m)))
```
```


|z  |  x|  y|
|:--|--:|--:|
|a  |  1|  2|
```

kable() recycles the align argument correctly.

```{r}
kable(m, align = 'c')
```
```


|   | x | y |
|:--|:-:|:-:|
|a  | 1 | 2 |
```

kable() align with strings correctly (align = c('c', 'r') matches align = 'cr').

```{r}
identical(as.character(kable(m, align = c('c', 'r'))), as.character(kable(m, align = 'cr')))
```
```
[1] TRUE
```

kable() works on character data frames.

```{r}
kable(data.frame(x = 'a'))
```
```


|x  |
|:--|
|a  |
```

kable() works on NA's.

```{r}
kable(data.frame(x = c(NA, FALSE)))
```
```


|x     |
|:-----|
|NA    |
|FALSE |
```

kable() works with the jira format.

```{r}
kable(m, 'jira')
```
```


||   ||  x||  y||
|a  |  1|  2|
```

kable() works with the org format.

```{r}
kable(m, 'org')
m2 = m; colnames(m2) = NULL  # no column names
kable(m2, 'org')
# the case of only one column
kable(m[, 1, drop = FALSE], 'org', row.names = FALSE)
```
```


|   |  x|  y|
|<l>|<r>|<r>|
|---+---+---|
|a  |  1|  2|


|<l>|<r>|<r>|
|a  |  1|  2|


|  x|
|<r>|
|---|
|  1|
```

kable() does not add extra spaces to character columns.

```{r}
kable(data.frame(x = c(1.2, 4.87), y = c('fooooo', 'bar')), 'latex')
```
```

\begin{tabular}{r|l}
\hline
x & y\\
\hline
1.20 & fooooo\\
\hline
4.87 & bar\\
\hline
\end{tabular}
```

kable() in LaTeX mode formats minus signs, infinities, scientific notation, and decimal and thousands separators correctly.

```{r}
kable(data.frame(w = c(1111e10, -0.5e-5), x = c(-1111, -Inf), y = c(1111, -0.5), z = c('text,comma', '-0.5')), 'latex',
      format.args = list(big.mark = ','), numeric.math = TRUE)
# zero in a sci-notation column renders as \(0\), not \(0\times 10^{0}\)
kable(data.frame(x = c(0, 1e-5)), 'latex', numeric.math = TRUE)
```
```

\begin{tabular}{r|r|r|l}
\hline
w & x & y & z\\
\hline
\(1.111\times 10^{13}\) & \(-1{,}111\) & \(1{,}111.0\) & text,comma\\
\hline
\(-5.000\times 10^{-6}\) & \(-\infty\) & \(-0.5\) & -0.5\\
\hline
\end{tabular}

\begin{tabular}{r}
\hline
x\\
\hline
\(0\)\\
\hline
\(10^{-5}\)\\
\hline
\end{tabular}
```

kable() escapes LaTeX special characters by default.

```{r}
kable(data.frame(x = c('10%', '5%'), col_name = c('3_8', '40_6')), 'latex')
```
```

\begin{tabular}{l|l}
\hline
x & col\_name\\
\hline
10\% & 3\_8\\
\hline
5\% & 40\_6\\
\hline
\end{tabular}
```

kable() doesn't escape LaTeX special characters when escape = FALSE.

```{r}
kable(data.frame(x = c('10%', '5%'), col_name = c('3_8', '40_6')), 'latex', escape = FALSE)
```
```

\begin{tabular}{l|l}
\hline
x & col_name\\
\hline
10% & 3_8\\
\hline
5% & 40_6\\
\hline
\end{tabular}
```

kable() escapes special characters in the caption (#2436).

```{r}
kable(data.frame(x = 1), 'latex', caption = 'S&P rating')
grepl('<caption>A &amp; B</caption>', as.character(kable(data.frame(x = 1), 'html', caption = 'A & B')))
```
```
\begin{table}

\caption{S\&P rating}
\centering
\begin{tabular}[t]{r}
\hline
x\\
\hline
1\\
\hline
\end{tabular}
\end{table}
[1] TRUE
```

kable() does not escape the caption when escape = FALSE (#2436).

```{r}
grepl('\\caption{A & B}', as.character(kable(
  data.frame(x = 1), 'latex', caption = 'A & B', escape = FALSE
)), fixed = TRUE)
```
```
[1] TRUE
```

kable() adds {} before [] when booktabs = TRUE.

```{r}
kable(data.frame(x = c('[0, 1]', '(1, 2]'), y = c(35, 62)), 'latex', booktabs = TRUE)
```
```

\begin{tabular}{lr}
\toprule
x & y\\
\midrule
{}[0, 1] & 35\\
(1, 2] & 62\\
\bottomrule
\end{tabular}
```

kable(format = "latex", linesep = ...) works.

```{r}
kable(data.frame(x = 1:4), 'latex', linesep  = c('', '', '\\midrule'))
```
```

\begin{tabular}{r}
\hline
x\\
\hline
1\\
2\\
3\\
\midrule
4\\
\hline
\end{tabular}
```

kable() does not trim escaped spaces at the end.

```{r}
kable(data.frame(x = '\\ '), 'latex', escape = FALSE)
```
```

\begin{tabular}{l}
\hline
x\\
\hline
\ \\
\hline
\end{tabular}
```

kable() escapes HTML special characters by default.

```{r}
kable(data.frame(x = c('10<>', '5&2'), y = c('3>8', '"40"')), 'html')
```
```
<table>
 <thead>
  <tr>
   <th style="text-align:left;"> x </th>
   <th style="text-align:left;"> y </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 10&lt;&gt; </td>
   <td style="text-align:left;"> 3&gt;8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 5&amp;2 </td>
   <td style="text-align:left;"> "40" </td>
  </tr>
</tbody>
</table>
```

kable() doesn't escape HTML special characters when escape = FALSE.

```{r}
kable(data.frame(x = c('10<>', '5&2'), y = c('3>8', '"40"')), 'html', escape = FALSE)
```
```
<table>
 <thead>
  <tr>
   <th style="text-align:left;"> x </th>
   <th style="text-align:left;"> y </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 10<> </td>
   <td style="text-align:left;"> 3>8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 5&2 </td>
   <td style="text-align:left;"> "40" </td>
  </tr>
</tbody>
</table>
```

kable(digits = vector) works on numeric matrices.

```{r}
kable(matrix(c(1.1, 1.2, 2.3, 2.4), 2, dimnames = list(NULL, c('a', 'b'))), digits = c(0, 1))
```
```


|  a|   b|
|--:|---:|
|  1| 2.3|
|  1| 2.4|
```

kable() works on matrices with duplicate row names.

```{r}
kable(matrix(c(1, 1, 1, 1), ncol = 2, dimnames = list(c('A', 'A'), c('B', 'B'))))
```
```


|   |  B|  B|
|:--|--:|--:|
|A  |  1|  1|
|A  |  1|  1|
```

kable() works on matrices with NA colname.

```{r}
kable(matrix(c(1, 1, 1, 1), ncol = 2, dimnames = list(c('A', NA), c('B', NA))))
```
```


|   |  B| NA|
|:--|--:|--:|
|A  |  1|  1|
|NA |  1|  1|
```

kable() does not error on edge-case empty matrices.

```{r}
x1 = matrix(NA, 0, 0)
x2 = matrix(NA, 0, 1)
x3 = matrix(NA, 1, 0)
for (f in c('simple', 'html', 'latex', 'rst', 'jira', 'org')) {
  kable(x1, f)
  kable(x2, f)
  kable(x3, f)
}
```
```
```

kable(, "pipe") works for a 0 row 1 column matrix.

```{r}
colnames(x2) = 'a'
kable(x2, 'pipe')
```
```


|a  |
|:--|
```

kable(, "pipe") works for a 1-column matrix without column names.

```{r}
kable(matrix(1), 'pipe')
```
```


|   |
|--:|
|  1|
```

kable(, "simple") generates a pipe table for a 0 row data.frame.

```{r}
kable(data.frame(x = character(0), y = integer(0)), 'simple')
```
```


|x  |  y|
|:--|--:|
```

kable(, "simple") generates a simple table for a 1-column object.

```{r}
# if the object has column names, indent the table by one space
kable(data.frame(x = 1), 'simple')
# if it doesn't have column names, generate the usual table (without indent)
kable(matrix(1), 'simple')
```
```


   x
 ---
   1


---
  1
---
```

kable(, "simple", caption = "Table Caption") works for a 1-column matrix.

```{r}
x4 = matrix(1:2, ncol = 1, dimnames = list(NULL, 'a'))
kable(x4, 'simple', caption = 'Table Caption')
```
```


Table: Table Caption

   a
 ---
   1
   2
```

kable() works on an irregular matrix.

```{r}
kable(matrix(list('a', 2, 3, 4), nrow = 2), col.names = c('a', 'b'))
```
```


|a  |b  |
|:--|:--|
|a  |3  |
|2  |4  |
```

has_rownames() works.

```{r}
has_rownames(matrix(1:4, 2))
has_rownames(iris)
has_rownames(mtcars)
has_rownames(as.data.frame(matrix(nrow = 0, ncol = 3)))
```
```
[1] FALSE
[1] FALSE
[1] TRUE
[1] FALSE
```

kable() can display NA as empty strings.

```{r}
op = options(knitr.kable.NA = '')
kable(matrix(c(1, NA, 3, 4), nrow = 2), col.names = c('a', 'b'))
options(op)
```
```


|  a|  b|
|--:|--:|
|  1|  3|
|   |  4|
```

kable() can apply formatting to custom objects.

```{r}
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

kable(d)
```
```


|x        |
|:--------|
|PRINT_ME |
```
