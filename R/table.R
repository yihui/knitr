#' Create tables in LaTeX, HTML, Markdown and reStructuredText
#'
#' A very simple table generator, and it is simple by design. It is not intended
#' to replace any other R packages for making tables. The \code{kable()}
#' function returns a single table for a single data object, and returns a table
#' that contains multiple tables if the input object is a list of data objects.
#' The \code{kables()} function is similar to \code{kable(x)} when \code{x} is a
#' list of data objects, but \code{kables()} accepts a list of \code{kable()}
#' values directly instead of data objects (see examples below).
#'
#' Missing values (\code{NA}) in the table are displayed as \code{NA} by
#' default. If you want to display them with other characters, you can set the
#' option \code{knitr.kable.NA}, e.g. \code{options(knitr.kable.NA = '')} to
#' hide \code{NA} values.
#' @param x For \code{kable()}, \code{x} is an R object, which is typically a
#'   matrix or data frame. For \code{kables()}, a list with each element being a
#'   returned value from \code{kable()}.
#' @param format A character string. Possible values are \code{latex},
#'   \code{html}, \code{pipe} (Pandoc's pipe tables), \code{simple} (Pandoc's
#'   simple tables), \code{rst}, \code{jira}, and \code{org} (Emacs Org-mode).
#'   The value of this argument will be automatically determined if the function
#'   is called within a \pkg{knitr} document. The \code{format} value can also
#'   be set in the global option \code{knitr.table.format}. If \code{format} is
#'   a function, it must return a character string.
#' @param digits Maximum number of digits for numeric columns, passed to
#'   \code{round()}. This can also be a vector of length \code{ncol(x)}, to set
#'   the number of digits for individual columns.
#' @param row.names Logical: whether to include row names. By default, row names
#'   are included if \code{rownames(x)} is neither \code{NULL} nor identical to
#'   \code{1:nrow(x)}.
#' @param col.names A character vector of column names to be used in the table.
#' @param align Column alignment: a character vector consisting of \code{'l'}
#'   (left), \code{'c'} (center) and/or \code{'r'} (right). By default or if
#'   \code{align = NULL}, numeric columns are right-aligned, and other columns
#'   are left-aligned. If \code{length(align) == 1L}, the string will be
#'   expanded to a vector of individual letters, e.g. \code{'clc'} becomes
#'   \code{c('c', 'l', 'c')}, unless the output format is LaTeX.
#' @param caption The table caption. By default, it is retrieved from the chunk
#'   option \code{tab.cap}.
#' @param label The table reference label. By default, the label is obtained
#'   from \code{knitr::\link{opts_current}$get('label')} (i.e., the current
#'   chunk label). To disable the label, use \code{label = NA}.
#' @param format.args A list of arguments to be passed to \code{\link{format}()}
#'   to format table values, e.g. \code{list(big.mark = ',')}.
#' @param escape Boolean; whether to escape special characters when producing
#'   HTML or LaTeX tables. When \code{escape = FALSE}, you have to make sure
#'   that special characters will not trigger syntax errors in LaTeX or HTML.
#' @param ... Other arguments (see Examples and References).
#' @return A character vector of the table source code.
#' @seealso Other R packages such as \pkg{huxtable}, \pkg{xtable},
#'   \pkg{kableExtra}, \pkg{gt} and \pkg{tables} for HTML and LaTeX tables, and
#'   \pkg{ascii} and \pkg{pander} for different flavors of markdown output and
#'   some advanced features and table styles. For more on other packages for
#'   creating tables, see
#'   \url{https://bookdown.org/yihui/rmarkdown-cookbook/table-other.html}.
#' @note When using \code{kable()} as a \emph{top-level} expression, you do not
#'   need to explicitly \code{print()} it due to R's automatic implicit
#'   printing. When it is wrapped inside other expressions (such as a
#'   \code{\link{for}} loop), you must explicitly \code{print(kable(...))}.
#' @references See
#'   \url{https://bookdown.org/yihui/rmarkdown-cookbook/kable.html} for some
#'   examples about this function, including specific arguments according to the
#'   \code{format} selected.
#' @export
#' @examples d1 = head(iris); d2 = head(mtcars)
#' # pipe tables by default
#' kable(d1)
#' kable(d2[, 1:5])
#' # no inner padding
#' kable(d2, format = 'pipe', padding = 0)
#' # more padding
#' kable(d2, format = 'pipe', padding = 2)
#' kable(d1, format = 'latex')
#' kable(d1, format = 'html')
#' kable(d1, format = 'latex', caption = 'Title of the table')
#' kable(d1, format = 'html', caption = 'Title of the table')
#' # use the booktabs package
#' kable(mtcars, format = 'latex', booktabs = TRUE)
#' # use the longtable package
#' kable(matrix(1000, ncol=5), format = 'latex', digits = 2, longtable = TRUE)
#' # change LaTeX default table environment
#' kable(d1, format = "latex", caption = "My table", table.envir='table*')
#' # add some table attributes
#' kable(d1, format = 'html', table.attr = 'id="mytable"')
#' # reST output
#' kable(d2, format = 'rst')
#' # no row names
#' kable(d2, format = 'rst', row.names = FALSE)
#' # Pandoc simple tables
#' kable(d2, format = 'simple', caption = 'Title of the table')
#' # format numbers using , as decimal point, and ' as thousands separator
#' x = as.data.frame(matrix(rnorm(60, 1e6, 1e4), 10))
#' kable(x, format.args = list(decimal.mark = ',', big.mark = "'"))
#' # save the value
#' x = kable(d2, format = 'html')
#' cat(x, sep = '\n')
#' # can also set options(knitr.table.format = 'html') so that the output is HTML
#'
#' # multiple tables via either kable(list(x1, x2)) or kables(list(kable(x1), kable(x2)))
#' kable(list(d1, d2), caption = 'A tale of two tables')
#' kables(list(kable(d1, align = 'l'), kable(d2)), caption = 'A tale of two tables')
kable = function(
  x, format, digits = getOption('digits'), row.names = NA, col.names = NA,
  align, caption = opts_current$get('tab.cap'), label = NULL, format.args = list(),
  escape = TRUE, ...
) {

  format = kable_format(format)
  # expand align if applicable
  if (!missing(align) && length(align) == 1L && !grepl('[^lcr]', align))
    align = strsplit(align, '')[[1]]

  if (inherits(x, 'list')) {
    format = kable_format_latex(format)
    res = lapply(
      x, kable, format = format, digits = digits, row.names = row.names,
      col.names = col.names, align = align, caption = NA,
      format.args = format.args, escape = escape, ...
    )
    return(kables(res, format, caption, label))
  }

  caption = kable_caption(label, caption, format)

  if (!is.matrix(x)) x = as.data.frame(x)
  if (identical(col.names, NA)) col.names = colnames(x)
  m = ncol(x)
  # numeric columns
  isn = if (is.matrix(x)) rep(is.numeric(x), m) else sapply(x, is.numeric)
  if (missing(align) || (format == 'latex' && is.null(align)))
    align = ifelse(isn, 'r', 'l')
  # rounding
  digits = rep(digits, length.out = m)
  for (j in seq_len(m)) {
    if (is_numeric(x[, j])) x[, j] = round(x[, j], digits[j])
  }
  if (any(isn)) {
    if (is.matrix(x)) {
      if (is.table(x) && length(dim(x)) == 2) class(x) = 'matrix'
      x = format_matrix(x, format.args)
    } else x[, isn] = format_args(x[, isn], format.args)
  }
  if (is.na(row.names)) row.names = has_rownames(x)
  if (!is.null(align)) align = rep(align, length.out = m)
  if (row.names) {
    x = cbind(' ' = rownames(x), x)
    if (!is.null(col.names)) col.names = tail(c(' ', col.names), ncol(x))
    if (!is.null(align)) align = c('l', align)  # left align row names
  }
  n = nrow(x)
  x = replace_na(to_character(x), is.na(x))
  if (!is.matrix(x)) x = matrix(x, nrow = n)
  # trim white spaces except those escaped by \ at the end (#2308)
  x = gsub('^\\s+|(?<!\\\\)\\s+$', '', x, perl = TRUE)
  colnames(x) = col.names
  if (format != 'latex' && length(align) && !all(align %in% c('l', 'r', 'c')))
    stop("'align' must be a character vector of possible values 'l', 'r', and 'c'")
  attr(x, 'align') = align
  # simple tables do not 0-row tables (--- will be treated as an hr line)
  if (format == 'simple' && nrow(x) == 0) format = 'pipe'
  res = do.call(
    paste('kable', format, sep = '_'),
    list(x = x, caption = caption, escape = escape, ...)
  )
  structure(res, format = format, class = 'knitr_kable')
}

kable_caption = function(label, caption, format) {
  # create a label for bookdown if applicable
  if (is.null(label)) label = opts_current$get('label')
  if (is.null(label)) label = NA
  if (!is.null(caption) && !anyNA(caption) && !anyNA(label)) caption = paste0(
    create_label(
      opts_knit$get('label.prefix')[['table']],
      label, latex = (format == 'latex')
    ), caption
  )
  caption
}

# determine the table format
kable_format = function(format = NULL) {
  if (missing(format) || is.null(format)) format = getOption('knitr.table.format')
  if (is.null(format)) format = if (is.null(pandoc_to())) switch(
    out_format() %n% 'markdown',
    latex = 'latex', listings = 'latex', sweave = 'latex',
    html = 'html', markdown = 'pipe', rst = 'rst',
    stop('table format not implemented yet!')
  ) else if (isTRUE(opts_knit$get('kable.force.latex')) && is_latex_output()) {
    # force LaTeX table because Pandoc's longtable may not work well with floats
    # http://tex.stackexchange.com/q/276699/9128
    'latex'
  } else 'pipe'
  if (is.function(format)) format = format()
  # backward compatibility with knitr <= v1.28
  switch(format, pandoc = 'simple', markdown = 'pipe', format)
}

# if the output is for Pandoc and we want multiple tabular in one table, we
# should use the latex format instead, because Pandoc does not support Markdown
# in LaTeX yet https://github.com/jgm/pandoc/issues/2453
kable_format_latex = function(format) {
  if (format == 'pipe' && is_latex_output()) 'latex' else format
}

#' @rdname kable
#' @export
kables = function(x, format, caption = NULL, label = NULL) {
  format = kable_format(format)
  format = kable_format_latex(format)
  caption = kable_caption(label, caption, format)
  # in case `x` contains kable()s, make sure all kable()s use the same default format
  opts = options(knitr.table.format = format); on.exit(options(opts), add = TRUE)
  if (!inherits(x, 'list')) stop("'x' must be a list (of kable() values)")
  res = unlist(lapply(x, one_string))
  res = if (format == 'latex') {
    kable_latex_caption(res, caption)
  } else if (format == 'html' || (format == 'pipe' && is_html_output())) kable_html(
    matrix(paste0('\n\n', res, '\n\n'), 1), caption = caption, escape = FALSE,
    table.attr = 'class="kable_wrapper"'
  ) else {
    res = paste(res, collapse = '\n\n')
    if (format == 'pipe') kable_pandoc_caption(res, caption) else res
  }
  structure(res, format = format, class = 'knitr_kable')
}

# convert to character while preserving dim/dimnames attributes
to_character = function(x) {
  if (is.character(x)) return(x)
  # format columns individually if x is not a matrix
  if (!is.matrix(x)) {
    for (j in seq_len(ncol(x))) x[, j] = format_args(x[, j])
    x = as.matrix(x)
  }
  x2 = as.character(x); dim(x2) = dim(x); dimnames(x2) = dimnames(x)
  x2
}

# as.data.frame() does not allow duplicate row names (#898)
format_matrix = function(x, args) {
  nms = rownames(x)
  rownames(x) = NULL
  x = as.matrix(format_args(as.data.frame(x), args))
  rownames(x) = nms
  x
}

format_args = function(x, args = list()) {
  args$x = x
  args$trim = TRUE
  replace_na(do.call(format, args), is.na(x))
}

replace_na = function(x, which = is.na(x), to = getOption('knitr.kable.NA')) {
  if (is.null(to)) return(x)
  x[which] = to
  x
}

has_rownames = function(x) {
  !is.null(rownames(x)) && !identical(rownames(x), as.character(seq_len(NROW(x))))
}

#' @export
print.knitr_kable = function(x, ...) {
  if (!(attr(x, 'format') %in% c('html', 'latex'))) cat('\n\n')
  cat(x, sep = '\n')
}

#' @export
knit_print.knitr_kable = function(x, ...) {
  x = one_string(c(
    if (!(attr(x, 'format') %in% c('html', 'latex'))) c('', ''), x, '\n'
  ))
  asis_output(x)
}

kable_latex = function(
  x, booktabs = FALSE, longtable = FALSE, tabular = if (longtable) 'longtable' else 'tabular',
  valign = if (tabular %in% c('tabularx', 'xltabular')) '{\\linewidth}' else '[t]',
  position = '', centering = TRUE,
  vline = getOption('knitr.table.vline', if (booktabs) '' else '|'),
  toprule = getOption('knitr.table.toprule', if (booktabs) '\\toprule' else '\\hline'),
  bottomrule = getOption('knitr.table.bottomrule', if (booktabs) '\\bottomrule' else '\\hline'),
  midrule = getOption('knitr.table.midrule', if (booktabs) '\\midrule' else '\\hline'),
  linesep = if (booktabs) c('', '', '', '', '\\addlinespace') else '\\hline',
  caption = NULL, caption.short = '', table.envir = if (!is.null(caption)) 'table',
  escape = TRUE, ...
) {
  if (!is.null(align <- attr(x, 'align'))) {
    align = paste(align, collapse = vline)
    align = paste0('{', align, '}')
  }
  centering = if (centering && !is.null(caption)) '\n\\centering'
  # vertical align only if 'caption' is not NULL (may be NA) or 'valign' has
  # been explicitly specified; tabularx and xltabular always use 'valign'
  valign = if ((!is.null(caption) || !missing(valign) || tabular %in% c('tabularx', 'xltabular')) && valign != '') {
    if (grepl('^[[{]', valign)) valign else sprintf('[%s]', valign)
  } else ''
  if (identical(caption, NA)) caption = NULL
  if (position != '') position = paste0('[', position, ']')
  env1 = sprintf('\\begin{%s}%s\n', table.envir, position)
  env2 = sprintf('\n\\end{%s}',   table.envir)
  if (caption.short != '') caption.short = paste0('[', caption.short, ']')
  cap = if (is.null(caption)) '' else sprintf('\n\\caption%s{%s}', caption.short, caption)

  if (nrow(x) == 0) midrule = ""

  linesep = if (nrow(x) > 1) {
    c(rep(linesep, length.out = nrow(x) - 1), '')
  } else rep('', nrow(x))
  linesep = ifelse(linesep == "", linesep, paste0('\n', linesep))

  x = escape_latex_table(x, escape, booktabs)
  if (!is.character(toprule)) toprule = NULL
  if (!is.character(bottomrule)) bottomrule = NULL

  paste(c(
    if (cap_env <- !tabular %in% c('longtable', 'xltabular')) c(env1, cap, centering),
    sprintf('\n\\begin{%s}%s', tabular, valign), align,
    if (!cap_env && cap != '') c(cap, '\\\\'),
    sprintf('\n%s', toprule), '\n',
    if (!is.null(cn <- colnames(x))) {
      cn = escape_latex_table(cn, escape, booktabs)
      paste0(paste(cn, collapse = ' & '), sprintf('\\\\\n%s\n', midrule))
    },
    one_string(apply(x, 1, paste, collapse = ' & '), sprintf('\\\\%s', linesep), sep = ''),
    sprintf('\n%s', bottomrule),
    sprintf('\n\\end{%s}', tabular),
    if (cap_env) env2
  ), collapse = '')
}

# when using booktabs, add {} before [ so that the content in [] won't be
# treated as parameters of booktabs commands like \midrule:
# https://github.com/yihui/knitr/issues/1595
escape_latex_table = function(x, escape = TRUE, brackets = TRUE) {
  if (escape) x = escape_latex(x)
  if (brackets) x = gsub('^(\\s*)(\\[)', '\\1{}\\2', x)
  x
}

kable_latex_caption = function(x, caption) {
  paste(c(
    '\\begin{table}\n', sprintf('\\caption{%s}\n', caption), x, '\n\\end{table}'
  ), collapse = '')
}

kable_html = function(
  x, table.attr = getOption('knitr.table.html.attr', ''), caption = NULL, escape = TRUE, ...
) {
  table.attr = trimws(table.attr)
  # need a space between <table and attributes
  if (nzchar(table.attr)) table.attr = paste('', table.attr)
  align = if (is.null(align <- attr(x, 'align'))) '' else {
    sprintf(' style="text-align:%s;"', c(l = 'left', c = 'center', r = 'right')[align])
  }
  if (identical(caption, NA)) caption = NULL
  cap = if (length(caption)) sprintf('\n<caption>%s</caption>', caption) else ''
  if (escape) x = escape_html(x)
  one_string(c(
    sprintf('<table%s>%s', table.attr, cap),
    if (!is.null(cn <- colnames(x))) {
      if (escape) cn = escape_html(cn)
      c(' <thead>', '  <tr>', sprintf('   <th%s> %s </th>', align, cn), '  </tr>', ' </thead>')
    },
    '<tbody>',
    paste(
      '  <tr>',
      apply(x, 1, function(z) one_string(sprintf('   <td%s> %s </td>', align, z))),
      '  </tr>', sep = '\n'
    ),
    '</tbody>',
    '</table>'
  ))
}

#' Generate tables for Markdown and reST
#'
#' This function provides the basis for Markdown and reST tables.
#' @param x The data matrix.
#' @param sep.row A length-3 character vector, specifying separators to be
#'   printed before the header, after the header, and at the end of the table
#'   respectively.
#' @param sep.col The column separator.
#' @param sep.head The column separator for the header of the table (i.e., the
#'   line with the column names).
#' @param padding Number of spaces for the table cell padding.
#' @param align.fun A function to process the separator under the header
#'   according to the alignment.
#' @return A character vector of the table content.
#' @noRd
kable_mark = function(x, sep.row = c('=', '=', '='), sep.col = '  ', padding = 0,
                      align.fun = function(s, a) s, rownames.name = '',
                      sep.head = sep.col, newline = NULL, ...) {
  # when the column separator is |, replace existing | with its HTML entity
  if (sep.col == '|') for (j in seq_len(ncol(x))) {
    x[, j] = gsub('\\|', '&#124;', x[, j])
  }
  l = if (prod(dim(x)) > 0) apply(x, 2, function(z) max(nchar(remove_urls(z), type = 'width'), na.rm = TRUE))
  cn = colnames(x)
  if (length(cn) > 0) {
    cn[is.na(cn)] = "NA"
    if (sep.head == '|') cn = gsub('\\|', '&#124;', cn)
    if (grepl('^\\s*$', cn[1L])) cn[1L] = rownames.name  # no empty cells for reST
    l = pmax(if (length(l) == 0) 0 else l, nchar(remove_urls(cn), type = 'width'))
  }
  align = attr(x, 'align')
  padding = padding * if (length(align) == 0) 2 else {
    ifelse(align == 'c', 2, 1)
  }
  l = pmax(l + padding, 3)  # at least of width 3 for Github Markdown
  s = strrep(sep.row[2], l)
  res = rbind(if (!is.na(sep.row[1])) s, cn, align.fun(s, align),
              x, if (!is.na(sep.row[3])) s)
  res = mat_pad(res, l, align)
  res = add_mark_col_sep(res, sep.col, sep.head)
  if (is.character(newline)) res = gsub('\n', newline, res, fixed = TRUE)
  res
}

# add column separators to header and body separately
add_mark_col_sep = function(table, sep.col, sep.head) {
  if (any(dim(table) == 0)) return(table)
  h = paste(table[1, ], collapse = sep.head)  # header
  b = table[-1, , drop = FALSE]
  b = apply(b, 1, paste, collapse = sep.col)  # body
  c(h, b)
}

kable_rst = function(x, rownames.name = '\\', ...) {
  kable_mark(x, rownames.name = rownames.name)
}

# Pandoc's pipe table
kable_pipe = function(x, caption = NULL, padding = 1, caption.label = 'Table:', ...) {
  if (is.null(colnames(x))) colnames(x) = rep('', ncol(x))
  res = kable_mark(x, c(NA, '-', NA), '|', padding, align.fun = function(s, a) {
    if (is.null(a)) return(s)
    r = c(l = '^.', c = '^.|.$', r = '.$')
    for (i in seq_along(s)) {
      s[i] = gsub(r[a[i]], ':', s[i])
    }
    s
  }, ...)
  res = sprintf('|%s|', res)
  kable_pandoc_caption(res, caption, caption.label)
}

# Pandoc's simple table
kable_simple = function(x, caption = NULL, padding = 1, ...) {
  tab = kable_mark(
    x, c(NA, '-', if (is_blank(colnames(x))) '-' else NA),
    padding = padding, ...
  )
  # when x has only one column with name, indent by one space so --- won't be
  # treated as an hr line
  if (ncol(x) == 1 && !is.null(colnames(x))) tab = paste0(' ', tab)
  kable_pandoc_caption(tab, caption)
}

# Jira table
kable_jira = function(x, caption = NULL, padding = 1, ...) {
  tab = kable_mark(x, c(NA, NA, NA), '|', padding, sep.head = '||', ...)
  if ((n <- length(tab)) == 0) return(tab)
  # remove the line that separates the table header from the table body
  if (n >= 2) tab = tab[-2]
  tab[1] = sprintf('||%s||', tab[1])
  tab[-1] = sprintf('|%s|', tab[-1])
  kable_pandoc_caption(tab, caption)
}

# Emacs Org-mode table
kable_org = function(...) {
  res = kable_pipe(..., caption.label = '#+CAPTION:')
  i = grep('^[-:|]+$', res)  # find the line like |--:|---| under header
  if (length(i)) {
    i = i[1]
    res[i] = gsub('(-|:)[|](-|:)', '\\1+\\2', res[i])  # use + as separator
  }
  res
}

kable_pandoc_caption = function(x, caption, label = 'Table:') {
  if (identical(caption, NA)) caption = NULL
  if (length(caption)) c(paste(label, caption), '', x) else x
}

# pad a matrix
mat_pad = function(m, width, align = NULL) {
  n = nrow(m); p = ncol(m)
  res = matrix('', nrow = n, ncol = p)
  if (n * p == 0) return(res)
  stopifnot(p == length(width))
  side = rep('both', p)
  if (!is.null(align)) side = c(l = 'right', c = 'both', r = 'left')[align]
  apply(m, 2, function(x) max(nchar(x, 'width') - nchar(x, 'chars')))
  matrix(pad_width(c(m), rep(width, each = n), rep(side, each = n)), ncol = p)
}

# pad a character vector to width (instead of number of chars), considering the
# case of width > chars (e.g. CJK chars)
pad_width = function(x, width, side) {
  if (!all(side %in% c('left', 'right', 'both')))
    stop("'side' must be 'left', 'right', or 'both'")
  w = width - nchar(x, 'width')
  w1 = floor(w / 2)  # the left half of spaces when side = 'both'
  s1 = strrep(' ', pmax(0, w * (side == 'left') + w1 * (side == 'both')))
  s2 = strrep(' ', pmax(0, w * (side == 'right') + (w - w1) * (side == 'both')))
  paste0(s1, x, s2)
}
