#' @include defaults.R

# record input/output lines numbers in Rnw/tex and filenames
knit_concord = new_defaults(list(
  inlines = NULL, outlines = NULL, infile = NULL, outfile = NULL
))

# do not consider child mode for concordance
concord_mode = function() {
  opts_knit$get('concordance') && !child_mode() &&
    (out_format(c('latex', 'sweave', 'listings')) ||
     out_format('markdown') && is_html_output())
}

current_lines = function(i) {
  # a helpr function to return line numbers for block i
  n = knit_concord$get('inlines')
  n1 = sum(head(n, i)); n0 = n1 - n[i] + 2
  c(min(n0, n1), n1)
}

# generate concordance for RStudio
concord_gen = function(infile, outfile) {
  if (!concord_mode()) return()
  i = knit_concord$get('inlines'); o = knit_concord$get('outlines')
  if (is.null(i) || is.null(o)) {
    warning('cannot generate concordance due to incomplete line numbers')
    return()
  }
  stopifnot(length(i) == length(o))

  steps = NULL # how many steps to jump forward to match output line numbers
  for (k in seq_along(i)) {
    steps = c(steps, if (o[k] >= i[k]) {
      rep(c(1L, 0L), c(i[k], o[k] - i[k]))
    } else {
      c(rep(1L, o[k] - 1L), i[k] - o[k] + 1L)
    })
  }

  # generate data structure
  vals = rle(steps)
  vals = c(1L, as.numeric(rbind(vals$lengths, vals$values)))
  concordance = paste(strwrap(paste(vals, collapse = ' ')), collapse = ' %\n')

  confile = paste(sans_ext(outfile), 'concordance.tex', sep = '-')
  # write to file
  if (out_format('markdown') && is_html_output())
    cat('<!-- concordance:', outfile, ':', infile, ':',
      concordance, ' -->\n', sep = '', file = outfile, append = TRUE)
  else
    cat('\\Sconcordance{concordance:', outfile, ':', infile, ':%\n',
      concordance, '}\n', sep = '', file = confile)
}
