# record input/output lines numbers in Rnw/tex, filenames, line number of child
# in parent, current block number (i), line number of next block after a child
knit_concord = new_defaults(
  list(inlines = NULL, outlines = NULL, infile = NULL, outfile = NULL,
       parent = NULL, parent.line = NULL, i = NULL, in.next = 1L, out.next = NULL)
)

concord_mode = function() opts_knit$get('concordance')

current_lines = function(i = knit_concord$get('i')) {
  # a helpr function to return line numbers for block i
  n = knit_concord$get('inlines')
  n0 = sum(head(n, i - 1L)) + 1L; n1 = n0 + n[i] - 1L
  c(n0, n1)
}

## generate concordance for RStudio
concord_gen = function(infile = knit_concord$get('infile'),
                       outfile = knit_concord$get('outfile')) {
  if (!concord_mode()) return()
  i = knit_concord$get('inlines'); o = knit_concord$get('outlines')
  if (is.null(i) || is.null(o)) {
    warning('cannot generate concordance due to incomplete line numbers')
    return()
  }

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
  vals = c(steps[1L], as.numeric(rbind(vals$lengths, vals$values)))
  concordance = paste(strwrap(paste(vals, collapse = " ")), collapse = " %\n")
  
  # build record
  parent = knit_concord$get('parent'); parent.line = knit_concord$get('parent.line')
  extra = if (is.null(parent) || is.null(parent.line)) '' else {
    str_c(parent, ':', parent.line, ':')
  }
  output = str_c("\\Sconcordance{concordance:", outfile, ":",
                  infile, ":", extra, "%\n", concordance,"}\n")
  # write to file
  confile = str_c(file_path_sans_ext(outfile), '-concordance.tex')
  cat(output, file = confile)
}
