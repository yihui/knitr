## currently we do not consider concordance for child documents
concord_mode = function() {
  opts_knit$get('concordance') && !child_mode()
}

## record line numbers in input and output
concord_input = function(n) {
  if (!child_mode()) .knitEnv$input = n
}
concord_output = function(n) {
  if (concord_mode()) .knitEnv$output = n
}

## generate concordance for RStudio
concord_gen = function(infile, outfile) {
  if (!concord_mode()) return()
  i = .knitEnv$input; o = .knitEnv$output
  if (is.null(i) || is.null(o)) {
    warning('cannot generate concordance due to incomplete line numbers')
    return()
  }
  .knitEnv$input = .knitEnv$output = NULL

  steps = NULL # how many steps to jump forward to match output line numbers
  for (k in seq_along(i)) {
    steps = c(steps, if (o[k] >= i[k]) {
      rep(c(1L, 0L), c(i[k], o[k] - i[k]))
    } else {
      c(rep(1L, o[k] - 1L), i[k] - o[k] + 1L)
    })
  }
  
  # generate data structure
  linesout = cumsum(steps)
  vals = rle(diff(linesout))
  vals = c(linesout[1L], as.numeric(rbind(vals$lengths, vals$values)))
  concordance = paste(strwrap(paste(vals, collapse = " ")), collapse = " %\n")
  
  # build record
  output = str_c("\\Sconcordance{concordance:", outfile, ":",
                  infile, ":", "%\n", concordance,"}\n")
  
  # write to file
  confile = str_c(file_path_sans_ext(outfile), '-concordance.tex')
  cat(output, file = confile)
}
