#' Convert an R script to a literate programming document
#'
#' This function takes a specially formatted R script and converts it to a
#' literate programming document. Normal text should be written after the
#' roxygen comment (\code{#'}) and code chunk options are written after
#' \code{#+} or \code{#-}.
#' @param input the path to the R script
#' @param knit logical: whether to compile the document after conversion
#' @param format character: the output format (it takes five possible values);
#'   the default is R Markdown
#' @note The function name came from this idiom: make a silk purse out of a
#'   sow's ear (R script).
#' @author Yihui Xie, with the original idea from Richard FitzJohn (who named it
#'   as \code{sowsear()})
#' @return The path of the literate programming document.
#' @export
#' @seealso \code{\link{stitch}} (feed a template with an R script)
#' @examples #' write normal text like this and chunk options like below
#' #+ label, opt=value
#'
#' (s = system.file('examples', 'knitr-silk.R', package = 'knitr'))
#' silk(s)  # default markdown
#' o = silk(s, knit = FALSE) # convert only
#' knit2html(o) # compile to HTML
#'
#' # other formats
#' silk(s, FALSE, format='Rnw')  # you need to write documentclass after #'
#' silk(s, FALSE, format='Rhtml')
#' silk(s, FALSE, format='Rtex')
#' silk(s, FALSE, format='Rrst')
silk = function(input, purse = TRUE, format = c('Rmd', 'Rnw', 'Rhtml', 'Rtex', 'Rrst')) {

  format = match.arg(format)
  x = readLines(input, warn = FALSE); r = rle(str_detect(x, "^#+'"))
  n = length(r$lengths); txt = vector('list', n); idx = c(0L, cumsum(r$lengths))
  p = .fmt.pat[[tolower(format)]]
  p1 = str_replace(str_c('^', p[1L], '.*', p[2L], '$'), '\\{', '\\\\{')

  for (i in seq_len(n)) {
    block = x[seq(idx[i] + 1L, idx[i+1])]
    txt[[i]] = if (r$value[i]) {
      # normal text; just strip #'
      str_replace(block, "^#+'\\s*", '')
    } else {
      # R code; #+/- indicates chunk options
      block = strip_white(block) # rm white lines in beginning and end
      if (!length(block)) next
      if (any(opt <- str_detect(block, '^#+(\\+|-)'))) {
        block[opt] = str_c(p[1L], str_replace(block[opt], '^#+(\\+|-)\\s*', ''), p[2L])
      }
      if (!str_detect(block[1L], p1)) {
        block = c(str_c(p[1L], p[2L]), block)
      }
      c('', block, p[3L], '')
    }
  }

  outsrc = str_c(file_path_sans_ext(input), '.', format)
  cat(unlist(txt), file = outsrc, sep = '\n')
  if (purse) knit(outsrc)

  invisible(outsrc)
}

.fmt.pat = list(
  rmd = c('```{r ', '}', '```'), rnw = c('<<', '>>=', '@'),
  rhtml = c('<!--begin.rcode ', '', 'end.rcode-->'),
  rtex = c('% begin.rcode ', '', '% end.rcode'), rrst = c('.. {r ', '}', '.. ..')
)