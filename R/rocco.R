# Pick up the last 'paragraph' of the 'document section' to align with the
# following 'code section'
docAdjust = function(x) {
  lastp = gregexpr('\n{2,}([^\n]+\n?[^\n]+)+\n{2,}$', x)[[1]]
  # If there is only one paragraph in the 'document section', just return it
  # Otherwise, insert some html tags before the last paragraph to make it align
  # with the following code chunk
  if (lastp[1] == 1) return(x)
  sub('(\n{2,}([^\n]+\n?[^\n]+)+\n{2,}$)',
      '\n</td><td class="code"></td></tr><tr><td class="docs">\\1\n', x)
}
#' Knit R Markdown using the classic Docco style
#'
#' The classic Docco style is a two-column layout, with text in the left and
#' code in the right column.
#' @param input path of the input R Markdown file
#' @param ... arguments to be passed to \code{\link{knit2html}}
#' @return An HTML file is written, and its name is returned.
#' @references The Docco package by Jeremy Ashkenas:
#'   \url{https://github.com/jashkenas/docco}
#' @export
#' @examples # TODO: need a better example
rocco = function(input, ...) {
  out = knit2html(
    input, ...,
    stylesheet = system.file('misc', 'docco-classic.css', package = 'knitr'),
    template = system.file('misc', 'docco-classic.html', package = 'knitr')
  )
  txt = readLines(out)
  i1 = which(txt == '      <table><!--table start-->')[1]
  i2 = which(txt == '      </table><!--table end-->')[1]
  x = paste(txt[seq(i1 + 1, i2 - 1)], collapse = '\n')
  m = gregexpr('<pre><code( class="[[:alnum:]]+")?>(.|\n)*?</code></pre>', x)
  if(m[[1]][1] == -1) stop('No code blocks in HTML output')
  code = regmatches(x, m)[[1]]
  code = paste('<td class="code">', c(code, ''), '</td></tr>', sep = '')
  doc = regmatches(x, m, invert = TRUE)[[1]]
  i = seq_len(length(doc))
  doc = paste(
    '<tr id="section', i, '"><td class="docs">',
    '<div class="pilwrap"><a class="pilcrow" href="#section', i, '">&para</a></div>',
    sapply(doc, docAdjust), '</td>', sep = ''
  )
  html = c(txt[1:i1], paste(doc, code, sep = '', collapse = ''), txt[i2:length(txt)])
  writeLines(html, out)
  invisible(out)
}
