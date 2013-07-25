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
#' @param ... options passed to \code{\link{knit2html}}
#' @param title title of the output html file
#' @param style path of the css file
#' @return The result is written into a file and the filename is returned
#' @export
#' @examples rocco(system.file('examples','knitr-rocco.Rmd', package='knitr'))
rocco = function(input, ..., title = 'Knitr Rocco', style = NULL) {
  out = knit2html(input, fragment.only = TRUE, ...)
  ## Path adjustment
  x = readLines(out)
  x = paste(x, collapse = '\n')
  m = gregexpr('<pre><code class="[[:alnum:]]+">(.|\n)*?</code></pre>', x)
  if(m[[1]][1] == -1) stop('No code blocks in HTML output')
  code = regmatches(x, m)[[1]]
  code = paste('<td class="code">', c(code, ''), '</td></tr>', sep = '')
  doc = regmatches(x, m, invert = TRUE)[[1]]
  doc = paste(
    '<tr id="section', seq_len(length(doc)), '"><td class="docs">',
    '<div class="pilwrap"><a class="pilcrow" href="#section',
    seq_len(length(doc)), '">&para</a></div>',
    unlist(lapply(doc, FUN=docAdjust)), '</td>', sep = ''
  )
  y = paste(doc, code, sep = '', collapse = '')
  html = readLines(system.file('misc', 'docco-classic.html', package='knitr'))
  html = sub('<!-- title -->', title, html, fixed=TRUE)
  html = sub('<!-- knitr_rocco -->', y, html, fixed=TRUE)
  if(is.null(style))
    style = system.file('themes','rocco.css',package='knitr')
  html = sub('<!-- knitr_rocco_style -->', style, html, fixed=TRUE)
  writeLines(html, con = out)
}
