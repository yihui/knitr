# move paragraphs of images one row above, and split the last paragraph from the
# previous ones
docAdjust = function(x) {
  if ((n <- length(x)) < 2) return(x)
  m = gregexpr('^\\s*<p>(<img src="data:[^>]+/>\\s*)+</p>\\s*', x)
  restart = '</td><td class="code"></td></tr><tr><td class="docs">'
  for (i in 2:n) if (m[[i]] > 0) {
    img = regmatches(x[i], m[[i]])
    txt = unlist(regmatches(x[i], m[[i]], invert = TRUE))
    if (grepl('^\\s*$', x[i - 1])) {
      x[i - 1] = img
    } else {
      x[i - 1] = paste(x[i - 1], restart, img, sep = '\n')
    }
    x[i] = paste(txt, collapse = '')
  }
  # split a doc cell if it has mutiple paragraphs, so that the code cell on the
  # right can match with its last paragraph
  x[2:n - 1] = unlist(lapply(strsplit(x[2:n - 1], '\n{2,}'), function(z) {
    n = length(z)
    if (n <= 1) return(z)
    if (length(idx <- grep('^\\s*$', z, invert = TRUE)) > 1) {
      i = max(idx)
      z[i] = paste0(restart, z[i])
    }
    paste(z, collapse = '\n\n')
  }))
  # might have produced some empty cells, so remove them
  gsub('<td class="code">\\s*</td></tr><tr><td class="docs">\\s*</td>', '', x)
}
#' Knit R Markdown using the classic Docco style
#'
#' The classic Docco style is a two-column layout, with text in the left and
#' code in the right column.
#'
#' The output HTML page supports resizing and hiding/showing the two columns.
#' Move the cursor to the center of the page, and it will change to a
#' bidirectional resize cursor; drag the cursor to resize the two columns. Press
#' the key \code{t} to hide the code column (show the text column only), and
#' press again to hide the text column (show code).
#' @param input Path of the input R Markdown file.
#' @param ... Arguments to be passed to \code{\link{knit2html}}
#' @return An HTML file is written, and its name is returned.
#' @author Weicheng Zhu and Yihui Xie
#' @references The Docco package by Jeremy Ashkenas:
#'   \url{https://github.com/jashkenas/docco}
#' @export
#' @examples rocco_view=function(input) {if (!file.exists(input)) return()
#' o=rocco(input, header='', quiet=TRUE)
#' if (interactive()) browseURL(o)}
#' # knit these two vignettes using the docco style
#' rocco_view(system.file('doc', 'docco-classic.Rmd', package = 'knitr'))
#' rocco_view(system.file('doc', 'knit_expand.Rmd', package = 'knitr'))
rocco = function(input, ...) {
  out = knit2html(
    input, ...,
    stylesheet = system.file('misc', 'docco-classic.css', package = 'knitr'),
    template = system.file('misc', 'docco-classic.html', package = 'knitr')
  )
  txt = read_utf8(out)
  i1 = min(grep('<!--table start-->$', txt))
  i2 = max(grep('<!--table end-->$', txt))
  x = one_string(txt[seq(i1 + 1, i2 - 1)])
  x = gsub('</pre>\\s*<pre>', '<!--ReDuNdAnTpRe-->', x)  # merge pre blocks
  m = gregexpr('<pre><code( class="[[:alnum:]]+")?>(.|\n)*?</code></pre>', x)
  if (m[[1]][1] == -1) stop('No code blocks in HTML output')

  code = regmatches(x, m)[[1]]
  code = gsub('<!--ReDuNdAnTpRe-->', '</pre>\n<pre>', code) # restore pre blocks
  code = paste0('<td class="code">', c(code, ''), '</td></tr>')
  doc = regmatches(x, m, invert = TRUE)[[1]]
  doc = paste0('<tr><td class="docs">', docAdjust(doc), '</td>')

  # write pilcrow anchors to rows
  sec = 1
  for (i in seq_along(doc)) {
    while (grepl('<tr><td class="docs">', doc[i])) {
      doc[i] = sub('<tr><td class="docs">', paste0(
        '<tr id="row', sec, '"><td class="docs">', '<div class="pilwrap">',
        '<a class="pilcrow" href="#row', sec, '">&para;</a></div>'
      ),  doc[i])
      sec = sec + 1
    }
  }

  html = c(txt[1:i1], paste0(doc, code, collapse = ''), txt[i2:length(txt)])
  writeLines(html, out)
  invisible(out)
}
