## stolen from Romain's highlight package (v0.3.2)

# styler assistant for latex
styler_assistant_latex = function(x) {

  styles = sapply(x, function(item) {
    settings = names(item)
    has = function(s, value) {
      s %in% settings && grepl(value, item[[s]])
    }
    start = end = ''
    if ('color' %in% settings) {
      start = str_c(start, '\\textcolor[rgb]{', col2latexrgb(item[['color']]), '}{')
      end = str_c(end, '}')
    }
    if (has('font-weight', 'bold')) {
      start = str_c(start, '\\textbf{')
      end = str_c('}', end)
    }
    if (has('font-style', 'italic')) {
      start = str_c(start, '\\textit{')
      end = str_c('}', end)
    }
    if (has('text-decoration', 'underline')) {
      start = str_c(start, '\\underline{')
      end = str_c('}', end)
    }
    sprintf('%s#1%s', start, end)
  })
  sprintf('\\newcommand{\\hl%s}[1]{%s}%%', names(x), styles)
}

col2latexrgb = function(hex) {
  col = col2rgb(hex)[, 1]/255
  paste(col, collapse = ',')
}
