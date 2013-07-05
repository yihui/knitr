## my extremely simple 'syntax highlighter' as a substitute for Romain's highlight()
hi.keywords =  paste('(\\W)(', paste(c(
  'if', 'else', 'repeat', 'while', 'function', 'for', 'in', 'next', 'break', 'repeat',
  'LETTERS', 'letters', 'month.abb', 'month.name', 'pi',
  'TRUE', 'FALSE', 'NULL', 'Inf', 'NaN', 'NA', 'NA_integer_', 'NA_real_', 'NA_complex_', 'NA_character_'
), collapse = '|'), ')(\\W)', sep = '')

#  at the moment, only highlight function names, strings and comments
hi_latex = function(x) {
  x = gsub('\\\\', '\\\\textbackslash{}', x)
  x = gsub('([{}])', '\\\\\\1', x)
  # yes I know this is stupid...
  x = gsub('\\\\textbackslash\\\\\\{\\\\\\}', '\\\\textbackslash{}', x)
  x = split_lines(x)
  i = grepl('^\\s*#', x)  # whole lines of comments
  x[i] = sprintf('\\hlcom{%s}', x[i])
  # comments: what if # inside quotes?
  if (any(idx <- grepl('#', x) & !grepl('"', x) & !i))
    x[idx] = gsub('(#.*)', '\\\\hlcom{\\1}', x[idx])
  i = which(!i)  # not comments
  # function names
  x[i] = gsub('([[:alnum:]_\\.]+)(\\s*)\\(', '\\\\hlkwd{\\1}\\2(', x[i])
  # character strings
  x[i] = gsub('"([^"]*)"', '\\\\hlstr{"\\1"}', x[i])
  x[i] = gsub("'([^']*)'", "\\\\hlstr{'\\1'}", x[i])
  # do not highlight keywords at the moment
  # x = gsub(hi.keywords, '\\1\\\\hlkwa{\\2}\\3', x)
  x
}
hi_html = function(x) {
  x = gsub('&', "&amp;", x)
  x = gsub('<', '&lt;', x)
  x = gsub('>', '&gt;', x)
  x = split_lines(x)
  # character strings
  x = gsub('"([^"]*)"', '<span class="hl str">"\\1"</span>', x)
  x = gsub("'([^']*)'", "<span class=\"hl str\">'\\1'</span>", x)
  # function names
  x = gsub('([[:alnum:]_\\.]+)(\\s*)\\(', '<span class="hl kwd">\\1</span>\\2(', x)
  if (any(idx <- grepl('#', x) & !grepl('"', x)))
    x[idx] = gsub('(#.*)', '<span class="hl com">\\1</span>', x[idx])
  gsub(hi.keywords, '\\1<span class="hl kwa">\\2</span>\\3', x)
}

hi_naive = function(x, format) {
  switch(format, html = hi_html(x), latex = hi_latex(x))
}

# need functions from my highr package
hilight_fun = function(name) {
  getFromNamespace(name, 'highr')
}

.default.css = css.parser(.default.sty)

hilight_source = function(x, format, options) {
  if (!((format %in% c('latex', 'html')) && options$highlight))
    return(if (options$prompt) line_prompt(x) else x)
  res = if (has_package('highr')) {
    hilight = hilight_fun('hilight')
    hilight(x, format, prompt = options$prompt)
  } else {
    if (options$prompt) x = line_prompt(x)
    hi_naive(x, format)
  }
  if (format == 'html') return(res)
  c('\\begin{alltt}', res, '\\end{alltt}')
}
