## my extremely simple 'syntax highlighter' as a substitute for Romain's highlight()
hi.keywords =  paste('(\\W)(', paste(c(
  'if', 'else', 'repeat', 'while', 'function', 'for', 'in', 'next', 'break', 'repeat',
  'LETTERS', 'letters', 'month.abb', 'month.name', 'pi',
  'TRUE', 'FALSE', 'NULL', 'Inf', 'NaN', 'NA', 'NA_integer_', 'NA_real_', 'NA_complex_', 'NA_character_'
), collapse = '|'), ')(\\W)', sep = '')

#  at the moment, only highlight function names, strings and comments
hi_latex = function(x, fragment = FALSE) {
  x = gsub('\\\\', '\\\\textbackslash{}', x)
  x = gsub('([{}])', '\\\\\\1', x)
  # yes I know this is stupid...
  x = gsub('\\\\textbackslash\\\\\\{\\\\\\}', '\\\\textbackslash{}', x)
  x = unlist(strsplit(x, '\n'))
  # function names
  x = gsub('([[:alnum:]_\\.]+)(\\s*)\\(', '\\\\hlfunctioncall{\\1}\\2(', x)
  # comments: what if # inside quotes?
  if (any(idx <- grepl('#', x) & !grepl('"', x)))
    x[idx] = gsub('(#.*)', '\\\\hlcomment{\\1}', x[idx])
  # character strings
  x = gsub('"([^"]*)"', '\\\\hlstring{"\\1"}', x)
  x = gsub("'([^']*)'", "\\\\hlstring{'\\1'}", x)
  # do not highlight keywords at the moment
  # x = gsub(hi.keywords, '\\1\\\\hlkeyword{\\2}\\3', x)
  x = paste(x, collapse = '\n')
  if (!fragment) x = paste('\\begin{alltt}', x, '\\end{alltt}', sep = '\n')
  x
}
hi_html = function(x) {
  x = gsub('&', "&amp;", x)
  x = gsub('<', '&lt;', x)
  x = gsub('>', '&gt;', x)
  x = unlist(strsplit(x, '\n'))
  # character strings
  x = gsub('"([^"]*)"', '<span class="string">"\\1"</span>', x)
  x = gsub("'([^']*)'", "<span class=\"string\">'\\1'</span>", x)
  # function names
  x = gsub('([[:alnum:]_\\.]+)(\\s*)\\(', '<span class="functioncall">\\1</span>\\2(', x)
  if (any(idx <- grepl('#', x) & !grepl('"', x)))
    x[idx] = gsub('(#.*)', '<span class="comment">\\1</span>', x[idx])
  x = gsub(hi.keywords, '\\1<span class="keyword">\\2</span>\\3', x)
  paste(x, collapse = '\n')
}

# may require the orphaned highlight package

hiren_latex = renderer_latex(document = FALSE,
                             styles = styler_assistant_latex(css.parser(.default.sty)))
hiren_html = renderer_html(document = FALSE, header = function() '', footer = function() '')

hilight_source = function(x, format, options) {
  if (!(format %in% c('latex', 'html'))) return(x)
  if (has_package('highlight')) {
    highlight = getFromNamespace('highlight', 'highlight')
    x = unlist(strsplit(x, '\n')) # remove the extra \n in code (#331)
    con = textConnection(x)
    on.exit(close(con))
    r = if (format == 'latex') hiren_latex else hiren_html
    enc = getOption('encoding')
    options(encoding = 'native.enc')  # make sure parser() writes with correct enc
    on.exit(options(encoding = enc), add = TRUE)
    out = capture.output(highlight(con, renderer = r, showPrompts = options$prompt, size = options$size))
    str_c(out, collapse = '\n')
  } else {
    if (options$prompt) x = sapply(x, line_prompt, USE.NAMES = FALSE)
    do.call(paste('hi', format, sep = '_'), list(x = x))
  }
}
