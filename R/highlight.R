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
  x[i] = sprintf('\\hlcomment{%s}', x[i])
  # comments: what if # inside quotes?
  if (any(idx <- grepl('#', x) & !grepl('"', x) & !i))
    x[idx] = gsub('(#.*)', '\\\\hlcomment{\\1}', x[idx])
  i = which(!i)  # not comments
  # function names
  x[i] = gsub('([[:alnum:]_\\.]+)(\\s*)\\(', '\\\\hlfunctioncall{\\1}\\2(', x[i])
  # character strings
  x[i] = gsub('"([^"]*)"', '\\\\hlstring{"\\1"}', x[i])
  x[i] = gsub("'([^']*)'", "\\\\hlstring{'\\1'}", x[i])
  # do not highlight keywords at the moment
  # x = gsub(hi.keywords, '\\1\\\\hlkeyword{\\2}\\3', x)
  x
}
hi_html = function(x) {
  x = gsub('&', "&amp;", x)
  x = gsub('<', '&lt;', x)
  x = gsub('>', '&gt;', x)
  x = split_lines(x)
  # character strings
  x = gsub('"([^"]*)"', '<span class="string">"\\1"</span>', x)
  x = gsub("'([^']*)'", "<span class=\"string\">'\\1'</span>", x)
  # function names
  x = gsub('([[:alnum:]_\\.]+)(\\s*)\\(', '<span class="functioncall">\\1</span>\\2(', x)
  if (any(idx <- grepl('#', x) & !grepl('"', x)))
    x[idx] = gsub('(#.*)', '<span class="comment">\\1</span>', x[idx])
  gsub(hi.keywords, '\\1<span class="keyword">\\2</span>\\3', x)
}

# may require the highlight package
highlight_fun = function(name) {
  do.call('library', list(package = 'parser', character.only = TRUE))
  getFromNamespace(name, 'highlight')
}

.default.css = css.parser(.default.sty)

hilight_source = function(x, format, options) {
  if (!((format %in% c('latex', 'html')) && options$highlight))
    return(if (options$prompt) line_prompt(x) else x)
  if (opts_knit$get('use.highlight')) {
    highlight = highlight_fun('highlight')
    x = split_lines(x) # remove the extra \n in code (#331)
    con = textConnection(x)
    on.exit(close(con))
    r = if (format == 'latex') {
      highlight_fun('renderer_latex')(
        document = FALSE, styles = highlight_fun('styler_assistant_latex')(.default.css)
      )
    } else {
      highlight_fun('renderer_html')(document = FALSE, header = function() '', footer = function() '')
    }
    enc = getOption('encoding')
    options(encoding = 'native.enc')  # make sure parser() writes with correct enc
    on.exit(options(encoding = enc), add = TRUE)
    out = capture.output(highlight(con, renderer = r, showPrompts = options$prompt, size = options$size))
    if (format == 'html') out else {
      # gsub() makes sure " will not produce an umlaut
      c('\\begin{flushleft}', gsub('"', '"{}', out), '\\end{flushleft}')
    }
  } else {
    if (options$prompt) x = line_prompt(x)
    if (format == 'html') hi_html(x) else c('\\begin{alltt}', hi_latex(x), '\\end{alltt}')
  }
}
