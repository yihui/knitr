
hilight_source = function(x, format, options) {
  if (!((format %in% c('latex', 'html')) && options$highlight))
    return(if (options$prompt) line_prompt(x) else x)
  res = highr::hilight(x, format, prompt = options$prompt)
  if (format == 'html') return(res)
  c('\\begin{alltt}', res, '\\end{alltt}')
}
