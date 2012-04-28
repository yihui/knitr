#' Engines of other languages
#'
#' This object controls how to execute the code from languages other than R
#' (when the chunk option \code{engine} is not \code{'R'}). Each component in
#' this object is a function that takes a list of current chunk options
#' (including the source code) and returns a character string to be written into
#' the output.
#' @export
#' @references Usage: \url{http://yihui.name/knitr/objects}
#' @examples knit_engines$get('python'); knit_engines$get('awk')
knit_engines = new_defaults()

wrap_fmt = function(x, lang = '') {
  fmt = opts_knit$get('out.format')
  tpl = if (fmt %in% c('latex', 'listings', 'sweave')) {
    '\\begin{verbatim}\n%s\\end{verbatim}'
  } else switch(fmt, html = '<pre class="knitr">%s</pre>',
                markdown = str_c('```', lang, '\n%s\n```'),
                rst = str_c('::\n\n', indent_block(x), '\n'),
                jekyll = str_c('{%% highlight ', if (lang == '') 'text' else lang,
                               ' %%}\n%s\n{%% endhighlight %%}'),
                '%s')
  sprintf(tpl, str_c(x, collapse = '\n'))
}

## Python
eng_python = function(options) {
  code = str_c(options$code, collapse = '\n')
  cmd = sprintf('python -c %s', shQuote(code))
  out = system(cmd, intern = TRUE)
  str_c(wrap_fmt(code, 'python'), '\n', wrap_fmt(out))
}

## Awk: file is the file to read in; awk.opts are other options to pass to awk
eng_awk = function(options) {
  code = str_c(options$code, collapse = '\n')
  cmd = paste(options$engine, shQuote(code), shQuote(options$file), options$awk.opts)
  out = system(cmd, intern = TRUE)
  str_c(wrap_fmt(code, 'awk'), '\n', wrap_fmt(out))
}

## C

## Java

knit_engines$set(python = eng_python, awk = eng_awk, gawk = eng_awk)
