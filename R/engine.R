#' Engines of other languages
#'
#' This object controls how to execute the code from languages other than R
#' (when the chunk option \code{engine} is not \code{'R'}). Each component in
#' this object is a function that takes a list of current chunk options
#' (including the source code) and returns a character string to be written into
#' the output.
#'
#' The engine function has one argument \code{options}: the source code of the
#' current chunk is in \code{options$code}. Usually we can call external
#' programs to run the code via \code{\link[base]{system}}. Other chunk options
#' are also contained in this argument, e.g. \code{options$echo} and
#' \code{options$eval}, etc.
#' @export
#' @references Usage: \url{http://yihui.name/knitr/objects}
#' @examples knit_engines$get('python'); knit_engines$get('awk')
knit_engines = new_defaults()

# give me source code, text output and I return formatted text using the three
# hooks: source, output and chunk
engine_output = function(code, out, options) {
  if (length(code) != 1L) code = str_c(code, collapse = '\n')
  if (length(out) != 1L) out = str_c(out, collapse = '\n')
  code = str_replace(code, '([^\n]+)$', '\\1\n')
  out = str_replace(out, '([^\n]+)$', '\\1\n')
  txt = paste(c(
    if (options$echo) knit_hooks$get('source')(code, options),
    if (options$results != 'hide' && !is_blank(out)) knit_hooks$get('output')(out, options)
  ), collapse = '\n')
  if (options$include) knit_hooks$get('chunk')(txt, options) else ''
}

## Python
eng_python = function(options) {
  code = str_c(options$code, collapse = '\n')
  cmd = sprintf('python -c %s', shQuote(code))
  out = if (options$eval) system(cmd, intern = TRUE) else ''
  engine_output(code, out, options)
}

## Awk: file is the file to read in; awk.opts are other options to pass to awk
eng_awk = function(options) {
  code = str_c(options$code, collapse = '\n')
  cmd = paste(options$engine, shQuote(code), shQuote(options$file), options$awk.opts)
  out = if (options$eval) system(cmd, intern = TRUE) else ''
  engine_output(code, out, options)
}

## C

## Java

## Ruby
eng_ruby = function(options) {
  code = str_c(options$code, collapse = '\n')
  cmd = sprintf('ruby -e %s', shQuote(code))
  out = if (options$eval) system(cmd, intern = TRUE) else ''
  engine_output(code, out, options)
}

## Haskell
eng_haskell = function(options) {
  code = str_c(options$code, collapse = '\n')
  cmd = sprintf('ghc -e %s', shQuote(code))
  out = if (options$eval) system(cmd, intern = TRUE) else ''
  engine_output(code, out, options)
}


knit_engines$set(python = eng_python, awk = eng_awk, gawk = eng_awk, ruby = eng_ruby, haskell = eng_haskell)
