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
## python perl ruby haskell bash(sh) zsh awk sed
## Python (TODO: how to emulate the console??)
## file is the file to read in cl.opts are additional options to the 
## interpreter
eng_interpreted = function(options) {
  code = str_c(options$code, collapse = '\n')
  code_option = switch(options$engine, bash = '-c', haskell = '-e', perl = '-e', 
                       python = '-c', ruby = '-e', sh = '-c', zsh = '-c', '')
  cmd = paste(options$engine, code_option, shQuote(code), 
              shQuote(options$file), options$cl.opts)
  out = if (options$eval) system(cmd, intern = TRUE) else ''
  engine_output(code, out, options)
}
## C

## Java

## Andre Simon's highlight
eng_highlight = function(options) {
  f = tempfile()
  writeLines(code <- options$code, f)
  on.exit(unlink(f))
  # e.g. highlight.opts can be '-S matlab -O latex'
  cmd = sprintf('highlight -f %s %s', options$highlight.opts %n% '-S text', shQuote(f))
  out = if (options$eval) system(cmd, intern = TRUE) else ''
  options$echo = FALSE; options$results = 'asis'  # do not echo source code
  engine_output(code, out, options)
}

knit_engines$set(
  awk = eng_interpreted, bash = eng_interpreted, gawk = eng_interpreted, 
  haskell = eng_interpreted, highlight = eng_highlight, perl = eng_interpreted, 
  python = eng_interpreted, ruby = eng_interpreted, sed = eng_interpreted, 
  sh = eng_interpreted, zsh = eng_interpreted 
)

# possible values for engines (for auto-completion in RStudio)
opts_chunk_attr$engine = as.list(sort(c('R', names(knit_engines$get()))))
