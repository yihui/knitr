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
#'
#' In most cases, \code{options$engine} can be directly used in command line to
#' execute the code, e.g. \code{python} or \code{ruby}, but sometimes we may
#' want to specify the path of the engine program, in which case we can pass it
#' through the \code{engine.path} option. For example, \code{engine='ruby',
#' engine.path='/usr/bin/ruby1.9.1'}. Additional command line arguments can be
#' passed through \code{options$engine.opts}, e.g. \code{engine='ruby',
#' engine.opts='-v'}.
#' @export
#' @references Usage: \url{http://yihui.name/knitr/objects}
#' @examples knit_engines$get('python'); knit_engines$get('awk')
#' names(knit_engines$get())
knit_engines = new_defaults()

# give me source code, text output and I return formatted text using the three
# hooks: source, output and chunk
engine_output = function(options, code, out, extra = NULL) {
  if (length(code) != 1L) code = str_c(code, collapse = '\n')
  if (length(out) != 1L) out = str_c(out, collapse = '\n')
  code = str_replace(code, '([^\n]+)$', '\\1\n')
  out = str_replace(out, '([^\n]+)$', '\\1\n')
  if (options$engine == 'Rscript') options$engine = 'r'
  txt = paste(c(
    if (options$echo) knit_hooks$get('source')(code, options),
    if (options$results != 'hide' && !is_blank(out)) {
      if (!output_asis(out, options)) out = comment_out(out, options$comment)
      knit_hooks$get('output')(out, options)
    },
    extra
  ), collapse = '\n')
  if (options$include) knit_hooks$get('chunk')(txt, options) else ''
}

## TODO: how to emulate the console?? e.g. for Python

eng_interpreted = function(options) {
  code = if (options$engine %in% c('highlight', 'Rscript', 'sas')) {
    f = basename(tempfile(options$engine, '.', '.txt'))
    writeLines(options$code, f)
    on.exit(unlink(f))
    f
  } else if (options$engine %in% c('haskell')) {
    # need multiple -e because the engine does not accept \n in code
    paste('-e', shQuote(options$code), collapse = ' ')
  } else paste(switch(
    options$engine, bash = '-c', coffee = '-p -e', perl = '-e', python = '-c',
    ruby = '-e', sh = '-c', zsh = '-c', NULL
  ), shQuote(str_c(options$code, collapse = '\n')))
  # FIXME: for these engines, the correct order is options + code + file
  code = if (options$engine %in% c('awk', 'gawk', 'sed'))
    paste(code, options$engine.opts) else paste(options$engine.opts, code)
  cmd = paste(shQuote(options$engine.path %n% options$engine), code)
  message('running: ', cmd)
  out = if (options$eval) system(cmd, intern = TRUE) else ''
  engine_output(options, options$code, out)
}
## C

## Java

## Rcpp
eng_Rcpp = function(options) {

  code = str_c(options$code, collapse = '\n')
  # engine.opts is a list of arguments to be passed to Rcpp function, e.g.
  # engine.opts=list(plugin='RcppArmadillo')
  opts = options$engine.opts
  if (!is.environment(opts$env)) opts$env = knit_global() # default env is knit_global()
  if (options$eval) {
    message('Building shared library for Rcpp code chunk...')
    do.call(Rcpp::sourceCpp, c(list(code = code), opts))
  }

  options$engine = 'cpp' # wrap up source code in cpp syntax instead of Rcpp
  engine_output(options, code, '')
}

## convert tikz string to PDF
eng_tikz = function(options) {
  if (!options$eval) return(engine_output(options, options$code, ''))

  lines = readLines(tmpl <- options$engine.opts$template %n%
                      system.file('misc', 'tikz2pdf.tex', package = 'knitr'))
  i = grep('%% TIKZ_CODE %%', lines)
  if (length(i) != 1L)
    stop("Couldn't find replacement string; or the are multiple of them.")

  s = append(lines, options$code, i)  # insert tikz into tex-template
  writeLines(s, texf <- str_c(f <- tempfile('tikz', '.'), '.tex'))
  unlink(outf <- str_c(f, '.pdf'))
  texi2pdf(texf, clean = TRUE)
  if (!file.exists(outf)) stop('failed to compile tikz; check the template: ', tmpl)
  unlink(texf)

  fig = fig_path('', options)
  file.rename(outf, str_c(fig, '.pdf'))
  # convert to the desired output-format, calling `convert`
  ext = tolower(options$fig.ext %n% dev2ext(options$dev))
  if (ext != 'pdf') {
    conv = system(sprintf('convert %s.pdf %s.%s', fig, fig, ext))
    if (conv != 0) stop('problems with `convert`; probably not installed?')
  }
  options$fig.num = 1L; options$fig.cur = 1L
  extra = knit_hooks$get('plot')(c(fig, ext), options)
  options$engine = 'tex'  # for output hooks to use the correct language class
  engine_output(options, options$code, '', extra)
}

## GraphViz (dot)
eng_dot = function(options){
  f = tempfile()
  writeLines(code <- options$code, f)
  on.exit(unlink(f))
  cmd = sprintf('%s %s -T%s -o%s', shQuote(options$engine %n% options$engine.path),
                shQuote(f), ext <- options$fig.ext %n% dev2ext(options$dev),
                shQuote(str_c(fig <- fig_path(), '.', ext)))
  dir.create(dirname(fig), showWarnings = FALSE)
  extra = if (options$eval) {
    system(cmd)
    options$fig.num = 1L; options$fig.cur = 1L
    knit_hooks$get('plot')(c(fig, ext), options)
  }
  engine_output(options, code, '', extra)
}

## Andre Simon's highlight
eng_highlight = function(options) {
  # e.g. engine.opts can be '-S matlab -O latex'
  if (is.null(options$engine.opts)) options$engine.opts = '-S text'
  options$engine.opts[1L] = paste('-f', options$engine.opts[1L])
  options$echo = FALSE; options$results = 'asis'  # do not echo source code
  eng_interpreted(options)
}

# set engines for interpreted languages
for (i in c('awk', 'bash', 'coffee', 'gawk', 'haskell', 'perl', 'python',
            'Rscript', 'ruby', 'sas', 'sed', 'sh', 'zsh')) {
  knit_engines$set(setNames(list(eng_interpreted), i))
}
# additional engines
knit_engines$set(
  highlight = eng_highlight, Rcpp = eng_Rcpp, tikz = eng_tikz, dot = eng_dot
)

# possible values for engines (for auto-completion in RStudio)
opts_chunk_attr$engine = as.list(sort(c('R', names(knit_engines$get()))))
opts_chunk_attr[c('engine.path', 'engine.opts')] = list('character', 'character')
