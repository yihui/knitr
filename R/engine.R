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
#' programs to run the code via \code{\link{system2}}. Other chunk options are
#' also contained in this argument, e.g. \code{options$echo} and
#' \code{options$eval}, etc.
#'
#' In most cases, \code{options$engine} can be directly used in command line to
#' execute the code, e.g. \code{python} or \code{ruby}, but sometimes we may
#' want to specify the path of the engine program, in which case we can pass it
#' through the \code{engine.path} option. For example, \code{engine='ruby',
#' engine.path='/usr/bin/ruby1.9.1'}. Additional command line arguments can be
#' passed through \code{options$engine.opts}, e.g. \code{engine='ruby',
#' engine.opts='-v'}.
#'
#' Below is a list of built-in language engines, retrieved via
#' \code{knit_engines$get()}:
#'
#' \Sexpr[results=verbatim]{str(knitr::knit_engines$get())}
#' @export
#' @references Usage: \url{http://yihui.name/knitr/objects}; examples:
#'   \url{http://yihui.name/knitr/demo/engines/}
#' @examples knit_engines$get('python'); knit_engines$get('awk')
#' names(knit_engines$get())
knit_engines = new_defaults()

#' An output wrapper for language engine output
#'
#' If you have designed a language engine, you may call this function in the end
#' to format and return the text output from your engine.
#' @param options a list of chunk options (usually this is just the object
#'   \code{options} passed to the engine function; see
#'   \code{\link{knit_engines}})
#' @param code the source code of the chunk, to which the output hook
#'   \code{source} is applied, unless the chunk option \code{echo == FALSE}
#' @param out the text output from the engine, to which the hook \code{output}
#'   is applied, unless the chunk option \code{results == 'hide'}
#' @param extra any additional text output that you want to include
#' @return A character string generated from the source code and output using
#'   the appropriate output hooks.
#' @export
engine_output = function(options, code, out, extra = NULL) {
  if (length(code) != 1L) code = paste(code, collapse = '\n')
  if (length(out) != 1L) out = paste(out, collapse = '\n')
  out = sub('([^\n]+)$', '\\1\n', out)
  # replace the engine names for markup later, e.g. ```Rscript should be ```r
  options$engine = switch(
    options$engine, 'Rscript' = 'r', node = 'javascript',
    options$engine
  )
  paste(c(
    if (options$echo) knit_hooks$get('source')(code, options),
    if (options$results != 'hide' && !is_blank(out)) {
      if (options$engine == 'highlight') out else wrap.character(out, options)
    },
    extra
  ), collapse = '\n')
}

## TODO: how to emulate the console?? e.g. for Python
#  see some experiments at https://github.com/yihui/runr

eng_interpreted = function(options) {
  engine = options$engine
  code = if (engine %in% c('highlight', 'Rscript', 'sas', 'haskell')) {
    f = basename(tempfile(engine, '.', switch(engine, sas = '.sas', Rscript = '.R', '.txt')))
    # SAS runs code in example.sas and creates 'listing' file example.lst and log file example.log
    writeLines(c(switch(
      engine,
      sas = "OPTIONS NONUMBER NODATE PAGESIZE = MAX FORMCHAR = '|----|+|---+=|-/<>*' FORMDLIM=' ';",
      haskell = ':set +m'
    ), options$code), f)
    on.exit(unlink(f))
    switch(engine, sas = {
      saslst = sub('[.]sas$', '.lst', f)
      on.exit(unlink(c(saslst, sub('[.]sas$', '.log', f))), add = TRUE)
      f
    }, haskell = paste('-e', shQuote(paste(':script', f))), f)
  } else paste(switch(
    engine, bash = '-c', coffee = '-e', groovy = '-e', node = '-e', perl = '-e',
    python = '-c', ruby = '-e', scala = '-e', sh = '-c', zsh = '-c', NULL
  ), shQuote(paste(options$code, collapse = '\n')))
  # FIXME: for these engines, the correct order is options + code + file
  code = if (engine %in% c('awk', 'gawk', 'sed', 'sas'))
    paste(code, options$engine.opts) else paste(options$engine.opts, code)
  cmd = options$engine.path %n% engine
  out = if (options$eval) {
    message('running: ', cmd, ' ', code)
    system2(cmd, code, stdout = TRUE, stderr = TRUE)
  } else ''
  # chunk option error=FALSE means we need to signal the error
  if (!options$error && !is.null(attr(out, 'status')))
    stop(paste(out, collapse = '\n'))
  if (options$eval && engine == 'sas' && file.exists(saslst))
    out = c(readLines(saslst), out)
  engine_output(options, options$code, out)
}

## C and Fortran (via R CMD SHLIB)
eng_shlib = function(options) {
  n = switch(options$engine, c = 'c', fortran = 'f')
  f = basename(tempfile(n, '.', paste('.', n, sep = '')))
  writeLines(options$code, f)
  on.exit(unlink(c(f, sub_ext(f, c('o', 'so', 'dll')))))
  if (options$eval) {
    out = system(paste('R CMD SHLIB', f), intern = TRUE)
    dyn.load(sub(sprintf('[.]%s$', n), .Platform$dynlib.ext, f))
  } else out = ''
  engine_output(options, options$code, out)
}

## Java
#  e.g. see http://cran.rstudio.com/package=jvmr

## Rcpp
eng_Rcpp = function(options) {

  code = paste(options$code, collapse = '\n')
  # engine.opts is a list of arguments to be passed to Rcpp function, e.g.
  # engine.opts=list(plugin='RcppArmadillo')
  opts = options$engine.opts
  if (!is.environment(opts$env)) opts$env = knit_global() # default env is knit_global()
  if (options$eval) {
    message('Building shared library for Rcpp code chunk...')
    do.call(getFromNamespace('sourceCpp', 'Rcpp'), c(list(code = code), opts))
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
  tools::texi2pdf(texf, clean = TRUE)
  if (!file.exists(outf)) stop('failed to compile tikz; check the template: ', tmpl)
  unlink(texf)

  fig = fig_path('', options)
  dir.create(dirname(fig), recursive = TRUE, showWarnings = FALSE)
  file.rename(outf, str_c(fig, '.pdf'))
  # convert to the desired output-format, calling `convert`
  ext = tolower(options$fig.ext %n% dev2ext(options$dev))
  if (ext != 'pdf') {
    conv = system(sprintf('convert %s.pdf %s.%s', fig, fig, ext))
    if (conv != 0) stop('problems with `convert`; probably not installed?')
  }
  options$fig.num = 1L; options$fig.cur = 1L
  extra = knit_hooks$get('plot')(paste(fig, ext, sep = '.'), options)
  options$engine = 'tex'  # for output hooks to use the correct language class
  engine_output(options, options$code, '', extra)
}

## GraphViz (dot) and Asymptote are similar
eng_dot = function(options) {

  # create temporary file
  f = tempfile('code', '.')
  writeLines(code <- options$code, f)
  on.exit(unlink(f))

  # adapt command to either graphviz or asymptote
  if (options$engine == 'dot') {
    command_string = '%s %s -T%s -o%s'
    syntax         = 'dot'
  } else if (options$engine == 'asy') {
    command_string = '%s %s -f %s -o %s'
    syntax         = 'cpp'  # use cpp syntax for syntax highlighting
  }

  # prepare system command
  cmd = sprintf(command_string, shQuote(options$engine %n% options$engine.path),
                shQuote(f), ext <- options$fig.ext %n% dev2ext(options$dev),
                shQuote(str_c(fig <- fig_path(), '.', ext)))

  # generate output
  dir.create(dirname(fig), recursive = TRUE, showWarnings = FALSE)
  outf = paste(fig, ext, sep = '.')
  unlink(outf)
  extra = if (options$eval) {
    message('running: ', cmd)
    system(cmd)
    if (!file.exists(outf)) stop('failed to compile content');
    options$fig.num = 1L; options$fig.cur = 1L
    knit_hooks$get('plot')(outf, options)
  }

  # wrap
  options$engine = syntax
  engine_output(options, code, '', extra)
}

## Andre Simon's highlight
eng_highlight = function(options) {
  # e.g. engine.opts can be '-S matlab -O latex'
  if (is.null(options$engine.opts)) options$engine.opts = '-S text'
  options$engine.opts[1L] = paste('-f', options$engine.opts[1L])
  options$echo = FALSE; options$results = 'asis'  # do not echo source code
  res = eng_interpreted(options)
  if (out_format('latex')) {
    highlight_header()
    sub('(.*)\\\\\\\\(.*)', '\\1\\2', res)
  } else res
}

## save the code
eng_cat = function(options) {
  lang = options$engine.opts$lang
  if (!is.null(lang)) options$engine.opts$lang = NULL
  do.call(cat, c(list(options$code, sep = '\n'), options$engine.opts))
  if (is.null(lang)) return('')
  options$engine = lang
  engine_output(options, options$code, NULL)
}

## output the code without processing it
eng_asis = function(options) {
  if (options$echo && options$eval) options$code
}

# set engines for interpreted languages
for (i in c(
  'awk', 'bash', 'coffee', 'gawk', 'groovy', 'haskell', 'node', 'perl', 'python',
  'Rscript', 'ruby', 'sas', 'scala', 'sed', 'sh', 'zsh'
)) knit_engines$set(setNames(list(eng_interpreted), i))
rm(i)

# additional engines
knit_engines$set(
  highlight = eng_highlight, Rcpp = eng_Rcpp, tikz = eng_tikz, dot = eng_dot,
  c = eng_shlib, fortran = eng_shlib, asy = eng_dot, cat = eng_cat,
  asis = eng_asis
)

# possible values for engines (for auto-completion in RStudio)
opts_chunk_attr$engine = as.list(sort(c('R', names(knit_engines$get()))))
opts_chunk_attr[c('engine.path', 'engine.opts')] = list('character', 'character')
