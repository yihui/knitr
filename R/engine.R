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

## TODO: how to emulate the console?? e.g. for Python

eng_interpreted = function(options) {
  code = str_c(options$code, collapse = '\n')
  code_option = switch(options$engine, bash = '-c', haskell = '-e', perl = '-e',
                       python = '-c', ruby = '-e', sh = '-c', zsh = '-c', '')
  cmd = paste(shQuote(options$engine.path %n% options$engine),
              code_option, shQuote(code), options$engine.opts)
  out = if (options$eval) system(cmd, intern = TRUE) else ''
  engine_output(code, out, options)
}
## C

## Java

## Rcpp
eng_Rcpp = function(options) {

  code = str_c(options$code, collapse = '\n')
  # engine.opts is a list of arguments to be passed to Rcpp function, e.g.
  # engine.opts=list(plugin='RcppArmadillo')
  if (options$eval) {
    message('Building shared library for Rcpp code chunk:')
    do.call(
      if (grepl('\\[\\[Rcpp::', code)) Rcpp::sourceCpp else Rcpp::cppFunction,
      c(list(code = code, env = knit_global()), options$engine.opts)
    )
  }

  options$engine = 'cpp' # wrap up source code in cpp syntax instead of Rcpp
  engine_output(code, '', options)
}

## tikz
eng_tikz = function(options) {
  procTikzString <- 
      function   # Converts a tikz-string into pdf by calling `pdflatex` and
                 ### returns a reference in current output
  (
      tikz        # lines of tikz
     ,tmpl        # file-name of tex-template 
     ,dir         # output-directory
     ,label       # path to output-file
     ,cap = label # figure caption
     ,dev = "pdf" # device to use
     ,repl = "<>" # replacement-string
          
  )
  {
      # Insert tikz into tex-template
      templ_lines <- readLines(tmpl)
      i <- grep(repl, templ_lines)
      if  (length(i)  != 1 ) 
          stop("Couldn't find replacement string; or the are multiple of them.")
      s <- c(templ_lines[1:(i-1)], tikz, templ_lines[(i+1):length(templ_lines)])
      # Call `pdflatex` to generate the pdf
      f <- tempfile()
      tex_file <- paste(f, ".tex", sep = "")
      writeLines(s, tex_file)
      cwd = getwd()
      setwd(dirname(tex_file))
      cmd = sprintf("pdflatex %s > /dev/null", tex_file)
      exit_tex = system(cmd)
      outfile = sprintf("%s%s.%s", dir, label, dev)
      if  (exit_tex != 0)
          stop("Problems with pdflatex and input file ", f, "; try to edit ", templ)
      # Convert to the desired output-format, calling `convert`
      if (dev != "pdf")
      {
          exit_conv = system(sprintf("convert %s.pdf %s.%s", f, f, dev))
          if (exit_conv != 0)
              stop("Problems with `convert`; probably not installed")
      }
      setwd(cwd)
      dir.create(dir, showWarnings = FALSE)
      file.copy(paste(f,".", dev, sep = ""), outfile)
      options$fig.num = 1
      knit_hooks$get('plot')(c(paste(dir, label, sep = ""),dev), options)
      ### Produces as side effect the output-pdf and returns a markdown string
  }
  # define defaults
  if  (is.null(options$engine.opts)) 
      options$engine.opts <- list()
  if (is.null(options$engine.opts$repl.st)) 
      options$engine.opts$repl.st = "<>"
  if (is.null(options$engine.opts$repl.tmpl)) 
      options$engine.opts$repl.tmpl = ".tikz2pdf.tex.st"
  out = 
  {
      if (options$eval)
      {
          TIKZ_TMPL = options$engine.opts$repl.tmpl
          if (!file.exists(TIKZ_TMPL))
              file.copy(system.file("misc/tikz2pdf.tex.st", package = "knitr"), TIKZ_TMPL)
          with(options, 
               procTikzString(code, TIKZ_TMPL, fig.path, label, 
                              fig.cap, dev, options$engine.opts$repl.st))
      }
      else 
          ''
  }
  options$results = 'asis'
  code = str_c(options$code, collapse = '\n')
  engine_output(code, out, options)
}

## dot
eng_dot = function(options){
  f = tempfile()
  writeLines(code <- options$code, f)
  on.exit(unlink(f))
  cmd = sprintf('dot -O %s -T%s', shQuote(f), options$dev)
  dir.create(options$fig.path, showWarnings = FALSE)
  out = 
  {
      if (options$eval) 
      {
          system(cmd)
          fig = with(options, paste(fig.path, label, ".", dev, sep = "" ))
          file.copy(paste(f, options$dev, sep = "."), fig)
          options$fig.num = 1
          with(options, 
              knit_hooks$get('plot')(c(paste(fig.path, label, sep = ""), options$dev), options)
          )
          
      } else 
          ''
  }
  options$results = 'asis'
  engine_output(code, out, options)
}
## Andre Simon's highlight
eng_highlight = function(options) {
  f = tempfile()
  writeLines(code <- options$code, f)
  on.exit(unlink(f))
  # e.g. engine.opts can be '-S matlab -O latex'
  if (!is.null(options$highlight.opts)) {
    warning("chunk option 'highlight.opts' has been deprecated; use 'engine.opts' instead")
    options$engine.opts = options$highlight.opts
  }
  cmd = sprintf('%s -f %s %s', shQuote(options$engine.path %n% options$engine),
                options$engine.opts %n% '-S text', shQuote(f))
  out = if (options$eval) system(cmd, intern = TRUE) else ''
  options$echo = FALSE; options$results = 'asis'  # do not echo source code
  engine_output('', out, options)
}

# set engines for interpreted languages
for (i in c('awk', 'bash', 'gawk', 'haskell', 'perl', 'python', 'ruby', 'sed', 'sh', 'zsh')) {
  knit_engines$set(setNames(list(eng_interpreted), i))
}
# additional engines
knit_engines$set(highlight = eng_highlight, Rcpp = eng_Rcpp, tikz = eng_tikz, dot = eng_dot)

# possible values for engines (for auto-completion in RStudio)
opts_chunk_attr$engine = as.list(sort(c('R', names(knit_engines$get()))))
opts_chunk_attr[c('engine.path', 'engine.opts')] = list('character', 'character')
