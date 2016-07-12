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
#' @note The Leiningen engine \code{lein} requires lein-exec plugin; see
#'   \url{https://github.com/yihui/knitr/issues/1176} for details.
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
  if (!is.logical(options$echo)) code = code[options$echo]
  if (length(code) != 1L) code = paste(code, collapse = '\n')
  if (options$engine == 'sas' && length(out) > 1L && !grepl('[[:alnum:]]', out[2]))
    out = tail(out, -3L)
  if (length(out) != 1L) out = paste(out, collapse = '\n')
  out = sub('([^\n]+)$', '\\1\n', out)
  # replace the engine names for markup later, e.g. ```Rscript should be ```r
  options$engine = switch(
    options$engine, mysql = 'sql', node = 'javascript', psql = 'sql', Rscript = 'r',
    options$engine
  )
  if (options$engine == 'stata') {
    out = gsub('\n\nrunning.*profile.do', '', out)
    out = sub('...\n\n\n', '', out)
    out = sub('\n. \nend of do-file\n', '', out)
  }
  paste(c(
    if (length(options$echo) > 1L || options$echo) knit_hooks$get('source')(code, options),
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
  code = if (engine %in% c('highlight', 'Rscript', 'sas', 'haskell', 'stata')) {
    f = basename(tempfile(engine, '.', switch(
      engine, sas = '.sas', Rscript = '.R', stata = '.do', '.txt'
    )))
    writeLines(c(switch(
      engine,
      sas = "OPTIONS NONUMBER NODATE PAGESIZE = MAX FORMCHAR = '|----|+|---+=|-/<>*' FORMDLIM=' ';title;",
      haskell = ':set +m'
    ), options$code), f)
    on.exit(unlink(f))
    switch(
      engine,
      haskell = paste('-e', shQuote(paste(':script', f))),
      sas = {
        logf = sub('[.]sas$', '.lst', f)
        on.exit(unlink(c(logf, sub('[.]sas$', '.log', f))), add = TRUE)
        f
      },
      stata = {
        logf = sub('[.]do$', '.log', f)
        on.exit(unlink(c(logf)), add = TRUE)
        paste(switch(Sys.info()[['sysname']], Windows='/q /e do', Darwin='-q -e do', Linux='-q -b do', '-q -b do'), f)
      },
      f
    )
  } else paste(switch(
    engine, bash = '-c', coffee = '-e', groovy = '-e', lein = 'exec -e',
    mysql = '-e', node = '-e', perl = '-e', psql = '-c', python = '-c',
    ruby = '-e', scala = '-e', sh = '-c', zsh = '-c', NULL
  ), shQuote(paste(options$code, collapse = '\n')))

  # FIXME: for these engines, the correct order is options + code + file
  code = if (engine %in% c('awk', 'gawk', 'sed', 'sas'))
    paste(code, options$engine.opts) else paste(options$engine.opts, code)
  cmd = options$engine.path %n% engine
  out = if (options$eval) {
    message('running: ', cmd, ' ', code)
    tryCatch(
      system2(cmd, code, stdout = TRUE, stderr = TRUE, env = options$engine.env),
      error = function(e) {
        if (!options$error) stop(e)
        paste('Error in running command', cmd)
      }
    )
  } else ''
  # chunk option error=FALSE means we need to signal the error
  if (!options$error && !is.null(attr(out, 'status')))
    stop(paste(out, collapse = '\n'))
  if (options$eval && engine %in% c('sas', 'stata') && file.exists(logf))
    out = c(readLines(logf), out)
  engine_output(options, options$code, out)
}

## C and Fortran (via R CMD SHLIB)
eng_shlib = function(options) {
  n = switch(options$engine, c = 'c', fortran = 'f')
  f = basename(tempfile(n, '.', paste0('.', n)))
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

## Stan
## Compiles Stan model in the code chunk, creates a stanmodel object,
## and assigns it to a variable with the name given in engine.opts$x.
eng_stan = function(options) {
  code = paste(options$code, collapse = '\n')
  opts = options$engine.opts
  ## name of the modelfit object returned by stan_model
  x = opts$x
  if (!is.character(x) || length(x) != 1L) stop(
    "engine.opts$x must be a character string; ",
    "provide a name for the returned `stanmodel` object."
  )
  opts$x = NULL
  if (options$eval) {
    message("Creating a 'stanmodel' object ", x)
    assign(
      x,
      do.call(getFromNamespace('stan_model', 'rstan'), c(list(model_code = code), opts)),
      envir = knit_global()
    )
  }
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
  writeLines(s, texf <- paste0(f <- tempfile('tikz', '.'), '.tex'))
  on.exit(unlink(texf), add = TRUE)

  ext = tolower(options$fig.ext %n% dev2ext(options$dev))

  to_svg = ext == 'svg'
  unlink(outf <- paste0(f, if (to_svg) '.dvi' else '.pdf'))
  tools::texi2dvi(texf, pdf = !to_svg, clean = TRUE)
  if (!file.exists(outf)) stop('Failed to compile tikz; check the template: ', tmpl)

  fig = fig_path(if (to_svg) '.dvi' else '.pdf', options)
  dir.create(dirname(fig), recursive = TRUE, showWarnings = FALSE)
  file.rename(outf, fig)

  fig2 = sub_ext(fig, ext)
  if (to_svg) {
    # dvisvgm needs to be on the path
    # dvisvgm for windows needs ghostscript bin dir on the path also
    conv = system2('dvisvgm', fig)
    # copy the svg to figure subdir
    file.rename(basename(fig2), fig2)
  } else {
    # convert to the desired output-format, calling `convert`
    conv = 0
    if (ext != 'pdf') {
      conv = system2(options$engine.opts$convert %n% 'convert', c(
        options$engine.opts$convert.opts, sprintf('%s %s', fig, fig2)
      ))
    }
  }
  if (conv != 0 && !options$error) stop('Failed to compile ', fig, ' to ', fig2)
  fig = fig2

  options$fig.num = 1L; options$fig.cur = 1L
  extra = knit_hooks$get('plot')(fig, options)
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
                shQuote(paste0(fig <- fig_path(), '.', ext)))

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
  cat2 = function(..., file = '', lang = NULL) {
    # do not write to stdout like the default behavior of cat()
    if (!identical(file, '')) cat(..., file = file)
  }
  do.call(cat2, c(list(options$code, sep = '\n'), options$engine.opts))
  if (is.null(lang <- options$engine.opts$lang)) return('')
  options$engine = lang
  engine_output(options, options$code, NULL)
}

## output the code without processing it
eng_asis = function(options) {
  if (options$echo && options$eval) paste(options$code, collapse = '\n')
}

# write a block environment according to the output format
eng_block = function(options) {
  if (isFALSE(options$echo)) return()
  code = paste(options$code, collapse = '\n')
  to = pandoc_to()
  is_pandoc = !is.null(to)
  if (!is_pandoc) {
    # not in R Markdown v2
    to = out_format()
    if (!(to %in% c('latex', 'html', 'markdown'))) to = NULL
  }
  if (is.null(to)) return(code)
  if (to == 'beamer') to = 'latex'
  if (is_html_output(to)) to = 'html'
  type = options$type
  if (is.null(type)) return(code)
  # convert the chunk content to HTML or LaTeX (ideally I only need to specify
  # the markdown extension, but it is not implemented yet for LaTeX:
  # https://github.com/jgm/pandoc/issues/2453)
  if (is_pandoc) code = pandoc_fragment(code, to)
  l1 = options$latex.options
  l1 = if (is.null(l1)) '' else paste0('[', l1, ']')
  h2 = options$html.tag %n% 'div'
  h3 = options$html.before %n% ''
  h4 = options$html.after %n% ''
  # e.g. type = c(latex = 'marginfigure', html = 'marginnote')
  if (to %in% names(type)) type = type[to]
  # block level tags? this is an incomplete list, but should work for most cases
  if (to == 'html') if (h2 %in% c('div', 'p', 'blockquote')) {
    code = paste0('\n', code, '\n')
  } else {
    code = gsub('<p>', '<span style="display: block;">', code)
    code = gsub('</p>', '</span>', code)
  }
  switch(
    to,
    latex = sprintf('\\begin%s{%s}\n%s\n\\end{%s}', l1, type, code, type),
    html =  sprintf('%s<%s class="%s">%s</%s>%s', h3, h2, type, code, h2, h4),
    code
  )
}

# helper to create engines the wrap embedded html assets (e.g. css,js)
eng_html_asset = function(prefix, postfix) {
  function(options) {
    if (options$eval && is_html_output(excludes = 'markdown')) {
      code = c(prefix, options$code, postfix)
      paste(code, collapse = '\n')
    }
  }
}

# include js in a script tag (ignore if not html output)
eng_js = eng_html_asset('<script type="text/javascript">', '</script>')

# include css in a style tag (ignore if not html output)
eng_css = eng_html_asset('<style type="text/css">', '</style>')

# sql engine
eng_sql = function(options) {
  # Return char vector of sql interpolation param names
  varnames_from_sql <- function(conn, sql) {
    varPos <- DBI::sqlParseVariables(conn, sql)
    if (length(varPos$start) > 0) {
      varNames <- substring(sql, varPos$start, varPos$end)
      sub("^\\?", "", varNames)
    }
  }

  # Vectorized version of exists
  mexists <- function(x, env = knitr::knit_global(), inherits = TRUE) {
    vapply(x, exists, logical(1), where = env, inherits = inherits)
  }

  # Interpolate a sql query based on the variables in an environment
  interpolate_from_env <- function(conn, sql, env = knitr::knit_global(), inherits = TRUE) {
    names <- unique(varnames_from_sql(conn, sql))
    names_missing <- names[!mexists(names, env, inherits)]
    if (length(names_missing) > 0) {
      stop("Object(s) not found: ",
           paste('"', names_missing, '"', collapse = ", "))
    }

    args <- if (length(names) > 0) {
      setNames(
        mget(names, inherits = inherits),
        names
      )
    }

    do.call(DBI::sqlInterpolate, c(list(conn, sql), args))
  }

  conn <- options$conn
  varname <- options$label
  sql <- options$code

  query <- interpolate_from_env(conn, sql)
  result <- DBI::dbGetQuery(conn, query)
  output <- capture.output(print(result))

  if (!is.null(varname)) {
    assign(varname, result, envir = knitr::knit_global())
  }

  engine_output(options, options$code, NULL)
}

# set engines for interpreted languages
local({
  for (i in c(
    'awk', 'bash', 'coffee', 'gawk', 'groovy', 'haskell', 'lein', 'mysql',
    'node', 'perl', 'psql', 'python', 'Rscript', 'ruby', 'sas', 'scala', 'sed',
    'sh', 'stata', 'zsh'
  )) knit_engines$set(setNames(list(eng_interpreted), i))
})

# additional engines
knit_engines$set(
  highlight = eng_highlight, Rcpp = eng_Rcpp, tikz = eng_tikz, dot = eng_dot,
  c = eng_shlib, fortran = eng_shlib, asy = eng_dot, cat = eng_cat,
  asis = eng_asis, stan = eng_stan, block = eng_block, js = eng_js, css = eng_css,
  sql = eng_sql
)

get_engine = function(name) {
  fun = knit_engines$get(name)
  if (is.function(fun)) return(fun)
  warning(
    "Unknown language engine '", name,
    "' (must be registered via knit_engines$set())."
  )
  function(options) {
    engine_output(options, options$code, '')
  }
}

# possible values for engines (for auto-completion in RStudio)
opts_chunk_attr$engine = as.list(sort(c('R', names(knit_engines$get()))))
opts_chunk_attr[c('engine.path', 'engine.opts')] = list('character', 'character')
