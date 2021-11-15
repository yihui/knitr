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
#' See \code{str(knitr::knit_engines$get())} for a list of built-in language
#' engines.
#' @export
#' @note The Leiningen engine \code{lein} requires lein-exec plugin; see
#'   \url{https://github.com/yihui/knitr/issues/1176} for details.
#' @references Usage: \url{https://yihui.org/knitr/objects/}; examples:
#'   \url{https://yihui.org/knitr/demo/engines/}
#' @examples knit_engines$get('python'); knit_engines$get('awk')
#' names(knit_engines$get())
knit_engines = new_defaults()


#' Cache engines of other languages
#'
#' This object controls how to load cached environments from languages other
#' than R (when the chunk option \code{engine} is not \code{'R'}). Each
#' component in this object is a function that takes the current path to the
#' chunk cache and loads it into the language environment.
#'
#' The cache engine function has one argument \code{options}, a list containing
#' all chunk options. Note that \code{options$hash} is the path to the current
#' chunk cache with the chunk's hash, but without any file extension, and the
#' language engine may write a cache database to this path (with an extension).
#'
#' The cache engine function should load the cache environment and should know
#' the extension appropriate for the language.
#' @references See \url{https://github.com/rstudio/reticulate/pull/167} for an
#'   implementation of a cache engine for Python.
#' @export
cache_engines = new_defaults()

#' An output wrapper for language engine output
#'
#' If you have designed a language engine, you may call this function in the end
#' to format and return the text output from your engine.
#'
#' For expert users, an advanced usage of this function is
#' \code{engine_output(options, out = LIST)} where \code{LIST} is a list that
#' has the same structure as the output of \code{evaluate::evaluate()}. In this
#' case, the arguments \code{code} and \code{extra} are ignored, and the list is
#' passed to \code{knitr::sew()} to return a character vector of final output.
#' @param options A list of chunk options. Usually this is just the object
#'   \code{options} passed to the engine function; see
#'   \code{\link{knit_engines}}.
#' @param code Source code of the chunk, to which the output hook \code{source}
#'   is applied, unless the chunk option \code{echo} is \code{FALSE}.
#' @param out Text output from the engine, to which the hook \code{output} is
#'   applied, unless the chunk option \code{results} is \code{'hide'}
#' @param extra Any additional text output that you want to include.
#' @return A character string generated from the source code and output using
#'   the appropriate output hooks.
#' @export
#' @examples library(knitr)
#' engine_output(opts_chunk$merge(list(engine = 'Rscript')), code = '1 + 1', out = '[1] 2')
#' engine_output(opts_chunk$merge(list(echo = FALSE, engine = 'Rscript')), code = '1 + 1', out = '[1] 2')
#'
#' # expert use only
#' engine_output(opts_chunk$merge(list(engine = 'python')), out = list(structure(list(src = '1 + 1'), class = 'source'), '2'))
engine_output = function(options, code, out, extra = NULL) {
  if (missing(code) && is.list(out)) return(unlist(sew(out, options)))
  if (!is.logical(options$echo)) code = code[options$echo]
  if (length(code) != 1L) code = one_string(code)
  if (options$engine == 'sas' && length(out) > 1L && !grepl('[[:alnum:]]', out[2]))
    out = tail(out, -3L)
  if (length(out) != 1L) out = one_string(out)
  out = sub('([^\n]+)$', '\\1\n', out)
  # replace the engine names for markup later, e.g. ```Rscript should be ```r
  options$engine = switch(
    options$engine, mysql = 'sql', node = 'javascript', psql = 'sql', Rscript = 'r',
    options$engine
  )
  if (options$engine == 'stata') {
    out = gsub('\n+running.*profile.do', '', out)
    out = sub('...\n+', '', out)
    out = sub('\n. \nend of do-file\n', '', out)
  }
  one_string(c(
    if (length(options$echo) > 1L || options$echo) knit_hooks$get('source')(code, options),
    if (options$results != 'hide' && !is_blank(out)) {
      if (options$engine == 'highlight') out else sew.character(out, options)
    },
    extra
  ))
}

## command-line tools
eng_interpreted = function(options) {
  engine = options$engine
  code = if (engine %in% c('highlight', 'Rscript', 'sas', 'haskell', 'stata')) {
    f = wd_tempfile(engine, switch(engine, sas = '.sas', Rscript = '.R', stata = '.do', '.txt'))
    write_utf8(c(switch(
      engine,
      sas = "OPTIONS NONUMBER NODATE PAGESIZE = MAX FORMCHAR = '|----|+|---+=|-/<>*' FORMDLIM=' ';title;",
      NULL
    ), options$code), f)
    on.exit(unlink(f), add = TRUE)
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
        sprintf(switch(
          Sys.info()[['sysname']],
          Windows = '/q /e do %s',
          Darwin = paste('-q < %s >', shQuote(xfun::normalize_path(logf))),
          Linux = '-q -e do %s',
          '-q -b do %s'
        ), shQuote(normalizePath(f)))
      },
      f
    )
  } else paste(switch(
    engine, bash = '-c', coffee = '-e', groovy = '-e', lein = 'exec -ep',
    mysql = '-e', node = '-e', octave = '--eval', perl = '-E', psql = '-c',
    python = '-c', ruby = '-e', scala = '-e', sh = '-c', zsh = '-c', NULL
  ), shQuote(one_string(options$code)))

  opts = get_engine_opts(options$engine.opts, engine)
  # FIXME: for these engines, the correct order is options + code + file
  code = if (engine %in% c('awk', 'gawk', 'sed', 'sas'))
    paste(code, opts) else paste(opts, code)
  cmd = get_engine_path(options$engine.path, engine)
  out = if (options$eval) {
    if (options$message) message('running: ', cmd, ' ', code)
    tryCatch(
      system2(cmd, code, stdout = TRUE, stderr = TRUE, env = options$engine.env),
      error = function(e) {
        if (!options$error) stop(e)
        paste('Error in running command', cmd)
      }
    )
  } else ''
  # chunk option error=FALSE means we need to signal the error
  if (!options$error && !is.null(attr(out, 'status'))) stop(one_string(out))
  if (options$eval && engine %in% c('sas', 'stata') && file.exists(logf))
    out = c(read_utf8(logf), out)
  engine_output(options, options$code, out)
}

# options$engine.path can be list(name1 = path1, name2 = path2, ...); similarly,
# options$engine.opts can be list(name1 = opts1, ...)
get_engine_opts = function(opts, engine, fallback = '') {
  if (is.list(opts)) opts = opts[[engine]]
  opts %n% fallback
}

get_engine_path = function(path, engine) get_engine_opts(path, engine, engine)

# engine.opts = list(command, file, args, args1, args2)
eng_exec = function(options) {
  opts = options$engine.opts %n% options[c('command', 'file', 'ext', 'args', 'args1', 'args2')]
  if (!is.character(cmd <- opts$command)) stop(
    "The command of the 'exec' engine was not specified."
  )

  # default options
  opts2 = list(
    ext = identity, file = function(code, file) {
      write_utf8(options$code, file)
      file
    }, args = function(code, file) {
      file
    }, clean = function(file) {
      unlink(file)
    }
  )

  opts = merge_list(opts2, opts)
  cmd2 = basename(cmd)  # in case command is a full path
  ext = opts$ext(cmd2)  # file extension
  f = if (is.function(opts$file)) {
    f = wd_tempfile(cmd2, paste0('.', ext))
    if (is.function(opts$clean)) on.exit(opts$clean(f), add = TRUE)
    opts$file(options$code, f)
  }
  a = c(opts$args1, if (is.function(opts$args)) opts$args(options$code, f), opts$args2)

  out = if (options$eval) {
    if (options$message) message('running: ', paste(c(cmd, a), collapse = ' '))
    f2 = wd_tempfile(cmd2)  # capture stderr
    tryCatch({
      res = system2(cmd, shQuote(a), stdout = TRUE, stderr = f2, env = options$engine.env)
      if (!options$error && file.exists(f2) && file.size(f2) > 0) {
        stop(file_string(f2))
      }
      res
    }, error = function(e) {
        if (!options$error) stop(e)
        paste('Error in running command', cmd)
      }
    )
  } else ''
  # chunk option error=FALSE means we need to signal the error
  if (!options$error && !is.null(attr(out, 'status'))) stop(one_string(out))
  engine_output(options, options$code, out)
}

## C, C++, and Fortran (via R CMD SHLIB)
eng_shlib = function(options) {
  n = switch(options$engine, c = 'c', cc  = 'cc', fortran = 'f', fortran95 = 'f95')
  f = wd_tempfile(n, paste0('.', n))
  write_utf8(options$code, f)
  on.exit(unlink(c(f, with_ext(f, c('o', 'so', 'dll')))), add = TRUE)
  if (options$eval) {
    out = system(paste('R CMD SHLIB', f), intern = TRUE)
    dyn.load(sub(sprintf('[.]%s$', n), .Platform$dynlib.ext, f))
  } else out = ''
  engine_output(options, options$code, out)
}

## Python
eng_python = function(options) {
  if (isFALSE(options$python.reticulate)) {
    eng_interpreted(options)
  } else {
    if (!loadable('reticulate')) warning2(
      "The 'python' engine in knitr requires the reticulate package. ",
      "If you do not want to use the reticulate package, set the chunk option ",
      "python.reticulate = FALSE."
    )
    reticulate::eng_python(options)
  }
}

cache_eng_python = function(options) {
  if (isFALSE(options$python.reticulate)) return()
  # TODO: change this hack to reticulate::cache_eng_python(options) after
  # https://github.com/rstudio/reticulate/pull/167 is merged and released
  if (!'cache_eng_python' %in% ls(asNamespace('reticulate'))) return()
  fun = getFromNamespace('cache_eng_python', 'reticulate')
  fun(options)
}

## Java
#  e.g. see http://cran.rstudio.com/package=jvmr

## Rcpp
eng_Rcpp = function(options) {

  sourceCpp = getFromNamespace('sourceCpp', 'Rcpp')

  code = one_string(options$code)
  # engine.opts is a list of arguments to be passed to Rcpp function, e.g.
  # engine.opts=list(plugin='RcppArmadillo')
  opts = options$engine.opts

  # use custom cacheDir for sourceCpp if it's supported
  cache = options$cache && ('cacheDir' %in% names(formals(sourceCpp)))
  if (cache) {
    opts$cacheDir = paste(valid_path(options$cache.path, options$label), 'sourceCpp', sep = '_')
    opts$cleanupCacheDir = TRUE
  }

  if (!is.environment(opts$env)) opts$env = knit_global() # default env is knit_global()
  if (options$eval) {
    message('Building shared library for Rcpp code chunk...')
    do.call(sourceCpp, c(list(code = code), opts))
  }

  options$engine = 'cpp' # wrap up source code in cpp syntax instead of Rcpp
  engine_output(options, code, '')
}

## Julia
eng_julia = function(options) {
  JuliaCall::eng_juliacall(options)
}

## Stan
## Compiles Stan model in the code chunk, creates a stanmodel object,
## and assigns it to a variable with the name given in engine.opts$x.
eng_stan = function(options) {
  code = one_string(options$code)
  opts = options$engine.opts
  ## name of the modelfit object returned by stan_model
  if (is.null(x <- options$output.var)) {
    warning("the option engine.opts$x is deprecated; use the chunk option output.var instead")
    x = opts$x
  }
  if (!is.character(x) || length(x) != 1L) stop(
    "the chunk option output.var must be a character string ",
    "providing a name for the returned `stanmodel` object."
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

  lines = read_utf8(
    options$engine.opts$template %n% system.file('misc', 'tikz2pdf.tex', package = 'knitr')
  )
  # add class options to template
  lines = insert_template(
    lines, '%% TIKZ_CLASSOPTION %%', options$engine.opts$classoption %n% 'tikz', TRUE
  )
  # insert code into preamble
  lines = insert_template(
    lines, '%% EXTRA_TIKZ_PREAMBLE_CODE %%', options$engine.opts$extra.preamble, TRUE
  )
  # insert tikz code into the tex template
  s = insert_template(lines, '%% TIKZ_CODE %%', options$code)
  write_utf8(s, texf <- wd_tempfile('tikz', '.tex'))
  on.exit(unlink(texf), add = TRUE)

  ext = tolower(options$fig.ext %n% dev2ext(options$dev))

  to_svg = ext == 'svg'
  outf = if (to_svg) tinytex::latexmk(texf, 'latex') else tinytex::latexmk(texf)

  fig = fig_path(if (to_svg) '.dvi' else '.pdf', options)
  dir.create(dirname(fig), recursive = TRUE, showWarnings = FALSE)
  file.rename(outf, fig)

  fig2 = with_ext(fig, ext)
  if (to_svg) {
    # dvisvgm needs to be on the path
    # dvisvgm for windows needs ghostscript bin dir on the path also
    if (Sys.which('dvisvgm') == '') tinytex::tlmgr_install('dvisvgm')
    if (system2('dvisvgm', c(
      options$engine.opts$dvisvgm.opts, '-o', shQuote(fig2), fig
    )) != 0) stop('Failed to compile ', fig, ' to ', fig2)
  } else {
    # convert to the desired output-format using magick
    if (ext != 'pdf') magick::image_write(do.call(magick::image_convert, c(
      list(magick::image_read_pdf(fig), ext), options$engine.opts$convert.opts
    )), fig2)
  }
  fig = fig2

  options$fig.num = 1L; options$fig.cur = 1L
  extra = run_hook_plot(fig, options)
  options$engine = 'tex'  # for output hooks to use the correct language class
  engine_output(options, options$code, '', extra)
}

## GraphViz (dot) and Asymptote are similar
eng_dot = function(options) {

  # write code to a temp file, and output to another temp file
  f = wd_tempfile('code'); f2 = wd_tempfile('out')
  write_utf8(code <- options$code, f)
  on.exit(unlink(c(f, f2)), add = TRUE)

  # adapt command to either graphviz or asymptote
  if (options$engine == 'dot') {
    command_string = '%s %s -T%s -o%s'
    syntax         = 'dot'
  } else if (options$engine == 'asy') {
    command_string = '%s %s -f %s -o %s'
    syntax         = 'cpp'  # use cpp syntax for syntax highlighting
  }

  # prepare system command
  cmd = sprintf(
    command_string, shQuote(get_engine_path(options$engine.path, options$engine)),
    shQuote(f), ext <- options$fig.ext %n% dev2ext(options$dev),
    shQuote(f2 <- paste0(f2, '.', ext))
  )

  # generate output
  outf = paste(fig_path(), ext, sep = '.')
  dir.create(dirname(outf), recursive = TRUE, showWarnings = FALSE)
  unlink(outf)
  extra = if (options$eval) {
    if (options$message) message('running: ', cmd)
    system(cmd)
    file.rename(f2, outf)
    if (!file.exists(outf)) stop('Failed to compile the ', options$engine, ' chunk')
    options$fig.num = 1L; options$fig.cur = 1L
    run_hook_plot(outf, options)
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
  cat2 = function(..., file = '', sep = '\n', lang = NULL) {
    # do not write to stdout like the default behavior of cat()
    if (!identical(file, '')) cat(..., file = file, sep = sep)
  }
  if (options$eval)
    do.call(cat2, c(list(options$code), options$engine.opts))

  if (is.null(lang <- options$engine.opts$lang) && is.null(lang <- options$class.source))
    return('')
  # Use engine to set the attribute
  options$engine = lang[1]
  options$class.source = setdiff(options$class.source, lang[1])
  engine_output(options, options$code, NULL)
}

## output the code without processing it
eng_asis = function(options) {
  if (options$echo) one_string(options$code)
}

# write a block environment according to the output format
eng_block = function(options) {
  if (isFALSE(options$echo)) return()
  code = one_string(options$code)
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
  if (is_pandoc) code = pandoc_fragment(code, if (to == 'html') 'html4' else to)
  l1 = options$latex.options
  if (is.null(l1)) l1 = ''
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
    latex = sprintf('\\begin{%s}%s\n%s\n\\end{%s}', type, l1, code, type),
    html =  sprintf('%s<%s class="%s">%s</%s>%s', h3, h2, type, code, h2, h4),
    code
  )
}

eng_block2 = function(options) {
  if (isFALSE(options$echo)) return()

  code = one_string(options$code); type = options$type
  if (is.null(type)) return(code)

  if (is.null(pandoc_to())) stop('The engine "block2" is for R Markdown only')

  l1 = options$latex.options
  if (is.null(l1)) l1 = ''
  # protect environment options because Pandoc may escape the characters like
  # {}; when encoded in integers, they won't be escaped, but will need to
  # restore them later; see bookdown:::restore_block2
  if (l1 != '') l1 = paste(
    c('\\iffalse{', utf8ToInt(enc2utf8(l1)), '}\\fi{}'), collapse = '-'
  )
  h2 = options$html.tag %n% 'div'
  h3 = options$html.before %n% ''
  h4 = options$html.after %n% ''
  h5 = options$html.before2 %n% ''
  h6 = options$html.after2 %n% ''
  if (is_latex_output()) {
    h7 = h8 = '\n'
  } else {
    h7 = sprintf('<%s class="%s">', h2, type)
    h8 = sprintf('</%s>', h2)
  }

  sprintf(
    '\\BeginKnitrBlock{%s}%s%s%s%s%s%s%s%s\\EndKnitrBlock{%s}',
    type, l1, h3, h7, h5, code, h6, h8, h4, type
  )
}

# helper to create engines the wrap embedded html assets (e.g. css,js)
eng_html_asset = function(prefix, postfix) {
  function(options) {
    out = if (options$eval && is_html_output(excludes = 'markdown')) {
      one_string(c(prefix, options$code, postfix))
    }
    options$results = 'asis'
    engine_output(options, options$code, out)
  }
}

# include js in a script tag (ignore if not html output)
eng_js = eng_html_asset('<script type="text/javascript">', '</script>')

# include css in a style tag (ignore if not html output)
eng_css = eng_html_asset('<style type="text/css">', '</style>')

# perform basic sql parsing to determine if a sql query is an update query
is_sql_update_query = function(query) {
  query = one_string(query)
  # remove line comments
  query = gsub('^\\s*--.*\n', '', query)
  # remove multi-line comments
  if (grepl('^\\s*\\/\\*.*', query)) query = gsub('.*\\*\\/', '', query)
  grepl('^\\s*(INSERT|UPDATE|DELETE|CREATE|DROP).*', query, ignore.case = TRUE)
}

# sql engine
eng_sql = function(options) {
  # return chunk before interpolation eagerly to avoid connection option check
  if (isFALSE(options$eval) && !isTRUE(options$sql.show_interpolated)) {
    return(engine_output(options, options$code, ''))
  }

  # Return char vector of sql interpolation param names
  varnames_from_sql = function(conn, sql) {
    varPos = DBI::sqlParseVariables(conn, sql)
    if (length(varPos$start) > 0) {
      varNames = substring(sql, varPos$start, varPos$end)
      sub('^\\?', '', varNames)
    }
  }

  # Vectorized version of exists
  mexists = function(x, env = knit_global(), inherits = TRUE) {
    vapply(x, exists, logical(1), where = env, inherits = inherits)
  }

  # Interpolate a sql query based on the variables in an environment
  interpolate_from_env = function(conn, sql, env = knit_global(), inherits = TRUE) {
    names = unique(varnames_from_sql(conn, sql))
    names_missing = names[!mexists(names, env, inherits)]
    if (length(names_missing) > 0) {
      stop("Object(s) not found: ", paste('"', names_missing, '"', collapse = ", "))
    }

    args = if (length(names) > 0) setNames(
      mget(names, envir = env, inherits = inherits), names
    )

    do.call(DBI::sqlInterpolate, c(list(conn, sql), args))
  }

  # extract options
  conn = options$connection
  if (is.character(conn)) conn = get(conn, envir = knit_global())
  if (is.null(conn)) stop2(
    "The 'connection' option (DBI connection) is required for sql chunks."
  )
  varname = options$output.var
  max.print = options$max.print %n% (opts_knit$get('sql.max.print') %n% 10)
  if (is.na(max.print) || is.null(max.print))
    max.print = -1
  sql = one_string(options$code)
  params = options$params

  query = interpolate_from_env(conn, sql)
  if (isFALSE(options$eval)) return(engine_output(options, query, ''))

  data = tryCatch({
    if (is_sql_update_query(query)) {
      DBI::dbExecute(conn, query)
      NULL
    } else if (is.null(varname) && max.print > 0) {
      # execute query -- when we are printing with an enforced max.print we
      # use dbFetch so as to only pull down the required number of records
      res = DBI::dbSendQuery(conn, query)
      data = DBI::dbFetch(res, n = max.print)
      DBI::dbClearResult(res)
      data

    } else {
      if (length(params) == 0) {
        DBI::dbGetQuery(conn, query)
      } else {
        # If params option is provided, parameters are not interplolated
        DBI::dbGetQuery(conn, sql, params = params)
      }
    }
  }, error = function(e) {
    if (!options$error) stop(e)
    e
  })

  if (inherits(data, "error"))
    return(engine_output(options, query, one_string(data)))

  # create output if needed (we have data and we aren't assigning it to a variable)
  output = if (length(dim(data)) == 2 && ncol(data) > 0 && is.null(varname)) capture.output({

    # apply max.print to data
    display_data = if (max.print == -1) data else head(data, n = max.print)

    # get custom sql print function
    sql.print = opts_knit$get('sql.print')

    # use kable for markdown
    if (!is.null(sql.print)) {
      options$results = 'asis'
      cat(sql.print(data))
    } else if (out_format('markdown')) {

      # we are going to output raw markdown so set results = 'asis'
      options$results = 'asis'

      # force left alignment if the first column is an incremental id column
      first_column = display_data[[1]]
      if (is.numeric(first_column) && length(first_column) > 1 && all(diff(first_column) == 1))
        display_data[[1]] = as.character(first_column)

      # wrap html output in a div so special styling can be applied
      if (is_html_output()) cat('<div class="knitsql-table">\n')

      # determine records caption
      caption = options$tab.cap
      if (is.null(caption)) {
        rows = nrow(data)
        rows_formatted = formatC(rows, format = "d", big.mark = ',')
        caption = if (max.print == -1 || rows < max.print) {
          paste(rows_formatted, "records")
        } else {
          paste("Displaying records 1 -", rows_formatted)
        }
      }
      # disable caption
      if (identical(caption, NA)) caption = NULL

      # print using kable
      print(kable(display_data, caption = caption))

      # terminate div
      if (is_html_output()) cat("\n</div>\n")

      # otherwise use tibble if it's available
    } else if (loadable('tibble')) {
      print(tibble::as_tibble(display_data), n = max.print)

    } else print(display_data) # fallback to standard print
  })
  if (options$results == 'hide') output = NULL

  # assign varname if requested
  if (!is.null(varname)) assign(varname, data, envir = knit_global())

  # reset query to pre-interpolated if not expanding
  if (!isTRUE(options$sql.show_interpolated)) query <- options$code

  # return output
  engine_output(options, query, output)
}

# go engine, added by @hodgesds https://github.com/yihui/knitr/pull/1330
eng_go = function(options) {
  f = wd_tempfile('code', '.go')
  write_utf8(code <- options$code, f)
  on.exit(unlink(f), add = TRUE)
  cmd = get_engine_path(options$engine.path, options$engine)

  fmt_args = sprintf('fmt %s', f)

  tryCatch(
    system2(cmd, fmt_args, stdout = TRUE, stderr = TRUE, env = options$engine.env),
    error = function(e) {
      if (!options$error) stop(e)
    }
  )

  run_args = sprintf(" run %s", f)

  extra = if (options$eval) {
    message('running: ', cmd, run_args)
    tryCatch(
      system2(cmd, run_args, stdout = TRUE, stderr = TRUE, env = options$engine.env),
      error = function(e) {
        if (!options$error) stop(e)
        'Error in executing go code'
      }
    )
  }

  if (options$results == 'hide') extra = NULL

  engine_output(options, code, extra)
}

# SASS / SCSS engine (contributed via https://github.com/yihui/knitr/pull/1666)
#
# Converts SASS / SCSS -> CSS (with same treatments as CSS engine) using either:
# LibSass sass R package (https://github.com/rstudio/sass) when
#   + the package is installed
#   + engine.opts does not set package = FALSE (e.g. engine.opts = list(package = FALSE))
#   + an explicit path to the executable is not provided through engine.path, or
# dart-sass standalone executable (https://sass-lang.com/install) otherwise
#
# CSS output is compressed by default but formatting can be set through style in engine.opts
#  For the sass R package, valid styles are "compressed","expanded", "nested", and "compact"
#  For the executable, valid styles are "compressed" and "expanded"
#  Please refer to respective package / executable documentation for more details
eng_sxss = function(options) {

  # early exit if evaluated output not requested
  options$results = 'asis'
  if (!options$eval) return(engine_output(options, options$code, ''))

  # create temporary file with input code
  f = wd_tempfile('code', paste0('.', options$engine))
  xfun::write_utf8(options$code , f)
  on.exit(unlink(f), add = TRUE)

  # process provided engine options
  package = options$engine.opts$package %n% TRUE
  style = options$engine.opts$style %n% "compressed"
  cmd = get_engine_path(options$engine.path, "sass")

  use_package = loadable("sass") && package && cmd == "sass"

  style = match.arg(style, c("compressed", "expanded", if (use_package) c("compact", "nested")))
  # convert sass/sxss -> css
  if (use_package) {
    message("Converting with the R package sass")

    sass_fun = options$engine.opts$sass_fun %n% sass::sass
    out = tryCatch(
      sass_fun(sass::sass_file(f), options = sass::sass_options(output_style = style)),
      error = function(e) {
        if (!options$error) stop(e)
        warning2(paste('Error in converting to CSS using sass R package:', e, sep = "\n"))
        NULL
      }
    )

    # remove final newline chars from output
    if (!is.null(out)) out = sub("\\n$", "", out)
  } else {
    message("Converting sass with ", cmd)
    style = paste0("--style=", style)

    # attempt execution of sass
    out = tryCatch(
      system2(cmd, args = c(f, style), stdout = TRUE, stderr = TRUE),
      error = function(e) {
        if (!options$error) stop2(e)
        warning2(paste('Error in converting to CSS using executable:', e, sep = "\n"))
        NULL
      }
    )

    # handle execution errors (status codes)
    if (!is.null(attr(out, 'status')) && !options$error) stop2(one_string(out))
  }

  # wrap final output for correct rendering
  final_out = if (!is.null(out) && is_html_output(excludes = 'markdown')) {
    one_string(c('<style type="text/css">', out, '</style>'))
  }

  engine_output(options, options$code, final_out)
}

eng_bslib = function(options) {
  if (!loadable("bslib")) {
    stop2("The 'bslib' package must be installed in order for the knitr engine 'bslib' to work.")
  }
  if (!is.null(options$engine.opts$sass_fun)) {
    stop2("The 'bslib' knitr engine does not allow for customization of the Sass compilation function.")
  }
  func = sass::sass_partial
  formals(func)$bundle = quote(bslib::bs_global_get())
  options$engine.opts$sass_fun = func
  eng_sxss(options)
}

# Target Markdown engine contributed by @wlandau
# Thread: https://github.com/ropensci/targets/issues/503
# Usage: https://books.ropensci.org/targets/markdown.html
# Docs: https://docs.ropensci.org/targets/reference/tar_engine_knitr.html
eng_targets = function(options) {
  targets::tar_engine_knitr(options = options)
}

# a comment engine to return nothing
eng_comment = function(options) {}

## a verbatim engine that returns its chunk content verbatim
eng_verbatim = function(options) {
  if (!out_format('markdown')) {
    warning("The 'verbatim' engine only works for Markdown output at the moment.")
    return(one_string(options$code))
  }

  # change default for the cat engine
  options$eval = FALSE
  # specify the lang name in engine.opts = list(lang = ), or lang/language,
  # or class.source; if all are empty, use 'default'
  options$engine.opts$lang = options$engine.opts$lang %n%
    unlist(options[c('lang', 'language')])[1] %n%
    options$class.source %n% 'default'

  eng_cat(options)
}

# set engines for interpreted languages
local({
  for (i in c(
    'awk', 'bash', 'coffee', 'gawk', 'groovy', 'haskell', 'lein', 'mysql',
    'node', 'octave', 'perl', 'psql', 'Rscript', 'ruby', 'sas',
    'scala', 'sed', 'sh', 'stata', 'zsh'
  )) knit_engines$set(setNames(list(eng_interpreted), i))
})

# additional engines
knit_engines$set(
  asis = eng_asis, asy = eng_dot, block = eng_block, block2 = eng_block2,
  bslib = eng_bslib, c = eng_shlib, cat = eng_cat, cc = eng_shlib,
  comment = eng_comment, css = eng_css, dot = eng_dot, exec = eng_exec,
  fortran = eng_shlib, fortran95 = eng_shlib, go = eng_go,
  highlight = eng_highlight, js = eng_js, julia = eng_julia, python = eng_python,
  R = eng_r, Rcpp = eng_Rcpp, sass = eng_sxss, scss = eng_sxss, sql = eng_sql,
  stan = eng_stan, targets = eng_targets, tikz = eng_tikz, verbatim = eng_verbatim
)

cache_engines$set(python = cache_eng_python)

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

cache_engine = function(options) {
  cache_fun = cache_engines$get(options$engine)
  if (!is.function(cache_fun)) return()
  cache_fun(options)
}

# possible values for engines (for auto-completion in RStudio)
opts_chunk_attr$engine = as.list(sort(c('R', names(knit_engines$get()))))
opts_chunk_attr[c('engine.path', 'engine.opts')] = list('character', 'character')
