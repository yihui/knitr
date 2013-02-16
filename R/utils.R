## copy objects in one environment to the other
copy_env = function(from, to, keys = ls(envir = from, all.names = TRUE)) {
  if (identical(from, to)) return()
  for (i in keys) assign(i, get(i, envir = from, inherits = FALSE), envir = to)
}


knit_counter = function(init = 0L) {
  n = init
  function(reset = FALSE) {
    if (reset) return(n <<- init)
    n <<- n + 1L
    n - 1L
  }
}

plot_counter = knit_counter(1L)
chunk_counter = knit_counter(1L)

## a vectorized and better version than evaluate:::line_prompt
line_prompt = function(x, prompt = getOption('prompt'), continue = getOption('continue')) {
  # match a \n, then followed by any character (use zero width assertion)
  str_c(prompt, gsub('(?<=\n)(?=.|\n)', continue, x, perl = TRUE))
}

## add a prefix to output
comment_out = function(x, prefix = '##', which = TRUE, newline = TRUE) {
  x = gsub('[\n]{2,}$', '\n', x)
  if (newline) x = gsub('([^\n])$', '\\1\n', x)  # add \n if not exists
  if (is.null(prefix) || !nzchar(prefix) || is.na(prefix)) return(x)
  prefix = str_c(prefix, ' ')
  x = gsub(' +([\n]*)$', '\\1', x)
  x[which] = line_prompt(x[which], prompt = prefix, continue = prefix)
  x
}

## assign string in comments to a global variable
comment_to_var = function(x, varname, pattern, envir) {
  if (str_detect(x, pattern)) {
    assign(varname, str_replace(x, pattern, ''), envir = envir)
    return(TRUE)
  }
  FALSE
}

is_blank = function(x) {
  if (length(x)) all(str_detect(x, '^\\s*$')) else TRUE
}
valid_path = function(prefix, label) {
  if (length(prefix) == 0L || is.na(prefix) || prefix == 'NA') prefix = ''
  str_c(prefix, label)
}

## define a color variable in TeX
color_def = function(col, variable = 'shadecolor') {
  x = if (length(col) == 1L) sc_split(col) else col
  if ((n <- length(x)) != 3L) {
    if (n == 1L) x = drop(col2rgb(x)/255) else {
      x = switch(variable, shadecolor = rep(.97, 3), fgcolor = rep(0, 3))
      warning("the color '", col, "' is invalid;",
              'using default color...',
              'see http://yihui.name/knitr/options')
    }
  }
  if (length(x) != 3L) stop('invalid color:', col)
  if (is.numeric(x)) x = round(x, 3L)
  outdec = options(OutDec = '.'); on.exit(options(outdec))
  sprintf('\\definecolor{%s}{rgb}{%s, %s, %s}', variable, x[1], x[2], x[3])
}

## split by semicolon or colon
sc_split = function(string) {
  if (is.call(string)) string = eval(string)
  if (is.numeric(string) || length(string) > 1L) return(string)
  str_trim(str_split(string, ';|,')[[1]])
}

## extract LaTeX packages for tikzDevice
set_preamble = function(input, patterns = knit_patterns$get()) {
  if (!out_format('latex')) return()
  if (length(db <- patterns$document.begin) != 1L) return()  # no \begin{document} pattern
  if (length(hb <- patterns$header.begin) != 1L) return()  # no \documentclass{} pattern
  idx2 = str_detect(input, db)
  if (!any(idx2)) return()
  if ((idx2 <- which(idx2)[1]) < 2L) return()
  txt = str_c(input[seq_len(idx2 - 1L)], collapse = '\n')  # rough preamble
  idx = str_locate(txt, hb)  # locate documentclass
  if (any(is.na(idx))) return()
  options(tikzDocumentDeclaration = str_sub(txt, idx[, 1L], idx[, 2L]))
  preamble = pure_preamble(split_lines(str_sub(txt, idx[, 2L] + 1L)), patterns)
  .knitEnv$tikzPackages = c(.header.sweave.cmd, preamble, '\n')
}
## filter out code chunks from preamble if they exist (they do in LyX/Sweave)
pure_preamble = function(preamble, patterns) {
  res = split_file(lines = preamble, set.preamble = FALSE, patterns) # should avoid recursion
  if (!parent_mode()) {
    ## when not in parent mode, just return normal texts and skip code
    return(unlist(res))
  }
  owd = setwd(input_dir()); on.exit(setwd(owd))
  progress = opts_knit$get('progress')  # suppress printing of blocks and texts
  opts_knit$set(progress = FALSE); on.exit(opts_knit$set(progress = progress), add = TRUE)
  ## run the code in the preamble
  sapply(res, if (opts_knit$get('tangle')) process_tangle else process_group)
}

#' Specify the parent document of child documents
#'
#' This function extracts the LaTeX preamble of the parent document to use for
#' the child document, so that the child document can be compiled as an
#' individual document.
#'
#' When the preamble of the parent document also contains code chunks and inline
#' R code, they will be evaluated as if they were in this child document. For
#' examples, when \pkg{knitr} hooks or other options are set in the preamble of
#' the parent document, it will apply to the child document as well.
#' @param parent path to the parent document (relative to the current child
#'   document)
#' @return The preamble is extracted and stored to be used later when the
#'   complete output is written.
#' @note Obviously this function is only useful when the output format is LaTeX.
#'   This function only works when the child document is compiled in a
#'   standalone mode using \code{\link{knit}()} (instead of being called in
#'   \code{\link{knit_child}()}); when the parent document is compiled, this
#'   function in the child document will be ignored.
#' @references \url{http://yihui.name/knitr/demo/child/}
#' @export
#' @examples ## can use, e.g. \Sexpr{set_parent('parent_doc.Rnw')} or
#'
#' # <<setup-child, include=FALSE>>=
#'
#' # set_parent('parent_doc.Rnw')
#'
#' # @@
set_parent = function(parent) {
  if (child_mode()) return(invisible(NULL)) # quit if in child mode
  opts_knit$set(parent = TRUE)
  set_preamble(readLines(parent, warn = FALSE))
  invisible(NULL)
}

## whether to write results as-is?
output_asis = function(x, options) {
  is_blank(x) || options$results == 'asis'
}

## path relative to dir of the input file
input_dir = function() .knitEnv$input.dir %n% '.'

## scientific notation in TeX, HTML and reST
format_sci_one = function(x, format = 'latex') {

  if (!is.double(x) || is.na(x) || x == 0) return(as.character(x))

  if (abs(lx <- floor(log10(abs(x)))) < getOption('scipen') + 4L)
    return(as.character(round(x, getOption('digits')))) # no need sci notation

  b = round(x/10^lx, getOption('digits'))
  b[b %in% c(1, -1)] = ''

  switch(format, latex = {
    s = sci_notation('%s%s10^{%s}', b, '\\times ', lx)
    sprintf('\\ensuremath{%s}', s)
  }, html = sci_notation('%s%s10<sup>%s</sup>', b, ' &times; ', lx), rst = {
    # if AsIs, use the :math: directive
    if (inherits(x, 'AsIs')) {
      s = sci_notation('%s%s10^{%s}', b, '\\times ', lx)
      sprintf(':math:`%s`', s)
    } else {
      # This needs the following line at the top of the file to define |times|
      # .. include <isonum.txt>
      sci_notation('%s%s10 :sup:`%s`', b, ' |times| ', lx)
    }
  }, as.character(x))
}

sci_notation = function(format, base, times, power) {
  sprintf(format, base, ifelse(base == '', '', times), power)
}

## vectorized version of format_sci_one()
format_sci = function(x, ...) {
  vapply(x, format_sci_one, character(1L), ..., USE.NAMES = FALSE)
}

## absolute path?
is_abs_path = function(x) {
  if (.Platform$OS.type == 'windows')
    grepl(':', x, fixed = TRUE) || grepl('^\\\\', x) else grepl('^[/~]', x)
}

## is tikz device without externalization?
is_tikz_dev = function(options) {
  'tikz' %in% options$dev && !options$external
}

tikz_dict = function(path) {
  str_c(file_path_sans_ext(basename(path)), '-tikzDictionary')
}

## compatibility with Sweave and old beta versions of knitr
fix_options = function(options) {
  # if you want to use subfloats, fig.show must be 'hold'
  if (length(options$fig.subcap)) options$fig.show = 'hold'

  ## deal with aliases: a1 is real option; a0 is alias
  if (length(a1 <- opts_knit$get('aliases')) && length(a0 <- names(a1))) {
    for (i in seq_along(a1)) {
      # use alias only if the name exists in options
      if (a0[i] %in% names(options)) options[[a1[i]]] = options[[a0[i]]]
    }
  }

  options
}

## try eval an option (character) to its value
eval_opt = function(x) {
  if (!is.character(x)) return(x)
  eval(parse(text = x), envir = knit_global())
}

## eval options as symbol/language objects
eval_lang = function(x, envir = knit_global()) {
  if (!is.symbol(x) && !is.language(x)) return(x)
  eval(x, envir = envir)
}

## counterpart of isTRUE()
isFALSE = function(x) identical(x, FALSE)

## check latex packages; if not exist, copy them over to ./
test_latex_pkg = function(name, path) {
  res = try(system(sprintf('kpsewhich %s.sty', name), intern = TRUE), silent = TRUE)
  if (inherits(res, 'try-error') || !length(res)) {
    warning("unable to find LaTeX package '", name, "'; will use a copy from knitr")
    file.copy(path, '.')
  }
}

## get child and parent mode
child_mode = function() opts_knit$get('child')
parent_mode = function() opts_knit$get('parent')

# return the output format, or if current format is in specified formats
out_format = function(x) {
  fmt = opts_knit$get('out.format')
  if (missing(x)) fmt else !is.null(fmt) && (fmt %in% x)
}

#' Path for figure files
#'
#' The filename of figure files is the combination of options \code{fig.path}
#' and \code{label}. This function returns the path of figures for the current
#' chunk by default.
#' @param suffix a suffix of the filename
#' @param options a list of options; by default the options of the current chunk
#' @return A character string (path)
#' @note When there are multiple figures in a chunk, this function only provides
#'   a prefix of the filenames by default, and the actual filenames are of the
#'   form \file{prefix1}, \file{prefix2}, ... where \file{prefix} is the string
#'   returned by this function.
#'
#'   When there are special characters (not alphanumeric or \samp{-} or
#'   \samp{_}) in the path, they will be automatically replaced with \samp{_}.
#'   For example, \file{a b/c.d-} will be sanitized to \file{a_b/c_d-}. This
#'   makes the filenames safe to LaTeX.
#' @export
#' @examples fig_path('.pdf', list(fig.path='figure/abc-', label='first-plot'))
#' fig_path(1:10, list(fig.path='foo-', label='bar'))
fig_path = function(suffix = '', options = opts_current$get()) {
  path = valid_path(options$fig.path, options$label)
  if (!out_format(c('latex', 'sweave', 'listings'))) return(str_c(path, suffix))
  # sanitize filename for LaTeX
  if (str_detect(path, '[^-_./\\[:alnum:]]')) {
    warning('replaced special characters in figure filename "', path, '" -> "',
            path <- str_replace_all(path, '[^-_./\\[:alnum:]]', '_'), '"')
  }
  # replace . with _ except ../ and ./
  s = str_split(path, '[/\\]')[[1L]]
  i = (s != '.') & (s != '..') & str_detect(s, '\\.')
  if (any(i)) {
    s[i] = str_replace_all(s[i], '\\.', '_')
    path = str_c(s, collapse = '/')
    warning('dots in figure paths replaced with _ ("', path, '")')
  }
  str_c(path, suffix)
}

#' The global environment in which code chunks are evaluated
#'
#' This function makes the environment of a code chunk accessible inside a
#' chunk.
#'
#' It returns the \code{envir} argument of \code{\link{knit}}, e.g. if we call
#' \code{\link{knit}()} in the global environment, \code{knit_global()} returns
#' R's global environment by default. You can call functions like
#' \code{\link{ls}()} on this environment.
#' @export
knit_global = function() {
  .knitEnv$knit_global
}
# current environment for knitr's code chunks
knit_env = function() {
  .knitEnv$knit_env
}

# Indents a Block
#  Input
#     "library(ggplot2)\nqplot(wt, mpg, data = mtcars)"
#  Output
#          library(ggplot2)
#          qplot(wt, mpg, data  = mtcars)
indent_block = function(block, spaces = '    ') {
  if (is.null(block) || !nzchar(block)) return(spaces)
  line_prompt(block, spaces, spaces)
}

# print knitr logs
print_knitlog = function() {
  if (!opts_knit$get('verbose') || child_mode() || !length(klog <- knit_log$get()))
    return()
  for (i in unlist(klog, use.names = FALSE)) {
    cat(i, '\n\n')
    cat(knit_code$get(sub('^Chunk ([^:]+):\n.*', '\\1', i)), sep = '\n')
    cat('\n')
  }
  cat('\nNumber of messages:\n')
  print(sapply(klog, length))
}

# count the number of lines
line_count = function(x) str_count(x, '\n') + 1L

# faster than require() but less rigorous
has_package = function(pkg) pkg %in% .packages(TRUE)

# if LHS is NULL, return the RHS
`%n%` = function(x, y) if (is.null(x)) y else x

# merge elements of y into x with the same names
merge_list = function(x, y) {
  x[names(y)] = y
  x
}

# paths of all figures
all_figs = function(options, ext = options$fig.ext, num = options$fig.num) {
  fig_path(paste(if (num == 1L) '' else seq_len(num),
                 '.', ext, sep = ''), options)
}

# evaluate an expression in a diretory and restore wd after that
in_dir = function(dir, expr) {
  if (!is.null(dir)) {
    owd = setwd(dir); on.exit(setwd(owd))
  }
  expr
}

# escape special LaTeX characters
escape_latex = function(x, newlines = FALSE, spaces = FALSE) {
  x = gsub('\\\\', '\\\\textbackslash', x)
  x = gsub('([#$%&_{}])', '\\\\\\1', x)
  x = gsub('\\\\textbackslash([^{]|$)', '\\\\textbackslash{}\\1', x)
  x = gsub('~', '\\\\textasciitilde{}', x)
  x = gsub('\\^', '\\\\textasciicircum{}', x)
  if (newlines) x = gsub('(?<!\n)\n(?!\n)', '\\\\\\\\', x, perl = TRUE)
  if (spaces) x = gsub('  ', '\\\\ \\\\ ', x)
  x
}

# escape special HTML chars
escape_html = function(x) {
  x = gsub('&', '&amp;', x)
  x = gsub('<', '&lt;', x)
  x = gsub('>', '&gt;', x)
  x = gsub('"', '&quot;', x)
  x
}

#' Read source code from R-Forge
#'
#' This function reads source code from the SVN repositories on R-Forge.
#' @param path relative path to the source script on R-Forge
#' @param project name of the R-Forge project
#' @param extra extra parameters to be passed to the URL (e.g. \code{extra =
#'   '&revision=48'} to check out the source of revision 48)
#' @param base the base URL
#' @return A character vector of the source code.
#' @author Yihui Xie and Peter Ruckdeschel
#' @export
#' @examples \dontrun{read_rforge('rgl/R/axes.R', project = 'rgl')
#' read_rforge('rgl/R/axes.R', project = 'rgl', extra='&revision=519')}
read_rforge = function(path, project, extra = '',
                      base = 'http://r-forge.r-project.org/scm/viewvc.php/*checkout*/pkg') {
  readLines(sprintf('%s/%s?root=%s%s', base, path, project, extra))
}

# because I think strsplit('', 'foo') should return '' instead of character(0)
split_lines = function(x) {
  if (length(grep('\n', x)) == 0L) return(x)
  con = textConnection(x)
  on.exit(close(con))
  readLines(con)
}

# if a string has explicit encoding, convert it to native encoding
native_encode = function(x, to = '') {
  enc = Encoding(x)
  idx = enc != 'unknown'
  if (to == '') {
    x2 = x
    if (any(idx)) x2[idx] = iconv(x2[idx], enc[idx][1L], to)
    if (any(is.na(x2))) {
      warning('some characters may not work under the current locale')
    } else x = x2  # use conversion only if it succeeds
  } else x = iconv(x, if (any(idx)) enc[idx][1L] else '', to)
  x
}

# make the encoding case-insensitive, e.g. LyX uses ISO-8859-15 but R uses iso-8859-15
correct_encode = function(encoding) {
  if (encoding == 'native.enc' || encoding == '' || encoding == localeToCharset())
    return('')
  if (is.na(idx <- match(tolower(encoding), tolower(iconvlist())))) {
    warning('encoding "', encoding, '" not supported; using the native encoding instead')
    ''
  } else iconvlist()[idx]
}
