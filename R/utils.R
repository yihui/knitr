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
  paste(prompt, gsub('(?<=\n)(?=.|\n)', continue, x, perl = TRUE), sep = '')
}

## add a prefix to output
comment_out = function(x, prefix = '##', which = TRUE, newline = TRUE) {
  x = gsub('[\n]{2,}$', '\n', x)
  if (newline) x = gsub('([^\n])$', '\\1\n', x)  # add \n if not exists
  if (is.null(prefix) || !nzchar(prefix) || is.na(prefix)) return(x)
  prefix = paste(prefix, '')
  x = gsub(' +([\n]*)$', '\\1', x)
  x[which] = line_prompt(x[which], prompt = prefix, continue = prefix)
  x
}

## assign string in comments to a global variable
comment_to_var = function(x, varname, pattern, envir) {
  if (grepl(pattern, x)) {
    assign(varname, sub(pattern, '', x), envir = envir)
    return(TRUE)
  }
  FALSE
}

is_blank = function(x) {
  if (length(x)) all(grepl('^\\s*$', x)) else TRUE
}
valid_path = function(prefix, label) {
  if (length(prefix) == 0L || is.na(prefix) || prefix == 'NA') prefix = ''
  paste(prefix, label, sep = '')
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
  idx2 = grepl(db, input)
  if (!any(idx2)) return()
  if ((idx2 <- which(idx2)[1]) < 2L) return()
  txt = paste(input[seq_len(idx2 - 1L)], collapse = '\n')  # rough preamble
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

  if (!is.numeric(x) || !is.double(x) || is.na(x) || x == 0) return(as.character(x))

  if (abs(lx <- floor(log10(abs(x)))) < getOption('scipen') + 4L)
    return(as.character(round(x, getOption('digits')))) # no need sci notation

  b = round(x/10^lx, getOption('digits'))
  b[b %in% c(1, -1)] = ''

  switch(format, latex = {
    sci_notation('%s%s10^{%s}', b, '\\times ', lx)
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
  paste(sans_ext(basename(path)), 'tikzDictionary', sep = '-')
}

## compatibility with Sweave and old beta versions of knitr
fix_options = function(options) {
  # if you want to use subfloats, fig.show must be 'hold'
  if (length(options$fig.subcap)) options$fig.show = 'hold'

  # cache=TRUE -> 3; FALSE -> 0
  if (is.logical(options$cache)) options$cache = options$cache * 3
  # non-R code should not use cache=1,2
  if (options$engine != 'R') options$cache = (options$cache > 0) * 3

  ## deal with aliases: a1 is real option; a0 is alias
  if (length(a1 <- opts_knit$get('aliases')) && length(a0 <- names(a1))) {
    for (i in seq_along(a1)) {
      # use alias only if the name exists in options
      if (a0[i] %in% names(options)) options[[a1[i]]] = options[[a0[i]]]
    }
  }

  options
}

# parse but do not keep source
parse_only = formatR:::parse_only

## eval options as symbol/language objects
eval_lang = function(x, envir = knit_global()) {
  if (!is.symbol(x) && !is.language(x)) return(x)
  eval(x, envir = envir)
}

## counterpart of isTRUE()
isFALSE = function(x) identical(x, FALSE)

## check latex packages; if not exist, copy them over to ./
test_latex_pkg = function(name, path) {
  res = try(system(sprintf('%s %s.sty', kpsewhich(), name), intern = TRUE), silent = TRUE)
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
  (if (out_format(c('latex', 'sweave', 'listings', 'markdown'))) sanitize_fn else
    str_c)(path, suffix)
}
# sanitize filename for LaTeX
sanitize_fn = function(path, suffix = '') {
  if (grepl('[^~:_./\\[:alnum:]-]', path)) {
    warning('replaced special characters in figure filename "', path, '" -> "',
            path <- gsub('[^~:_./\\[:alnum:]-]', '_', path), '"')
  }
  # replace . with _ except ../ and ./
  s = str_split(path, '[/\\]')[[1L]]
  i = (s != '.') & (s != '..') & grepl('\\.', s)
  if (any(i)) {
    s[i] = gsub('\\.', '_', s[i])
    path = paste(s, collapse = '/')
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
  .knitEnv$knit_global %n% globalenv()
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
  x = gsub('\\\\textbackslash', '\\\\textbackslash{}', x)
  x = gsub('~', '\\\\textasciitilde{}', x)
  x = gsub('\\^', '\\\\textasciicircum{}', x)
  if (newlines) x = gsub('(?<!\n)\n(?!\n)', '\\\\\\\\', x, perl = TRUE)
  if (spaces) x = gsub('  ', '\\\\ \\\\ ', x)
  x
}

# escape special HTML chars
escape_html = highr:::escape_html

#' Read source code from R-Forge
#'
#' This function reads source code from the SVN repositories on R-Forge.
#' @param path relative path to the source script on R-Forge
#' @param project name of the R-Forge project
#' @param extra extra parameters to be passed to the URL (e.g. \code{extra =
#'   '&revision=48'} to check out the source of revision 48)
#' @return A character vector of the source code.
#' @author Yihui Xie and Peter Ruckdeschel
#' @export
#' @examples \dontrun{read_rforge('rgl/R/axes.R', project = 'rgl')
#' read_rforge('rgl/R/axes.R', project = 'rgl', extra='&revision=519')}
read_rforge = function(path, project, extra = '') {
  base = 'http://r-forge.r-project.org/scm/viewvc.php/*checkout*/pkg'
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
  if (encoding == 'native.enc' || encoding == '') return('')
  lcc = localeToCharset()[1L]
  if (!is.na(lcc) && encoding == lcc) return('')
  if (is.na(idx <- match(tolower(encoding), tolower(iconvlist())))) {
    warning('encoding "', encoding, '" not supported; using the native encoding instead')
    ''
  } else iconvlist()[idx]
}

# import functions from tools
file_ext = tools::file_ext
sans_ext = tools::file_path_sans_ext
# substitute extension
sub_ext = function(x, ext) {
  if (grepl('\\.([[:alnum:]]+)$', x)) x = sans_ext(x)
  paste(x, ext, sep = '.')
}

#' Wrap long lines in Rmd files
#'
#' This function wraps long paragraphs in an R Markdown file. Other elements are
#' not wrapped: the YAML preamble, fenced code blocks, section headers and
#' indented elements. The main reason for wrapping long lines is to make it
#' easier to review differences in version control.
#' @param file the input Rmd file
#' @param width the expected line width
#' @param text an alternative to \code{file} to input the text lines
#' @param backup the path to back up the original file (in case anything goes
#'   wrong); if \code{NULL}, it is ignored; by default it is constructed from
#'   \code{file} by adding \code{__} before the base filename
#' @return If \code{file} is provided, it is overwritten; if \code{text} is
#'   provided, a character vector is returned.
#' @note Currently it does not wrap blockquotes or lists (ordered or unordered).
#'   This feature may or may not be added in the future.
#' @export
#' @examples wrap_rmd(text = c('```', '1+1', '```', '- a list item', '> a quote', '',
#' paste(rep('this is a normal paragraph', 5), collapse = ' ')))
wrap_rmd = function(file, width = 80, text = NULL, backup) {
  x = if (is.null(text)) readLines(file, warn = FALSE) else split_lines(text)
  x = strip_white(x)  # strip blank lines in the beginning and end
  if ((n <- length(x)) <= 1L) return(x)  # are you kidding?
  idx = NULL  # collect the lines to exclude from wrapping
  i = grep('^---$', x)  # yaml preamble
  if (length(i) > 1 && i[1L] == 1L) idx = c(idx, i[1L]:i[2L])
  i = grep('^(```|\\{% (end|)highlight [a-z ]*%\\}|</?script.*>)', x)  # code blocks
  if (length(i)) {
    if (length(i) %% 2L != 0L) stop('markers for code blocks must be paired up')
    idx = c(idx, unlist(apply(matrix(i, ncol = 2L, byrow = TRUE), 1L,
                              function(z) z[1L]:z[2L])))
  }
  # section headers, indented code blocks and latex math
  idx = c(idx, grep('^(#|===|---|    |\t)', x))
  # blank lines
  idx = c(idx, grep('^\\s*$', x))
  # TODO: this is naive -- I treat a line as (a part of) a normal paragraph if
  # it does not start with a space, or > (blockquotes) or -, *, 1. (lists), and
  # only wrap paragraphs
  idx = c(idx, grep('^\\s*( |> |- |\\* |\\d+ )', x))
  idx = unique(idx)
  if (length(idx) == n) return(x)  # no need to wrap anything

  i = logical(n); i[idx] = TRUE; r = rle(i)
  n = length(r$lengths); txt = vector('list', n); j = c(0L, cumsum(r$lengths))
  for (i in seq_len(n)) {
    block = x[seq(j[i] + 1L, j[i + 1])]
    txt[[i]] = if (r$value[i]) {
      # those lines not to be wrapped
      gsub('\\s+$', '', block)  # strip pending spaces
    } else {
      strwrap(paste(block, collapse = '\n'), width)
    }
  }
  txt = unlist(txt)
  if (is.null(text)) {
    if (missing(backup)) backup = file.path(dirname(file), paste0('__', basename(file)))
    if (!is.null(backup)) file.copy(file, backup, overwrite = TRUE)
    writeLines(txt, file)
  } else txt
}

# change the default device to an appropriate device when the output is html
# (e.g. markdown, reST, AsciiDoc)
set_html_dev = function() {
  # only change if device is pdf, because pdf does not work for html
  if (!identical(opts_chunk$get('dev'), 'pdf')) return()
  # in some cases, png() does not work (e.g. options('bitmapType') == 'Xlib' on
  # headless servers); use svg then
  opts_chunk$set(dev = if (inherits(try({
    png(tempfile()); dev.off()
  }, silent = TRUE), 'try-error')) 'svg' else 'png')
}

# locate kpsewhich especially for Mac OS because /usr/texbin may not be in PATH
kpsewhich = function() {
  if (Sys.info()['sysname'] != 'Darwin' || !file.exists(x <- '/usr/texbin/kpsewhich')
      || nzchar(Sys.which('kpsewhich')))
    'kpsewhich' else x
}
