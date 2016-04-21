# copy objects in one environment to the other
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
shot_counter = knit_counter(1L)
chunk_counter = knit_counter(1L)

# a vectorized and better version than evaluate:::line_prompt
line_prompt = function(x, prompt = getOption('prompt'), continue = getOption('continue')) {
  # match a \n, then followed by any character (use zero width assertion)
  paste0(prompt, gsub('(?<=\n)(?=.|\n)', continue, x, perl = TRUE))
}

# add a prefix to output
comment_out = function(x, prefix = '##', which = TRUE, newline = TRUE) {
  x = gsub('[\n]{2,}$', '\n', x)
  if (newline) x = gsub('([^\n])$', '\\1\n', x)  # add \n if not exists
  if (is.null(prefix) || !nzchar(prefix) || is.na(prefix)) return(x)
  prefix = paste(prefix, '')
  x = gsub(' +([\n]*)$', '\\1', x)
  x[which] = line_prompt(x[which], prompt = prefix, continue = prefix)
  x
}

# assign string in comments to a global variable
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
  paste0(prefix, label)
}

# define a color variable in TeX
color_def = function(col, variable = 'shadecolor') {
  if (is.na(col)) return('')  # no LaTeX code when color is NA
  x = if (length(col) == 1L) sc_split(col) else col
  if ((n <- length(x)) != 3L) {
    if (n == 1L) x = drop(col2rgb(x) / 255) else {
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

# split by semicolon or colon
sc_split = function(string) {
  if (is.call(string)) string = eval(string)
  if (is.numeric(string) || length(string) > 1L) return(string)
  stringr::str_trim(stringr::str_split(string, ';|,')[[1]])
}

# extract LaTeX packages for tikzDevice
set_preamble = function(input, patterns = knit_patterns$get()) {
  if (!out_format('latex')) return()
  .knitEnv$tikzPackages = .knitEnv$bibliography = NULL
  if (length(db <- patterns$document.begin) != 1L) return()  # no \begin{document} pattern
  if (length(hb <- patterns$header.begin) != 1L) return()  # no \documentclass{} pattern
  idx2 = grep(db, input)[1]
  if (is.na(idx2) || idx2 < 2L) return()
  idx1 = grep(hb, input)[1]
  if (is.na(idx1) || idx1 >= idx2) return()
  txt = paste(input[idx1:(idx2 - 1L)], collapse = '\n')  # rough preamble
  idx = stringr::str_locate(txt, hb)  # locate documentclass
  options(tikzDocumentDeclaration = stringr::str_sub(txt, idx[, 1L], idx[, 2L]))
  preamble = pure_preamble(split_lines(stringr::str_sub(txt, idx[, 2L] + 1L)), patterns)
  .knitEnv$tikzPackages = c(.header.sweave.cmd, preamble, '\n')
  .knitEnv$bibliography = grep('^\\\\bibliography.+', input, value = TRUE)
}
# filter out code chunks from preamble if they exist (they do in LyX/Sweave)
pure_preamble = function(preamble, patterns) {
  res = split_file(lines = preamble, set.preamble = FALSE, patterns) # should avoid recursion
  if (!parent_mode()) {
    # when not in parent mode, just return normal texts and skip code
    return(unlist(res))
  }
  owd = setwd(input_dir()); on.exit(setwd(owd))
  progress = opts_knit$get('progress')  # suppress printing of blocks and texts
  opts_knit$set(progress = FALSE); on.exit(opts_knit$set(progress = progress), add = TRUE)
  # run the code in the preamble
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

# whether to write results as-is?
output_asis = function(x, options) {
  is_blank(x) || options$results == 'asis'
}

# path relative to dir of the input file
input_dir = function() {
  # LyX is a special case: the input file is in tempdir, and we should use
  # root.dir as the real input dir (#809)
  (if (is_lyx()) opts_knit$get('root.dir')) %n% .knitEnv$input.dir %n% '.'
}

is_lyx = function() {
  args = commandArgs(TRUE)
  if (length(args) < 4) return(FALSE)
  grepl('[.]Rnw$', args[1]) && !is.na(Sys.getenv('LyXDir', NA))
}

# scientific notation in TeX, HTML and reST
format_sci_one = function(x, format = 'latex') {

  if (!(class(x)[1] == 'numeric') || is.na(x) || x == 0) return(as.character(x))

  if (is.infinite(x)) {
    return(
      switch(format, latex = {
        sprintf("%s\\infty{}", ifelse(x < 0, "-", ""))
      }, html = {
        sprintf("%s&infin;", ifelse(x < 0, "-", ""))
      }, as.character(x)))
  }

  if (abs(lx <- floor(log10(abs(x)))) < getOption('scipen') + 4L)
    return(as.character(round(x, getOption('digits')))) # no need sci notation

  b = round(x / 10^lx, getOption('digits'))
  b[b %in% c(1, -1)] = ''

  switch(format, latex = {
    sci_notation('%s%s10^{%s}', b, '\\times ', lx)
  },
  html = sci_notation('%s%s10<sup>%s</sup>', b, ' &times; ', lx),
  md   = sci_notation('%s%s10^%s^', b, '&times; ', lx),
  rst  = {
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

# vectorized version of format_sci_one()
format_sci = function(x, ...) {
  vapply(x, format_sci_one, character(1L), ..., USE.NAMES = FALSE)
}

# absolute path?
is_abs_path = function(x) {
  if (is_windows())
    grepl(':', x, fixed = TRUE) || grepl('^\\\\', x) else grepl('^[/~]', x)
}

# is tikz device without externalization?
is_tikz_dev = function(options) {
  'tikz' %in% options$dev && !options$external
}

tikz_dict = function(path) {
  paste(sans_ext(basename(path)), 'tikzDictionary', sep = '-')
}

# compatibility with Sweave and old beta versions of knitr
fix_options = function(options) {
  # if you want to use subfloats, fig.show must be 'hold'
  if (length(options$fig.subcap)) options$fig.show = 'hold'
  # the default device NULL is not valid; use pdf is not set
  if (is.null(options$dev)) options$dev = 'pdf'

  # the figure/cache filenames may contain UTF-8 chars, which won't work on
  # Windows, e.g. png() fails if filename contains UTF-8 chars (must use native
  # encoding); in rmarkdown::render(input), basename(input) may implicitly
  # convert the encoding of the filename `input` to UTF-8
  options$fig.path = native_encode(options$fig.path)
  options$cache.path = native_encode(options$cache.path)

  # cache=TRUE -> 3; FALSE -> 0
  if (is.logical(options$cache)) options$cache = options$cache * 3
  # non-R code should not use cache=1,2
  if (options$engine != 'R') options$cache = (options$cache > 0) * 3

  options$eval = unname(options$eval)

  # if aspect ratio is specified, calculate figure height
  if (is.numeric(options$fig.asp)) {
    options$fig.height = options$fig.width * options$fig.asp
  }

  # out.[width|height].px: unit in pixels for sizes
  for (i in c('width', 'height')) {
    options[[sprintf('out.%s.px', i)]] = options[[sprintf('out.%s', i)]] %n%
      (options[[sprintf('fig.%s', i)]] * options$dpi)
  }
  # for Retina displays, increase physical size, and decrease output size
  if (is.numeric(r <- options$fig.retina) && r != 1) {
    if (is.null(options[['out.width']])) {
      options$out.width = options$fig.width * options$dpi
    }
    options$dpi = options$dpi * r
  } else {
    options$fig.retina = 1
  }

  # turn x% to x/100\linewidth
  if (is_latex_output())
    options['out.width'] = list(percent_latex_width(options[['out.width']]))

  # deal with aliases: a1 is real option; a0 is alias
  if (length(a1 <- opts_knit$get('aliases')) && length(a0 <- names(a1))) {
    for (i in seq_along(a1)) {
      # use alias only if the name exists in options
      if (a0[i] %in% names(options)) options[[a1[i]]] = options[[a0[i]]]
    }
  }

  options
}

is_latex_output = function() {
  out_format('latex') || pandoc_to(c('latex', 'beamer'))
}

# turn percent width to LaTeX unit, e.g. out.width = 30% -> .3\linewidth
percent_latex_width = function(x) {
  if (!is.character(x)) return(x)
  i = grep('^[0-9.]+%$', x)
  if (length(i) == 0) return(x)
  xi = as.numeric(sub('%$', '', x[i]))
  if (any(is.na(xi))) return(x)
  x[i] = paste0(xi / 100, '\\linewidth')
  x
}

# parse but do not keep source
parse_only = formatR:::parse_only

# eval options as symbol/language objects
eval_lang = function(x, envir = knit_global()) {
  if (!is.symbol(x) && !is.language(x)) return(x)
  eval(x, envir = envir)
}

# counterpart of isTRUE()
isFALSE = function(x) identical(x, FALSE)

# check latex packages; if not exist, copy them over to ./
test_latex_pkg = function(name, path) {
  res = try_silent(system(sprintf('%s %s.sty', kpsewhich(), name), intern = TRUE))
  if (inherits(res, 'try-error') || !length(res)) {
    warning("unable to find LaTeX package '", name, "'; will use a copy from knitr")
    file.copy(path, '.')
  }
}

# get child and parent mode
child_mode = function() opts_knit$get('child')
parent_mode = function() opts_knit$get('parent')

# return the output format, or if current format is in specified formats
out_format = function(x) {
  fmt = opts_knit$get('out.format')
  if (missing(x)) fmt else !is.null(fmt) && (fmt %in% x)
}

# rmarkdown sets an option for the Pandoc output format from markdown
pandoc_to = function(x) {
  fmt = opts_knit$get('rmarkdown.pandoc.to')
  if (missing(x)) fmt else !is.null(fmt) && (fmt %in% x)
}

# rmarkdown's input format
pandoc_from = function() {
  opts_knit$get('rmarkdown.pandoc.from') %n% 'markdown'
}

pandoc_fragment = function(text, to, from = pandoc_from()) {
  f1 = tempfile('pandoc', '.', '.md'); f2 = tempfile('pandoc', '.')
  on.exit(unlink(c(f1, f2)))
  writeLines(enc2utf8(text), f1, useBytes = TRUE)
  rmarkdown::pandoc_convert(f1, to, from, f2)
  code = readLines(f2, encoding = 'UTF-8', warn = FALSE)
  paste(code, collapse = '\n')
}

#' Path for figure files
#'
#' The filename of figure files is the combination of options \code{fig.path}
#' and \code{label}. This function returns the path of figures for the current
#' chunk by default.
#' @param suffix a suffix of the filename; if it is not empty, nor does it
#'   contain a dot \code{.}, it will be treated as the filename extension (e.g.
#'   \code{png} will be used as \code{.png})
#' @param options a list of options; by default the options of the current chunk
#' @param number the current figure number (by default the internal chunk option
#'   \code{fig.cur} if available)
#' @return A character vector of the form \file{fig.path-label-i.suffix}.
#' @note When there are special characters (not alphanumeric or \samp{-} or
#'   \samp{_}) in the path, they will be automatically replaced with \samp{_}.
#'   For example, \file{a b/c.d-} will be sanitized to \file{a_b/c_d-}. This
#'   makes the filenames safe to LaTeX.
#' @export
#' @examples fig_path('.pdf', options = list(fig.path='figure/abc-', label='first-plot'))
#' fig_path('.png', list(fig.path='foo-', label='bar'), 1:10)
fig_path = function(suffix = '', options = opts_current$get(), number) {
  if (suffix != '' && !grepl('[.]', suffix)) suffix = paste0('.', suffix)
  if (missing(number)) number = options$fig.cur %n% 1L
  if (!is.null(number)) suffix = paste0('-', number, suffix)
  path = valid_path(options$fig.path, options$label)
  (if (out_format(c('latex', 'sweave', 'listings'))) sanitize_fn else
    paste0)(path, suffix)
}
# sanitize filename for LaTeX
sanitize_fn = function(path, suffix = '') {
  if (grepl('[^~:_./\\[:alnum:]-]', path)) {
    warning('replaced special characters in figure filename "', path, '" -> "',
            path <- gsub('[^~:_./\\[:alnum:]-]', '_', path), '"')
  }
  # replace . with _ except ../ and ./
  s = strsplit(path, '[/\\\\]')[[1L]]
  i = (s != '.') & (s != '..') & grepl('\\.', s)
  if (any(i)) {
    s[i] = gsub('\\.', '_', s[i])
    path = paste(s, collapse = '/')
    warning('dots in figure paths replaced with _ ("', path, '")')
  }
  paste0(path, suffix)
}

#' Obtain the figure filenames for a chunk
#'
#' Given a chunk label, the figure file extension, the figure number(s), and the
#' chunk option \code{fig.path}, return the filename(s).
#'
#' This function can be used in an inline R expression to write out the figure
#' filenames without hard-coding them. For example, if you created a plot in a
#' code chunk with the label \code{foo} and figure path \file{my-figure/}, you
#' are not recommended to use hard-coded figure paths like
#' \samp{\includegraphics{my-figure/foo-1.pdf}} (in \file{.Rnw} documents) or
#' \samp{![](my-figure/foo-1.png)} (R Markdown) in your document. Instead, you
#' should use \samp{\\Sexpr{fig_chunk('foo', 'pdf')}} or \samp{![](`r
#' fig_chunk('foo', 'png')`)}.
#'
#' You can generate plots in a code chunk but not show them inside the code
#' chunk by using the chunk option \code{fig.show = 'hide'}. Then you can use
#' this function if you want to show them elsewhere.
#' @param label the chunk label
#' @param ext the figure file extension, e.g. \code{png} or \code{pdf}
#' @param number the figure number (by default \code{1})
#' @param fig.path the chunk option \code{fig.path}
#' @return A character vector of filenames.
#' @export
#' @examples library(knitr)
#' fig_chunk('foo', 'png')
#' fig_chunk('foo', 'pdf')
#' fig_chunk('foo', 'svg', 2)  # the second plot of the chunk foo
#' fig_chunk('foo', 'png', 1:5)  # if the chunk foo produced 5 plots
fig_chunk = function(label, ext = '', number, fig.path = opts_chunk$get('fig.path')) {
  fig_path(ext, list(fig.path = fig.path, label = label), number)
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
  if (is.null(block) || !any(nzchar(block))) return(rep(spaces, length(block)))
  if (spaces == '') return(block)
  line_prompt(block, spaces, spaces)
}

# print knitr logs
print_knitlog = function() {
  if (!opts_knit$get('verbose') || child_mode() || !length(klog <- knit_log$get(drop = FALSE)))
    return()

  for (i in unlist(klog, use.names = FALSE)) {
    cat(sub('\n+$', '', i), '\n\n')
    if (length(code <- knit_code$get(sub('^Chunk ([^:]+):\n.*', '\\1', i))))
      cat(code, sep = '\n')
  }
  cat('\nNumber of messages:\n')
  print(sapply(klog, length))
}

# count the number of lines
line_count = function(x) stringr::str_count(x, '\n') + 1L

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
  fig_path(ext, options, number = seq_len(num))
}

# evaluate an expression in a diretory and restore wd after that
in_dir = function(dir, expr) {
  if (!is.null(dir)) {
    owd = setwd(dir); on.exit(setwd(owd))
  }
  wd1 = getwd()
  res = expr
  wd2 = getwd()
  if (wd1 != wd2) warning(
    'You changed the working directory to ', wd2, ' (probably via setwd()). ',
    'It will be restored to ', wd1, '. See the Note section in ?knitr::knit'
  )
  res
}

# evaluate under the base.dir
in_base_dir = function(expr) {
  d = opts_knit$get('base.dir')
  if (is.character(d) && !file_test('-d', d)) dir.create(d, recursive = TRUE)
  in_dir(d, expr)
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
#' @examples library(knitr)
#' \donttest{# relies on r-forge.r-project.org being accessible
#' read_rforge('rgl/R/axes.R', project = 'rgl')
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

# if a string is encoded in UTF-8, convert it to native encoding
native_encode = function(x, to = '') {
  idx = Encoding(x) == 'UTF-8'
  x2 = iconv(x, if (any(idx)) 'UTF-8' else '', to)
  if (!any(is.na(x2))) return(x2)  # use conversion only if it succeeds
  warning('some characters may not work under the current locale')
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

# re-encode an input file to UTF-8
encode_utf8 = function(input, encoding = getOption('encoding'), output = input) {
  if (encoding == 'UTF-8') {
    if (input != output) file.copy(input, output)
    return()
  }
  con = file(input, encoding = encoding)
  tryCatch(txt <- readLines(con), finally = close(con))
  writeLines(enc2utf8(txt), output, useBytes = TRUE)
}

# import functions from tools
file_ext = tools::file_ext
sans_ext = tools::file_path_sans_ext
# substitute extension
sub_ext = function(x, ext) {
  i = grep('\\.([[:alnum:]]+)$', x)
  x[i] = sans_ext(x[i])
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
  # only change if device has not been set
  if (!is.null(opts_chunk$get('dev'))) return()
  # in some cases, png() does not work (e.g. options('bitmapType') == 'Xlib' on
  # headless servers); use svg then
  opts_chunk$set(dev = if (inherits(try_silent({
    grDevices::png(tempfile()); grDevices::dev.off()
  }), 'try-error')) 'svg' else 'png')
}

# locate kpsewhich especially for Mac OS because /usr/texbin may not be in PATH
kpsewhich = function() {
  if (Sys.info()['sysname'] != 'Darwin' || !file.exists(x <- '/usr/texbin/kpsewhich')
      || nzchar(Sys.which('kpsewhich')))
    'kpsewhich' else x
}

# call try with silent = TRUE
try_silent = function(expr) try(expr, silent = TRUE)

# check if a utility exists; if it does, save its availability in opts_knit
has_utility = function(name, package = name) {
  name2 = paste('util', name, sep = '_')  # e.g. util_pdfcrop
  if (is.logical(yes <- opts_knit$get(name2))) return(yes)
  yes = nzchar(Sys.which(name))
  if (!yes) warning(package, ' not installed or not in PATH')
  opts_knit$set(setNames(list(yes), name2))
  yes
}

is_windows = function() .Platform$OS.type == 'windows'

#' Query the current input filename
#'
#' Returns the name of the input file passed to \code{\link{knit}()}.
#' @param dir whether to prepend the current working directory to the file path
#'   (i.e. return an absolute path or a relative path)
#' @return A character string, if this function is called inside an input
#'   document (otherwise \code{NULL}).
#' @export
current_input = function(dir = FALSE) {
  input = knit_concord$get('infile')
  outwd = opts_knit$get('output.dir')
  if (is.null(input)) return()
  if (dir) {
    if (is.null(outwd)) {
      warning('Cannot determine the directory of the input document')
      dir = FALSE
    }
  }
  if (dir) file.path(outwd, input) else input
}

# import output handlers from evaluate
default_handlers = evaluate:::default_output_handler
# change the value handler in evaluate default handlers
knit_handlers = function(fun, options) {
  if (!is.function(fun)) fun = knit_print
  if (length(formals(fun)) < 2)
    stop("the chunk option 'render' must be a function of the form ",
         "function(x, options) or function(x, ...)")
  merge_list(default_handlers, list(value = function(x, visible) {
    if (visible) fun(x, options = options)
  }))
}

# conditionally disable some features during R CMD check
is_R_CMD_check = function() {
  ('CheckExEnv' %in% search()) ||
    any(c('_R_CHECK_TIMINGS_', '_R_CHECK_LICENSE_') %in% names(Sys.getenv()))
}

# is the inst dir under . or ..? differs in R CMD build/INSTALL and devtools/roxygen2
inst_dir = function(...) {
  p = file.path(c('..', '.'), 'inst', ...)
  p[file.exists(p)]
}

# normalize two paths to see if they are the same file
same_file = function(f1, f2) {
  f1 = normalizePath(f1, mustWork = FALSE)
  f2 = normalizePath(f2, mustWork = FALSE)
  f1 == f2
}

# a restricted version of is.numeric (e.g. do not treat chron::chron() as
# numeric since their behavior may be somewhat unpredictable, e.g. through
# round(), #1118)
is_numeric = function(x) {
  class(x)[1] %in% c('numeric', 'integer')
}

# create \label{x} or (\#x); the latter is current an internal hack for bookdown
create_label = function(..., latex = FALSE) {
  if (isTRUE(opts_knit$get('bookdown.internal.label'))) {
    lab1 = '(\\#'; lab2 = ')'
  } else if (latex) {
    lab1 = '\\label{'; lab2 = '}'
  } else {
    return('')  # we don't want the label at all
  }
  paste0(lab1, ..., lab2)
}

# yes I hate partial matching
attr = function(...) base::attr(..., exact = TRUE)

#' Combine multiple words into a single string
#'
#' When a value from an inline R expression is a character vector of multiple
#' elements, we may want to combine them into a phrase like \samp{a and b}, or
#' \code{a, b, and c}. That is what this a helper function does.
#'
#' If the length of the input \code{words} is smaller than or equal to 1,
#' \code{words} is returned. When \code{words} is of length 2, the first word
#' and second word are combined using the \code{and} string. When the length is
#' greater than 2, \code{sep} is used to separate all words, and the \code{and}
#' string is prepended to the last word.
#' @param words a character vector
#' @param sep the separator to be inserted among words
#' @param and a character string to be prepended to the last word
#' @param before,after A character string to be added before/after each word
#' @return A character string.
#' @export
#' @examples combine_words('a'); combine_words(c('a', 'b'))
#' combine_words(c('a', 'b', 'c'))
#' combine_words(c('a', 'b', 'c'), sep = ' / ', and = '')
#' combine_words(c('a', 'b', 'c'), and = '')
#' combine_words(c('a', 'b', 'c'), before = '"', after = '"')
combine_words = function(words, sep = ', ', and = ' and ', before = '', after = before) {
  n = length(words)
  if (n == 0) return(words)
  words = paste0(before, words, after)
  if (n == 1) return(words)
  if (n == 2) return(paste(words, collapse = and))
  if (grepl('^ ', and) && grepl(' $', sep)) and = gsub('^ ', '', and)
  words[n] = paste0(and, words[n])
  paste(words, collapse = sep)
}
