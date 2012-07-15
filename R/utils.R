## copy objects in one environment to the other
copy_env = function(from, to, keys = ls(envir = from, all.names = TRUE)) {
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

line_prompt = evaluate:::line_prompt

## add a prefix to output
comment_out = function(x, options) {
  prefix = options$comment
  if (!is.null(prefix) && nzchar(prefix) && !is.na(prefix)) {
    prefix = str_c(prefix, ' ')
    line_prompt(x, prompt = prefix, continue = prefix)
  } else x
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
  str_detect(x, '^\\s*$')
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
              "using default color...",
              "see http://yihui.name/knitr/options")
    }
  }
  if (length(x) != 3L) stop('invalid color:', col)
  if (is.numeric(x)) x = round(x, 3L)
  outdec = options(OutDec = '.'); on.exit(options(outdec))
  sprintf('\\definecolor{%s}{rgb}{%s, %s, %s}', variable, x[1], x[2], x[3])
}

## split by semicolon or colon
sc_split = function(string) {
  if (length(string) > 1L) return(string)
  str_trim(str_split(string, ';|,')[[1]])
}

## extract LaTeX packages for tikzDevice
set_preamble = function(input) {
  if (!out_format('latex')) return()
  db = knit_patterns$get('document.begin')
  if (length(db) != 1L) return()  # no \begin{document} pattern
  hb = knit_patterns$get('header.begin')
  if (length(hb) != 1L) return()  # no \documentclass{} pattern
  idx2 = str_detect(input, db)
  if (!any(idx2)) return()
  if ((idx2 <- which(idx2)[1]) < 2L) return()
  txt = str_c(input[seq_len(idx2 - 1L)], collapse = '\n')  # rough preamble
  idx = str_locate(txt, hb)  # locate documentclass
  if (any(is.na(idx))) return()
  options(tikzDocumentDeclaration = str_sub(txt, idx[, 1L], idx[, 2L]))
  preamble = pure_preamble(str_split(str_sub(txt, idx[, 2L] + 1L), '\n')[[1L]])
  .knitEnv$tikzPackages = c(.header.sweave.cmd, preamble, '\n')
}
## filter out code chunks from preamble if they exist (they do in LyX/Sweave)
pure_preamble = function(preamble) {
  res = split_file(lines = preamble, set.preamble = FALSE) # should avoid recursion
  if (!parent_mode()) {
    ## when not in parent mode, just return normal texts and skip code
    return(unlist(res))
  }
  owd = setwd(input_dir()); on.exit(setwd(owd))
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
  if (child_mode()) return() # quit if in child mode
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

## scientific notation in TeX
format_sci = function(x, format = "latex") {
  if (!is.double(x)) return(x)
  scipen = getOption("scipen") + 4L
  if (all(abs(lx <- floor(log(abs(x), 10))) < scipen))
    return(round(x, getOption("digits"))) # no need sci notation
  b = round(x/10^lx, getOption("digits"))
  b[b %in% c(1, -1)] = ""
  res = switch(format, latex = {
    s = sci_notation("%s%s10^{%s}", b, "\\times ", lx)
    if (inherits(x, "AsIs")) s else sprintf("$%s$", s)
  }, html = sci_notation("%s%s10<sup>%s</sup>", b, " &times; ", lx), rst = {
    # if AsIs, use the :math: directive
    if (inherits(x, "AsIs")) {
      s = sci_notation("%s%s10^{%s}", b, "\\times ", lx)
      sprintf(":math:`%s`", s)
    } else {
      # This needs the following line at the top of the file to define |times|
      # .. include <isonum.txt>
      sci_notation("%s%s10 :sup:`%s`", b, " |times| ", lx)
    }
  }, x)
  res[x == 0] = 0
  res
}

sci_notation = function(format, base, times, power) {
  sprintf(format, base, ifelse(base == "", "", times), power)
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
  ## compatibility with Sweave
  for (dev in c('pdf', 'eps', 'jpeg', 'png')) {
    if (isTRUE(options[[dev]])) {
      options$dev = dev
      warning("chunk option ", dev,
              "=TRUE deprecated in knitr; use new option 'dev' please")
      break
    }
  }
  if (any(idx <- options$dev == 'eps')) options$dev[idx] = 'postscript'
  if (options$results == 'tex') {
    warning("option 'results' was changed from 'tex' to 'asis'")
    options$results = 'asis'
  }

  ## compatibility with old version of knitr
  fig = options$fig
  if (identical(fig, FALSE)) {
    warning("option 'fig' deprecated; use fig.keep='none' please")
    options$fig.keep = 'none'
  } else if (identical(fig, TRUE)) {
    if (isTRUE(options$fig.last)) {
      warning("option 'fig.last' deprecated; use fig.keep='last' please")
      options$fig.keep = 'last'
    }
    if (isTRUE(options$fig.low)) {
      warning("option 'fig.low' deprecated; use fig.keep='all' please")
      options$fig.keep = 'all'
    }
  }
  hold = options$fig.hold
  if (identical(hold, FALSE)) {
    warning("option 'fig.hold' deprecated; use fig.show='asis' please")
    options$fig.show = 'asis'
  } else if (identical(hold, TRUE)) {
    warning("option 'fig.hold' deprecated; use fig.show='hold' please")
    options$fig.show = 'hold'
  }
  if (isTRUE(options$animate)) {
    warning("option 'animate' deprecated; use fig.show='animate' please")
    options$fig.show = 'animate'
  }

  align = options$align
  if (!is.null(align)) {
    warning("option 'align' deprecated; use fig.align instead")
    options$fig.align = align
  }
  width = options$width
  if (!is.null(width)) {
    warning("option 'width' deprecated; use fig.width instead")
    options$fig.width = width
  }
  height = options$height
  if (!is.null(height)) {
    warning("option 'height' deprecated; use fig.height instead")
    options$fig.height = height
  }

  prefix = options$prefix.string
  if (!is.null(prefix)) {
    warning("option 'prefix.string' deprecated; use fig.path instead")
    options$fig.path = prefix
  }
  prefix = options$prefix.cache
  if (!is.null(prefix)) {
    warning("option 'prefix.cache' deprecated; use cache.path instead")
    options$cache.path = prefix
  }

  ## deal with aliases: a1 is real option; a0 is alias
  if (length(a1 <- opts_knit$get('aliases')) && length(a0 <- names(a1))) {
    for (i in seq_along(a1)) {
      options[[a1[i]]] = options[[a0[i]]]
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
  res = try(system(sprintf("kpsewhich %s.sty", name), intern = TRUE), silent = TRUE)
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
  if (missing(x)) fmt else fmt %in% x
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
  # sanitize filename for LaTeX
  if (str_detect(path, '[^-_/\\\\[:alnum:]]')) {
    warning('replaced special characters in figure filename "', path, '" -> "',
            path <- str_replace_all(path, '[^-_/\\\\[:alnum:]]', '_'), '"')
  }
  str_c(path, suffix)
}

#' The environment in which a code chunk is evaluated
#'
#' This function makes the environment of a code chunk accessible inside a
#' chunk.
#'
#' In some special cases, we need access to the environment of the current
#' chunk, e.g., to make sure the code is executed in the correct environment.
#' @references \url{http://yihui.name/knitr/demo/cache/}
#' @export
knit_env = function() {
  .knitEnv$knit_env
}
# 'global' environment for knitr
knit_global = function() {
  .knitEnv$knit_global
}

#' A wrapper for rst2pdf
#'
#' Convert reST to PDF using \command{rst2pdf} (which converts from rst to PDF
#' using the ReportLab open-source library).
#' @param input the input rst file
#' @param command a character string which gives the path of the
#'   \command{rst2pdf} program (if it is not in PATH, the full path has to be
#'   given)
#' @param options extra command line options, e.g. \code{'-o foo.pdf -v'}
#' @author Alex Zvoleff
#' @export
#' @seealso \code{\link{knit2pdf}}
#' @references \url{http://rst2pdf.ralsina.com.ar/}
rst2pdf = function(input, command = "rst2pdf", options = "") {
  system2(command, paste(input, options))
}

#' Convert Rnw or Rrst files to PDF using knit() and texi2pdf() or rst2pdf()
#'
#' Knit the input Rnw or Rrst document, and compile to PDF using \code{texi2pdf}
#' or \code{rst2pdf}.
#' @inheritParams knit
#' @param compiler a character string which gives the LaTeX program used to
#'   compile the tex document to PDF (by default it uses the default setting of
#'   \code{\link[tools]{texi2pdf}}, which is often PDFLaTeX); this argument will
#'   be used to temporarily set the environmental variable \samp{PDFLATEX}. For
#'   an Rrst file, setting compiler to \code{'rst2pdf'} will use
#'   \code{\link{rst2pdf}} to compiles the rst file to PDF using the ReportLab
#'   open-source library.
#' @param ... options to be passed to \code{\link[tools]{texi2pdf}} or
#'   \code{\link{rst2pdf}}
#' @author Ramnath Vaidyanathan, Alex Zvoleff and Yihui Xie
#' @export
#' @importFrom tools texi2pdf
#' @seealso \code{\link{knit}}, \code{\link[tools]{texi2pdf}},
#'   \code{\link{rst2pdf}}
#' @examples ## compile with xelatex
#'
#' ## knit2pdf(..., compiler = 'xelatex')
#'
#' ## compile a reST file with rst2pdf
#'
#' ## knit2pdf(..., compiler = 'rst2pdf')
knit2pdf = function(input, output = NULL, compiler = NULL, ..., envir = parent.frame()) {
  out = knit(input, output, envir = envir)
  owd = setwd(dirname(out)); on.exit(setwd(owd))
  if (!is.null(compiler)) {
    if (compiler == "rst2pdf") {
      if (tolower(file_ext(out)) != "rst") stop("for rst2pdf compiler input must be a .rst file")
      return(rst2pdf(basename(out), ...))
    } else {
      # use the specified PDFLATEX command
      oc = Sys.getenv('PDFLATEX')
      on.exit(Sys.setenv(PDFLATEX = oc), add = TRUE)
      Sys.setenv(PDFLATEX = compiler)
    }
  }
  texi2pdf(basename(out), ...)
}

#' Convert markdown to HTML using knit() and markdownToHTML()
#'
#' This is a convenience function to knit the input markdown source and call
#' \code{markdownToHTML()} to convert the result to HTML.
#' @inheritParams knit
#' @param ... options passed to \code{\link[markdown]{markdownToHTML}}
#' @export
#' @seealso \code{\link{knit}}, \code{\link[markdown]{markdownToHTML}}
#' @return If the argument \code{text} is NULL, a character string (HTML code)
#'   is returned; otherwise the result is written into a file and \code{NULL} is
#'   returned.
#' @examples # a minimal example
#' writeLines(c("# hello markdown", '``` {r hello-random, echo=TRUE}', 'rnorm(5)', '```'), 'test.Rmd')
#' knit2html('test.Rmd')
#' if (interactive()) browseURL('test.html')
knit2html = function(input, ..., text = NULL, envir = parent.frame()){
  if (is.null(text)) {
    out = knit(input, envir = envir)
    markdown::markdownToHTML(out, str_c(file_path_sans_ext(out), '.html'), ...)
  } else {
    out = knit(text = text, envir = envir)
    markdown::markdownToHTML(text = out, ...)
  }
}


#' Run the code in a specified chunk
#'
#' We can specify a chunk label and use this function to evaluate the code in
#' this chunk. It is an alternative to the chunk reference in Sweave.
#'
#' The difference between this type of chunk reference and the chunk option
#' \code{ref.label} is that the latter can only be used for a chunk so that it
#' has exactly the same code as the reference chunk, whereas this function makes
#' it possible to collect several little chunks and run them inside another big
#' chunk.
#' @param label the chunk label
#' @param envir the environment in which to evaluate the code
#' @return Values returned by the code in the chunk.
#' @note Recursion (must be finite, of course) of reference is allowed, e.g. we
#'   may run the code of \samp{chunk2} in \samp{chunk1}, and \samp{chunk2} also
#'   contains a reference to \samp{chunk3}, then if we run \samp{chunk1}, both
#'   the code in \samp{chunk2} and \samp{chunk3} will be evaluated.
#' @export
#' @examples # see http://yihui.name/knitr/demo/reference/
run_chunk = function(label, envir = parent.frame()) {
  eval(parse(text = knit_code$get(label)), envir = envir)
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
                 ".", ext, sep = ""), options)
}

# escape special LaTeX characters
escape_latex = function(x, newlines = FALSE) {
  x = gsub('\\\\', '\\\\textbackslash', x)
  x = gsub('([#$%&_{}])', '\\\\\\1', x)
  x = gsub('\\\\textbackslash([^{]|$)', '\\\\textbackslash{}\\1', x)
  x = gsub('~', '\\\\textasciitilde{}', x)
  x = gsub('\\^', '\\\\textasciicircum{}', x)
  if (newlines) x = gsub('\n', ' \\\\\\\\ \n', x)
  x
}
