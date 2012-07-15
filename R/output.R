#' Knit a document
#'
#' This function takes an input file, extracts the R code in it according to a
#' list of patterns, evaluates the code and writes the output in another file.
#' It can also tangle R source code from the input document (\code{purl()} is a
#' wrapper to \code{knit(..., tangle = TRUE)}).
#'
#' For most of the time, it is not necessary to set any options outside the
#' input document; in other words, a single call like
#' \code{knit('my_input.Rnw')} is usually enough. This function will try to
#' determine many internal settings automatically. For the sake of
#' reproducibility, it is a better practice to include the options inside the
#' input document (to be self-contained), instead of setting them before
#' knitting the document.
#'
#' First the filename of the output document is determined in this way:
#' \file{foo.Rnw} generates \file{foo.tex}, and other filename extensions like
#' \file{.Rtex}, \file{.Rhtml} (\file{.Rhtm}) and \file{.Rmd}
#' (\file{.Rmarkdown}) will generate \file{.tex}, \file{.html} and \file{.md}
#' respectively. For other types of files, the file extension is reserved; if
#' the filename contains \samp{_knit_}, this part will be removed in the output
#' file, e.g., \file{foo_knit_.html} creates the output \file{foo.html}; if
#' \samp{_knit_} is not found in the filename, \file{foo.ext} will produce
#' \file{foo-out.ext}. If \code{tangle = TRUE}, \file{foo.ext} generates an R
#' script \file{foo.R}.
#'
#' We need a set of syntax to identify special markups for R code chunks and R
#' options, etc. The syntax is defined in a pattern list. All built-in pattern
#' lists can be found in \code{all_patterns} (call it \code{apat}). First the
#' content of the input document is matched against all pattern lists to
#' automatically which pattern list is being used. If automatic detection
#' failed, the pattern list will be decided based on the filename extension of
#' the input document. \samp{Rnw} files use the list \code{apat$rnw}, \samp{tex}
#' uses the list \code{apat$tex}, \samp{brew} uses \code{apat$brew} and
#' HTML-like files use \code{apat$html} (e.g. \samp{html} and \samp{md} files).
#' You can manually set the pattern list using the \code{\link{knit_patterns}}
#' object or the \code{\link{pat_rnw}} series functions in advance and
#' \pkg{knitr} will respect the setting.
#'
#' According to the output format (\code{opts_knit$get('out.format')}), a set of
#' output hooks will be set to mark up results from R (see
#' \code{\link{render_latex}}). The output format can be LaTeX, Sweave and HTML,
#' etc. The output hooks decide how to mark up the results (you can customize
#' the hooks).
#'
#' See the package website and manuals in the references to know more about
#' \pkg{knitr}, including the full documentation of chunk options and demos,
#' etc.
#' @param input path of the input file
#' @param output path of the output file; if \code{NULL}, this function will try
#'   to guess and it will be under the current working directory
#' @param tangle whether to tangle the R code from the input file (like
#'   \code{\link[utils]{Stangle}})
#' @param text a character vector as an alternative way to provide the input
#'   file
#' @param envir the environment in which the code chunks are to be evaluated
#'   (can use \code{\link{new.env}()} to guarantee an empty new environment)
#' @return The compiled document is written into the output file, and the path
#'   of the output file is returned, but if the \code{output} path is
#'   \code{NULL}, the output is returned as a character vector.
#' @note The name \code{knit} comes from its counterpart \samp{weave} (as in
#'   Sweave), and the name \code{purl} (as \samp{tangle} in Stangle) comes from
#'   a knitting method `knit one, purl one'.
#'
#'   If the input document has child documents, they will also be compiled
#'   recursively. See \code{\link{knit_child}}.
#'
#'   The working directory when evaluating R code chunks is the directory of the
#'   input document by default, so if the R code involves with external files
#'   (like \code{read.table()}), it is better to put these files under the same
#'   directory of the input document so that we can use relative paths. However,
#'   it is possible to change this directory with the package option
#'   \code{\link{opts_knit}$set(root.dir = ...)} so all paths in code chunks are
#'   relative to this \code{root.dir}.
#'
#'   The arguments \code{input} and \code{output} do not have to be restricted
#'   to files; they can be \code{stdin()}/\code{stdout()} or other types of
#'   connections, but the pattern list to read the input has to be set in
#'   advance (see \code{\link{pat_rnw}}), and the output hooks should also be
#'   set (see \code{\link{render_latex}}), otherwise \pkg{knitr} will try to
#'   guess the patterns and output format.
#' @export
#' @references Package homepage: \url{http://yihui.name/knitr/}
#'
#'   The \pkg{knitr} main manual:
#'   \url{https://github.com/downloads/yihui/knitr/knitr-manual.pdf}
#'
#'   The \pkg{knitr} graphics manual:
#'   \url{https://github.com/downloads/yihui/knitr/knitr-graphics.pdf}
#' @examples library(knitr)
#' (f = tempfile(fileext = '.Rnw'))
#' file.copy(system.file('examples', 'knitr-minimal.Rnw', package = 'knitr'),
#'   f, overwrite = TRUE)
#' knit(f)
#' ## or setwd(dirname(f)); knit(basename(f))
#'
#' purl(f)  # extract R code only
knit = function(input, output = NULL, tangle = FALSE, text = NULL, envir = parent.frame()) {

  in.file = !missing(input) && is.character(input)  # is a file input
  oconc = knit_concord$get(); on.exit(knit_concord$set(oconc), add = TRUE)
  if (in.file) input2 = input # make a copy of the input path
  if (child_mode()) {
    setwd(opts_knit$get('output.dir')) # always restore original working dir
    # in child mode, input path needs to be adjusted
    if (in.file && !is_abs_path(input)) {
      input2 = str_c(opts_knit$get('child.path'), input)
      input = file.path(input_dir(), input2)
    }
  } else {
    .knitEnv$knit_global = envir  # the envir to eval code
    opts_knit$set(output.dir = getwd()) # record working directory in 1st run
    knit_log$restore()
    on.exit(chunk_counter(reset = TRUE), add = TRUE) # restore counter
    ## turn off fancy quotes, use smaller digits/width, warn immediately
    oopts = options(useFancyQuotes = FALSE, digits = 4L, width = 75L, warn = 1L,
                  device = function(file = NULL, width = 7, height = 7, ...) {
                    pdf(file, width, height, ...)
                  })
    on.exit(options(oopts), add = TRUE)
    # restore chunk options after parent exits
    optc = opts_chunk$get()
    on.exit({opts_chunk$restore(); opts_chunk$set(optc)}, add = TRUE)
    ocode = knit_code$get()
    if (tangle) knit_code$restore() # clean up code before tangling
    on.exit({knit_code$restore(); knit_code$set(ocode)}, add = TRUE)
    optk = opts_knit$get(); on.exit(opts_knit$set(optk), add = TRUE)
    opts_knit$set(tangle = tangle)
  }

  ext = 'unknown'
  if (in.file) {
    input.dir = .knitEnv$input.dir; on.exit({.knitEnv$input.dir = input.dir}, add = TRUE)
    .knitEnv$input.dir = dirname(input) # record input dir
    if (is.null(output)) output = basename(auto_out_name(input))
    ext = tolower(file_ext(input))
    options(tikzMetricsDictionary = tikz_dict(input)) # cache tikz dictionary
    knit_concord$set(infile = input2)
  }
  if (concord_mode()) {
    # 'outfile' from last parent call is my parent
    if (child_mode()) knit_concord$set(parent = knit_concord$get('outfile'))
    knit_concord$set(outfile = output)
  }

  text = if (is.null(text)) readLines(input, warn = FALSE) else {
    unlist(strsplit(text, '\n')) # make sure each element is one line
  }
  if (!length(text)) return() # a trivial case: simply and exit

  apat = all_patterns; opat = knit_patterns$get()
  on.exit({knit_patterns$restore(); knit_patterns$set(opat)}, add = TRUE)
  if (length(opat) == 0 || all(sapply(opat, is.null))) {
    # use ext if cannot auto detect pattern
    if (is.null(pattern <- detect_pattern(text, ext))) {
      # nothing to be executed; just return original input
      if (is.null(output)) return(paste(text, collapse = '\n')) else {
        cat(text, file = output); return(output)
      }
    }
    if (!(pattern %in% names(apat)))
      stop("a pattern list cannot be automatically found for the file extension '",
           ext, "' in built-in pattern lists; ",
           'see ?knit_patterns on how to set up customized patterns')
    knit_patterns$restore()
    knit_patterns$set(apat[[pattern]])
    opts_knit$set(out.format = switch(pattern, rnw = 'latex', tex = 'latex',
                                      html = 'html', md = 'markdown', rst = 'rst',
                                      brew = 'brew'))
  }

  if (is.null(out_format())) {
    fmt = switch(ext, rnw = 'latex', tex = 'latex', htm = 'html', html = 'html',
                 md = 'markdown', markdown = 'markdown', brew = 'brew', rst = 'rst',
                 {warning('cannot automatically decide the output format'); 'unknown'})
    ## set built-in hooks
    opts_knit$set(out.format = fmt)
  }
  ## change output hooks only if they are not set beforehand
  if (identical(knit_hooks$get(names(.default.hooks)), .default.hooks) && !child_mode()) {
    switch(out_format(), latex = render_latex(),
           sweave = render_sweave(), listings = render_listings(),
           html = render_html(), jekyll = render_jekyll(),
           markdown = render_markdown(), rst = render_rst())
    on.exit(knit_hooks$restore(), add = TRUE)
  }

  progress = opts_knit$get('progress')
  if (in.file) message(ifelse(progress, '\n\n', ''), 'processing file: ', input)
  res = process_file(text, output)
  res = knit_hooks$get('document')(res)
  cat(res, file = output %n% '')
  dep_list$restore()  # empty dependency list

  if (in.file && is.character(output) && file.exists(output)) {
    concord_gen(input2, output)  # concordance file
    if (!child_mode() && concord_mode()) {
      confile = str_c(file_path_sans_ext(output), '-concordance.tex')
      cat(.knitEnv$concordance, file = confile)
      .knitEnv$concordance = NULL # empty concord string
    }
    message('output file: ', normalizePath(output), ifelse(progress, '\n', ''))
  }

  output %n% res
}
#' @rdname knit
#' @param ... arguments passed to \code{\link{knit}}
#' @export
purl = function(...) {
  knit(..., tangle = TRUE)
}

process_file = function(text, output) {
  groups = split_file(lines = text)
  n = length(groups); res = character(n); olines = integer(n)
  tangle = opts_knit$get('tangle')

  if (opts_knit$get('progress')) {
    pb = txtProgressBar(0, n, char = '>', style = 3)
    on.exit(close(pb), add = TRUE)
  }
  for (i in 1:n) {
    knit_concord$set(i = i)
    if (opts_knit$get('progress')) {
      setTxtProgressBar(pb, i)
      if (!tangle) cat('\n')  # under tangle mode, only show one progress bar
      flush.console()
    }
    group = groups[[i]]
    txt = try((if (tangle) process_tangle else process_group)(group), silent = TRUE)
    if (inherits(txt, 'try-error')) {
      print(group)
      cat(res, sep = '\n', file = output %n% '')
      stop(sprintf('Quitting from lines %s: (%s) %s',
                   str_c(current_lines(i), collapse = '-'),
                   paste('', knit_concord$get('infile'), sep = ''), txt))
    }
    res[i] = txt
    # output line numbers
    if (concord_mode()) {
      # look back and see who is 0, then fill them up
      idx = which(olines[1:i] == 0L); olines[idx] = line_count(res[idx])
      knit_concord$set(outlines = olines)
    }
  }

  if (!tangle) res = insert_header(res)  # insert header
  print_knitlog()

  str_c(c(res, ""), collapse = "\n")
}

auto_out_name = function(input) {
  base = file_path_sans_ext(input)
  if (opts_knit$get('tangle')) return(str_c(base, '.R'))
  ext = tolower(file_ext(input))
  if (ext %in% c('rnw', 'snw')) return(str_c(base, '.tex'))
  if (ext %in% c('rmd', 'rmarkdown', 'rhtml', 'rhtm', 'rtex', 'stex', 'rrst'))
    return(str_c(base, '.', substring(ext, 2L)))
  if (ext %in% c('brew', 'tex', 'html', 'md')) {
    if (str_detect(input, '_knit_')) {
      return(str_replace(input, '_knit_', ''))
    } else return(str_c(base, '-out.', ext))
  }
  stop('cannot determine the output filename automatically')
}

#' Knit a child document
#'
#' This function knits a child document and returns a character string to input
#' the result into the main document. It is designed to be used in the chunk
#' option \code{child} and serves as the alternative to the
#' \command{SweaveInput} command in Sweave.
#'
#' For LaTeX output, the command used to input the child document (usually
#' \samp{input} or \samp{include}) is from the package option
#' \code{child.command} (\code{opts_knit$get('child.command')}). For other types
#' of output, the content of the compiled child document is returned.
#'
#' When we call \code{purl()} to extract R code, the code in the child document
#' is extracted and saved into an R script.
#'
#' The path of the child document is relative to the parent document.
#' @param ... arguments passed to \code{\link{knit}}
#' @param eval logical: whether to evaluate the child document
#' @return A character string of the form \samp{\command{child-doc.tex}} or
#'   \code{source("child-doc.R")}, depending on the argument \code{tangle}
#'   passed in. When concordance is turned on or the output format is not LaTeX,
#'   the content of the compiled child document is returned as a character
#'   string so it can be written back to the main document directly.
#' @references \url{http://yihui.name/knitr/demo/child/}
#' @note This function is not supposed be called directly like
#'   \code{\link{knit}()}; instead it must be placed in a parent document to let
#'   \code{\link{knit}()} call it indirectly.
#' @export
#' @examples ## you can write \Sexpr{knit_child('child-doc.Rnw')} in an Rnw file 'main.Rnw' to input child-doc.tex in main.tex
#'
#' ## comment out the child doc by \Sexpr{knit_child('child-doc.Rnw', eval = FALSE)}
#'
#' ## use \include: opts_knit$set(child.command = 'include')
knit_child = function(..., eval = TRUE) {
  if (!eval) return('')
  child = child_mode()
  opts_knit$set(child = TRUE) # yes, in child mode now
  on.exit(opts_knit$set(child = child)) # restore child status
  path = knit(..., tangle = opts_knit$get('tangle'))
  if (opts_knit$get('tangle')) {
    str_c('\n', 'source("', path, '")')
  } else if (concord_mode() || !out_format('latex')) {
    on.exit(unlink(path)) # child output file is temporary
    str_c(readLines(path), collapse = '\n')
  } else {
    str_c('\n\\', opts_knit$get('child.command'), '{', path, '}')
  }
}

#' Automatically create a report based on an R script and a template
#'
#' This is a convenience function for small-scale automatic reporting based on
#' an R script and a template.
#'
#' The first two lines of the R script can contain the title and author of the
#' report in comments of the form \samp{## title:} and \samp{## author:}. The
#' template must have a chunk named \samp{auto-report}, which will be used to
#' input all the R code from the script. See the examples below.
#' @param script path to the R script
#' @param template path of the template to use (by default the Rnw template in
#'   this package; there is also an HTML template in \pkg{knitr})
#' @param output the output filename (passed to \code{\link{knit}}); by default
#'   it uses the base filename of the script
#' @inheritParams knit
#' @return path of the output document
#' @export
#' @seealso \code{\link{spin}} (turn a specially formatted R script to a report)
#' @examples s = system.file('misc', 'stitch-test.R', package = 'knitr')
#' \dontrun{stitch(s)}
#'
#' # HTML report
#' stitch(s, system.file('misc', 'knitr-template.Rhtml', package = 'knitr'))
#'
#' # or convert markdown to HTML
#' stitch(s, system.file('misc', 'knitr-template.Rmd', package = 'knitr'))
stitch = function(script,
                  template = system.file('misc', 'knitr-template.Rnw', package = 'knitr'),
                  output = NULL, envir = parent.frame()) {
  lines = readLines(script, warn = FALSE)
  ## extract title and author from first two lines
  if (comment_to_var(lines[1L], '.knitr.title', '^#+ *title:', envir)) lines = lines[-1L]
  if (comment_to_var(lines[1L], '.knitr.author', '^#+ *author:', envir)) lines = lines[-1L]
  knit_code$set(`auto-report` = lines)
  input = basename(template)
  input = str_c(file_path_sans_ext(basename(script)), '.', file_ext(input))
  if (file.exists(input)) warning(input, ' already exists') else file.copy(template, input)
  out = knit(input, output, envir = envir)
  switch(file_ext(out), tex = {
    texi2pdf(out, clean = TRUE)
    system(paste(getOption('pdfviewer'), shQuote(str_replace(out, '\\.tex$', '.pdf'))))
  }, md = {
    out.html = str_c(file_path_sans_ext(out), '.html')
    markdown::markdownToHTML(out, out.html)
    browseURL(out.html)
  }, html = browseURL(out))
  knit_code$restore()
  out
}

knit_log = new_defaults()  # knitr log for errors, warnings and messages

#' Wrap evaluated results for output
#'
#' @param x output from \code{\link[evaluate]{evaluate}}
#' @param options list of options used to control output
#' @noRd
#' @S3method wrap list
#' @S3method wrap character
#' @S3method wrap source
#' @S3method wrap warning
#' @S3method wrap message
#' @S3method wrap error
#' @S3method wrap recordedplot
wrap = function(x, options = list()) {
  UseMethod("wrap", x)
}

wrap.list = function(x, options = list()) {
  lapply(x, wrap, options)
}

wrap.character = function(x, options) {
  if (!output_asis(x, options)) x = comment_out(x, options)
  knit_hooks$get('output')(x, options)
}

wrap.source = function(x, options) {
  src = x$src
  if (options$highlight) {
    src = hilight_source(src, out_format(), options)
  } else if (options$prompt) src = sapply(src, line_prompt, USE.NAMES = FALSE)
  src = str_c(src, collapse = '')
  src = str_replace(src, '([^\n]+)$', '\\1\n')
  knit_hooks$get('source')(src, options)
}

msg_wrap = function(message, type, options) {
  message = str_wrap(message, width = getOption('width'))
  knit_log$set(
    structure(list(c(knit_log$get(type), str_c('Chunk ', options$label, ':\n  ', message))),
    .Names = type)
  )
  knit_hooks$get(type)(comment_out(str_c(message, '\n'), options), options)
}

wrap.warning = function(x, options) {
  msg_wrap(str_c("Warning: ", x$message), 'warning', options)
}

wrap.message = function(x, options) {
  msg_wrap(x$message, 'message', options)
}

wrap.error = function(x, options) {
  msg_wrap(str_c("Error: ", x$message), 'error', options)
}

wrap.recordedplot = function(x, options) {
  if (!is.null(base.dir <- opts_knit$get('base.dir'))) {
    odir = setwd(base.dir); on.exit(setwd(odir)) # switch to abs dir, then restore
  }
  ## figure number sequence for multiple plots
  if (options$fig.num <= 1) fig.cur = 0L else {
    fig.cur = plot_counter()
  }
  options$fig.cur = fig.cur # put fig num in options
  name = fig_path(if(fig.cur == 0L) '' else fig.cur, options)
  if (!file.exists(dirname(name)))
    dir.create(dirname(name), recursive = TRUE) # automatically creates dir for plots
  ## vectorize over dev, ext and dpi: save multiple versions of the plot
  name.ext = mapply(save_plot,
                    dev = options$dev, ext = options$fig.ext, dpi = options$dpi,
                    MoreArgs = list(plot = x, name = name, options = options),
                    SIMPLIFY = FALSE)[[1]]
  knit_hooks$get('plot')(name.ext, options)
}
