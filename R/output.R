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
#' lists can be found in \code{all_patterns} (call it \code{apat}). First
#' \pkg{knitr} will try to decide the pattern list based on the filename
#' extension of the input document, e.g. \samp{Rnw} files use the list
#' \code{apat$rnw}, \samp{tex} uses the list \code{apat$tex}, \samp{brew} uses
#' \code{apat$brew} and HTML files use \code{apat$html}; for unkown extensions,
#' the content of the input document is matched against all pattern lists to
#' automatically which pattern list is being used. You can also manually set the
#' pattern list using the \code{\link{knit_patterns}} object or the
#' \code{\link{pat_rnw}} series functions in advance and \pkg{knitr} will
#' respect the setting.
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
#' @param encoding the encoding of the input file; see \code{\link{file}}
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
#'   \url{https://bitbucket.org/stat/knitr/downloads/knitr-manual.pdf}
#'
#'   The \pkg{knitr} graphics manual:
#'   \url{https://bitbucket.org/stat/knitr/downloads/knitr-graphics.pdf}
#' @examples library(knitr)
#' (f = system.file('examples', 'knitr-minimal.Rnw', package = 'knitr'))
#' knit(f)  # compile to tex
#'
#' purl(f)  # tangle R code
#' purl(f, documentation = 0)  # extract R code only
#' purl(f, documentation = 2)  # also include documentation
knit = function(input, output = NULL, tangle = FALSE, text = NULL,
                envir = parent.frame(), encoding = getOption('encoding')) {

  # is input from a file? (or a connection on a file)
  in.file = !missing(input) &&
    (is.character(input) || prod(inherits(input, c('file', 'connection'), TRUE)))
  oconc = knit_concord$get(); on.exit(knit_concord$set(oconc), add = TRUE)
  # make a copy of the input path in input2 and change input to file path
  if (!missing(input)) input2 = input
  if (in.file && !is.character(input)) input = summary(input)$description

  if (child_mode()) {
    setwd(opts_knit$get('output.dir')) # always restore original working dir
    # in child mode, input path needs to be adjusted
    if (in.file && !is_abs_path(input)) {
      input = str_c(opts_knit$get('child.path'), input)
      input = file.path(input_dir(), input)
    }
  } else {
    oenvir = .knitEnv$knit_global; .knitEnv$knit_global = envir
    on.exit({.knitEnv$knit_global = oenvir}, add = TRUE)
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
    optc = opts_chunk$get(); on.exit(opts_chunk$restore(optc), add = TRUE)
    ocode = knit_code$get(); on.exit(knit_code$restore(ocode), add = TRUE)
    if (tangle) knit_code$restore() # clean up code before tangling
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
    knit_concord$set(infile = input)
  }
  if (concord_mode()) {
    # 'outfile' from last parent call is my parent
    if (child_mode()) knit_concord$set(parent = knit_concord$get('outfile'))
    knit_concord$set(outfile = output)
  }

  encoding = correct_encode(encoding)
  text = if (is.null(text)) {
    readLines(if (is.character(input2)) {
      con = file(input2, encoding = encoding); on.exit(close(con), add = TRUE); con
    } else input2, warn = FALSE)
  } else split_lines(text) # make sure each element is one line
  if (!length(text)) return() # a trivial case: simply and exit
  text = native_encode(text)

  apat = all_patterns; opat = knit_patterns$get()
  on.exit(knit_patterns$restore(opat), add = TRUE)
  if (length(opat) == 0 || all(sapply(opat, is.null))) {
    # use ext if cannot auto detect pattern
    if (is.null(pattern <- detect_pattern(text, ext))) {
      # nothing to be executed; just return original input
      if (is.null(output)) return(paste(text, collapse = '\n')) else {
        cat(text, sep = '\n', file = output); return(output)
      }
    }
    if (!(pattern %in% names(apat)))
      stop("a pattern list cannot be automatically found for the file extension '",
           ext, "' in built-in pattern lists; ",
           'see ?knit_patterns on how to set up customized patterns')
    set_pattern(pattern)
    if (pattern == 'rnw' && is_sweave(text)) remind_sweave(if (in.file) input)
    opts_knit$set(out.format = switch(pattern, rnw = 'latex', tex = 'latex',
                                      html = 'html', md = 'markdown', rst = 'rst',
                                      brew = 'brew'))
  }

  if (is.null(out_format())) auto_format(ext)
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
  res = paste(knit_hooks$get('document')(res), collapse = '\n')
  if (!is.null(output))
    writeLines(if (encoding == '') res else native_encode(res, to = encoding),
               con = output, useBytes = encoding != '')
  if (!child_mode()) {
    dep_list$restore()  # empty dependency list
    .knitEnv$labels = NULL
  }

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
#' @param documentation an integer specifying the level of documentation to go
#'   the tangled script: \code{0} means pure code (discard all text chunks);
#'   \code{1} (default) means add the chunk headers to code; \code{2} means add
#'   all text chunks to code as roxygen comments
#' @param ... arguments passed to \code{\link{knit}()} from \code{purl()}
#' @export
purl = function(..., documentation = 1L) {
  # for compatibility to knitr <= 0.8.8
  if (is.logical(documentation)) documentation = ifelse(documentation, 2L, 1L)
  doc = opts_knit$get('documentation'); on.exit(opts_knit$set(documentation = doc))
  opts_knit$set(documentation = documentation)
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
    res[i] = withCallingHandlers(
      (if (tangle) process_tangle else process_group)(group),
      error = function(e) {
        cat(res, sep = '\n', file = output %n% '')
        message(
          'Quitting from lines ', str_c(current_lines(i), collapse = '-'),
          ' (', knit_concord$get('infile'), ') '
        )
      }
    )
    # output line numbers
    if (concord_mode()) {
      # look back and see who is 0, then fill them up
      idx = which(olines[1:i] == 0L); olines[idx] = line_count(res[idx])
      knit_concord$set(outlines = olines)
    }
  }

  if (!tangle) res = insert_header(res)  # insert header
  print_knitlog()

  res
}

auto_out_name = function(input) {
  base = file_path_sans_ext(input)
  if (opts_knit$get('tangle')) return(str_c(base, '.R'))
  ext = tolower(file_ext(input))
  if (ext %in% c('rnw', 'snw')) return(str_c(base, '.tex'))
  if (ext %in% c('rmd', 'rmarkdown', 'rhtml', 'rhtm', 'rtex', 'stex', 'rrst'))
    return(str_c(base, '.', substring(ext, 2L)))
  if (ext == 'brew') return(str_c(base, '.txt'))
  if (ext %in% c('tex', 'html', 'md')) {
    if (str_detect(input, '_knit_')) {
      return(str_replace(input, '_knit_', ''))
    } else return(str_c(base, '-out.', ext))
  }
  stop('cannot determine the output filename automatically')
}

## decide output format based on file extension
ext2fmt = c(
  rnw = 'latex', snw = 'latex', tex = 'latex', rtex = 'latex', stex = 'latex',
  htm = 'html', html = 'html', rhtml = 'html', rhtm = 'html',
  md = 'markdown', markdown = 'markdown', rmd = 'markdown', rmarkdown = 'markdown',
  brew = 'brew', rst = 'rst', rrst = 'rst'
)

auto_format = function(ext) {
  fmt = ext2fmt[ext]
  if (is.na(fmt)) fmt = {
    warning('cannot automatically decide the output format')
    'unknown'
  }
  opts_knit$set(out.format = fmt)
  invisible(fmt)
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
  if (is.null(path)) return() # the input document is empty
  if (opts_knit$get('tangle')) {
    str_c('\n', 'source("', path, '")')
  } else if (concord_mode() || !out_format('latex')) {
    on.exit(unlink(path)) # child output file is temporary
    str_c(readLines(path), collapse = '\n')
  } else {
    str_c('\n\\', opts_knit$get('child.command'), '{', path, '}')
  }
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
  if (length(x) == 0L) return(x)
  lapply(x, wrap, options)
}

wrap.character = function(x, options) {
  if (!output_asis(x, options)) x = comment_out(x, options$comment)
  knit_hooks$get('output')(x, options)
}

wrap.source = function(x, options) {
  src = str_replace(x$src, '\n$', '')
  src = hilight_source(src, out_format(), options)
  src = str_c(c(src, ''), collapse = '\n')
  knit_hooks$get('source')(src, options)
}

msg_wrap = function(message, type, options) {
  # when output format is latex, do not wrap messages (let latex deal with wrapping)
  if (!out_format(c('latex', 'listings', 'sweave')))
    message = str_wrap(message, width = getOption('width'))
  knit_log$set(setNames(
    list(c(knit_log$get(type), str_c('Chunk ', options$label, ':\n  ', message))),
    type
  ))
  knit_hooks$get(type)(comment_out(message, options$comment), options)
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
  fig.cur = plot_counter()
  options$fig.cur = fig.cur # put fig num in options
  name = fig_path(if(options$fig.num <= 1) '' else fig.cur, options)
  if (!file.exists(dirname(name)))
    dir.create(dirname(name), recursive = TRUE) # automatically creates dir for plots
  ## vectorize over dev, ext and dpi: save multiple versions of the plot
  name.ext = mapply(save_plot,
                    dev = options$dev, ext = options$fig.ext, dpi = options$dpi,
                    MoreArgs = list(plot = x, name = name, options = options),
                    SIMPLIFY = FALSE)[[1]]
  knit_hooks$get('plot')(name.ext, reduce_plot_opts(options))
}
