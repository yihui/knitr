#' Convert Sweave to knitr documents
#'
#' This function converts an Sweave document to a \pkg{knitr}-compatible
#' document.
#'
#' The pseudo command \samp{\\SweaveInput{file.Rnw}} is converted to a code
#' chunk header \code{<<child='file.Rnw'>>=}.
#'
#' Similarly \samp{\\SweaveOpts{opt = value}} is converted to a code chunk
#' \samp{opts_chunk$set(opt = value)} with the chunk option \code{include =
#' FALSE}; the options are automatically fixed in the same way as local chunk
#' options (explained below).
#'
#' The Sweave package \samp{\\usepackage{Sweave}} in the preamble is removed
#' because it is not required.
#'
#' Chunk options are updated if necessary: option values \code{true} and
#' \code{false} are changed to \code{TRUE} and \code{FALSE} respectively;
#' \code{fig=TRUE} is removed because it is not necessary for \pkg{knitr} (plots
#' will be automatically generated); \code{fig=FALSE} is changed to
#' \code{fig.keep='none'}; the devices \code{pdf/jpeg/png/eps/tikz=TRUE} are
#' converted to \code{dev='pdf'/'jpeg'/'png'/'postscript'/'tikz'};
#' \code{pdf/jpeg/png/eps/tikz=FALSE} are removed;
#' \code{results=tex/verbatim/hide} are changed to
#' \code{results='asis'/'markup'/'hide'}; \code{width/height} are changed to
#' \code{fig.width/fig.height}; \code{prefix.string} is changed to
#' \code{fig.path}; \code{print/term/prefix=TRUE/FALSE} are removed; most of the
#' character options (e.g. \code{engine} and \code{out.width}) are quoted;
#' \code{keep.source=TRUE/FALSE} is changed to \code{tidy=FALSE/TRUE} (note the
#' order of values).
#'
#' If a line \code{@@} (it closes a chunk) directly follows a previous
#' \code{@@}, it is removed; if a line \code{@@} appears before a code chunk and
#' no chunk is before it, it is also removed, because \pkg{knitr} only uses one
#' \samp{@@} after \samp{<<>>=} by default (which is not the original Noweb
#' syntax but more natural).
#' @param file the filename of the Rnw file
#' @param output the output filename (by default \file{file.Rnw} produces
#'   \file{file-knitr.Rnw}); if \code{text} is not NULL, no output file will be
#'   produced
#' @param encoding the encoding of the Rnw file
#' @param text an alternative way to provide the Sweave code as a character
#'   string (if provided, the \code{file} will be ignored)
#' @return If \code{text} is \code{NULL}, the \code{output} file is written and
#'   \code{NULL} is returned, otherwise the converted text string is returned.
#' @note If \samp{\\SweaveOpts{}} spans across multiple lines, it will not be
#'   fixed, and you have to fix it manually. The LaTeX-style syntax of Sweave
#'   chunks are ignored (see \code{?SweaveSyntaxLatex}); only the Noweb syntax
#'   is supported.
#' @seealso \code{\link{Sweave}}, \code{\link{gsub}}
#' @references The motivation of the changes in the syntax:
#'   \url{http://yihui.name/knitr/demo/sweave/}
#' @export
#' @examples Sweave2knitr(text='<<echo=TRUE>>=') # this is valid
#' Sweave2knitr(text="<<png=true>>=") # dev='png'
#' Sweave2knitr(text="<<eps=TRUE, pdf=FALSE, results=tex, width=5, prefix.string=foo>>=")
#' Sweave2knitr(text="<<,png=false,fig=TRUE>>=")
#' Sweave2knitr(text="\\SweaveOpts{echo=false}")
#' Sweave2knitr(text="\\SweaveInput{hello.Rnw}")
#' # Sweave example in utils
#' testfile = system.file("Sweave", "Sweave-test-1.Rnw", package = "utils")
#' Sweave2knitr(testfile, output = 'Sweave-test-knitr.Rnw')
#' knit('Sweave-test-knitr.Rnw') # or knit2pdf() directly
Sweave2knitr = function(file, output = gsub('[.]([^.]+)$', '-knitr.\\1', file),
                        encoding = getOption('encoding'), text = NULL) {
  x = if (is.null(text)) readLines(file(file, encoding = encoding), warn = FALSE) else text
  x = native_encode(x)
  x = gsub_msg('removing \\usepackage{Sweave}',
               '^\\s*\\\\usepackage(\\[.*\\])?\\{Sweave\\}', '', x)
  i = grep('^<<(.*)>>=\\s*$', x)
  if (length(i)) {
    opts = gsub('^<<(.*)>>=\\s*$', '\\1', x[i])
    x[i] = paste0('<<', fix_sweave(opts), '>>=')
  }
  x = gsub_msg("replacing \\SweaveInput{...} with <<child='...'>>=",
               '^\\s*\\\\SweaveInput\\{([^}]+)\\}', "\n<<'child-\\1', child='\\1'>>=\n@\n", x)
  s = '^\\s*\\\\SweaveOpts\\{([^}]*)\\}.*$'
  i = grep(s, x)
  if (length(i)) {
    opts = fix_sweave(gsub(s, '\\1', x[i]))
    x[i] = gsub_msg('changing \\SweaveOpts{} to opts_chunk$set()', s, '@_@_@', x[i])
    for (j in seq_along(i))
      x[i[j]] = gsub('@_@_@', paste(c(
        '\n<<include=FALSE>>=', 'library(knitr)', 'opts_chunk$set(', opts[j], ')', '@\n'
      ), collapse = '\n'), x[i[j]])
  }
  # remove the extra @
  i1 = grepl(all_patterns$rnw$chunk.begin, x)
  i2 = grepl(all_patterns$rnw$chunk.end, x)
  i = which(i2 & !filter_chunk_end(i1, i2))
  if (length(i)) {
    message('removing extra lines (#n shows line numbers):\n',
            paste(formatUL(sprintf('(#%d) %s', i, x[i]), offset = 4), collapse = '\n'))
    x = x[-i]
  }
  if (is.null(text)) {
    if (encoding != 'native.enc') x = native_encode(x, encoding)
    cat(x, sep = '\n', file = output)
  } else x
}

gsub_msg = function(msg, pattern, replacement, x, ...) {
  if (length(i <- grep(pattern, x, ...))) {
    message(msg, ':\n', paste(formatUL(x[i], offset = 4), collapse = '\n'))
    gsub(pattern, replacement, x, ...)
  } else x
}

fix_sweave = function(x) {
  x = gsub_msg('capitalizing true/false to TRUE/FALSE',
               '(echo|eval|split|include|fig|pdf|eps|jpeg|png|tikz|keep.source|print|term|strip.white|prefix)\\s*=\\s*(true|false)', '\\1=\\U\\2', x, perl = TRUE)

  x = gsub_msg('removing the unnecessary option fig=TRUE',
               'fig\\s*=\\s*TRUE', '', x)
  x = gsub_msg("fig=FALSE should be fig.keep='none'",
               'fig\\s*=\\s*FALSE', "fig.keep='none'", x)

  x = gsub_msg("replacing pdf/jpeg/png/tikz=TRUE with dev='pdf'/'jpeg'/'png'/'tikz'",
               '(pdf|jpeg|png|tikz)\\s*=\\s*TRUE', "dev='\\1'", x)
  x = gsub_msg('removing pdf/jpeg/png/eps/tikz=FALSE',
               '(pdf|jpeg|png|eps|tikz)\\s*=\\s*FALSE', '', x)
  x = gsub_msg("replacing eps=TRUE with dev='postscript'",
               'eps\\s*=\\s*TRUE', "dev='postscript'", x)

  x = gsub_msg('replacing results=tex with results=asis',
               'results\\s*=\\s*tex', 'results=asis', x)
  x = gsub_msg('replacing results=verbatim with results=markup',
               'results\\s*=\\s*verbatim', 'results=markup', x)
  x = gsub_msg('quoting the results option',
               'results\\s*=\\s*(asis|markup|hide)', "results='\\1'", x)

  x = gsub_msg('replacing width/height with fig.width/fig.height',
               '([^.])(width|height)\\s*=\\s*(\\d+)', '\\1fig.\\2=\\3', x)

  x = gsub_msg("replacing prefix.string=foo with fig.path='foo'",
               'prefix.string\\s*=\\s*([^,]+)', "fig.path='\\1'", x)

  x = gsub_msg("removing options 'print', 'term', 'prefix'",
               '(print|term|prefix)\\s*=\\s*(TRUE|FALSE|T|F)', '', x)

  x = gsub_msg('quoting options engine, fig.path, cache.path, fig.keep, fig.show, dev, out.width, out.height, fig.align',
               "(engine|fig\\.path|cache\\.path|fig\\.keep|fig\\.show|dev|out\\.width|out\\.height|fig\\.align)\\s*=\\s*([^,'\"]+)",
               "\\1='\\2'", x)

  x = gsub_msg('changing keep.source=TRUE to tidy=FALSE',
               'keep\\.source\\s*=\\s*(TRUE|T)', 'tidy=FALSE', x)
  x = gsub_msg('changing keep.source=FALSE to tidy=TRUE',
               'keep\\.source\\s*=\\s*(FALSE|F)', 'tidy=TRUE', x)

  x = gsub_msg('doubling backslashes', '\\', '\\\\', x, fixed = TRUE)
  # after we remove some options, there might be , ,
  while (length(grep(',\\s*,', x))) x = gsub(',\\s*,', ',', x)
  x = gsub('^(\\s*,\\s*)+|(\\s*,\\s*)+$', '', x)
  x
}

# check the source code to see if it is an Sweave document
which_sweave = function(x) {
  unique(c(
    grep('^\\s*\\\\(usepackage(\\[.*\\])?\\{Sweave|SweaveInput\\{|SweaveOpts\\{)', x),
    grep('^<<.*\\b(echo|eval|split|include)\\s*=\\s*(true|false)\\b.*>>=', x),
    grep('^<<.*\\bresults\\s*=\\s*(tex|verbatim|hide)\\b.*>>=', x),
    grep('^<<.*\\b(fig|pdf|eps|jpeg|png|tikz)\\s*=\\s*(true|false|T|F).*>>=', x),
    grep('^<<.*([, ])(width|height)\\s*=\\s*(\\d+).*>>=', x),
    grep('^<<.*\\b(keep.source|print|term|prefix)\\s*=\\s*(true|false|T|F).*>>=', x)
  ))
}

remind_sweave = function(file, sweave_lines) {
  msg = sprintf('It seems you are using the Sweave-specific syntax in line(s) %s; you may need Sweave2knitr("%s") to convert it to knitr',
                paste(sweave_lines, collapse = ', '), file)
  # throw a normal warning when R CMD check or tcltk not available
  warning(msg)
  if (!is_R_CMD_check() && Sys.getenv('CI') != 'true' && tcltk_available()) {
    do.call(
      getFromNamespace('tkmessageBox', 'tcltk'),
      list(title = 'Sweave Noweb syntax?', icon = 'info', message = msg)
    )
  }
}

tcltk_available = function() {
  capabilities('tcltk') && capabilities('X11') && has_package('tcltk') &&
    getFromNamespace('.TkUp', 'tcltk')
}
