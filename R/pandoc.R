#' A Pandoc wrapper to convert Markdown documents to other formats
#'
#' This function calls Pandoc to convert Markdown documents to other formats
#' such as HTML, LaTeX/PDF and Word, etc, (optionally) based on a configuration
#' file which specifies the options to use for Pandoc.
#'
#' The configuration file is a DCF file; see \code{\link{read.dcf}}. This file
#' must contain a field named \code{format} which means the output format. The
#' configurations are written in the form of \code{tag:value} and passed to
#' Pandoc (if no value is needed, just leave it empty, e.g. the option
#' \code{standalone} or \code{s} for short). If there are multiple output
#' formats, write each format and relevant configurations in a block, and
#' separate blocks with blank lines.
#' @param input a character vector of the Markdown filenames
#' @param format the output format (see References)
#' @param config the Pandoc configuration file; if missing, it is assumed to be
#'   a file with the same base name as the \code{input} file and an extension
#'   \code{.pandoc} (e.g. for \file{foo.md} it looks for \file{foo.pandoc})
#' @return The output filename (or an error if the conversion failed).
#' @references Pandoc: \url{http://johnmacfarlane.net/pandoc/}; Examples of the
#'   configuration file: \url{http://yihui.name/knitr/demo/pandoc}
#' @export
#' @examples system('pandoc -h') # see possible output formats
pandoc = function(input, format = 'html', config = getOption('config.pandoc')) {
  if (Sys.which('pandoc') == '')
    stop('Please install pandoc first: http://johnmacfarlane.net/pandoc/')
  cmd = 'pandoc'
  cfg = if (is.null(config)) sub_ext(input, 'pandoc') else config
  out = sub_ext(input, pandoc_ext(format))
  cmn = NULL  # common arguments
  if (file.exists(cfg)) {
    cfg = read.dcf(cfg)
    if (nrow(cfg) == 0L) stop('empty config file')
    if (nrow(cfg) == 1L) {
      if ('format' %in% colnames(cfg)) {
        if (cfg[1L, 'format'] != format) cfg = NA
      } else {cmn = drop(cfg); cfg = NA}
    } else {
      if (!('format' %in% colnames(cfg)))
        stop('for a config file with multiple formats, there must be a field named "format"')
      if (sum(idx <- is.na(cfg[, 'format'])) > 1L)
        stop('at most one "format" field can be NA')
      if (sum(idx) == 1L) cmn = cfg[idx, ]
      cfg = cfg[!idx, , drop = FALSE]
      cfg = cfg[cfg[, 'format'] == format, ]
    }
    out = unname(if (!is.na(cfg['o'])) cfg['o'] else {
      if (!is.na(cfg['output'])) cfg['output'] else sub_ext(input, pandoc_ext(format))
    })
    cfg = cfg[setdiff(names(cfg), c('o', 'output', 'format'))]
    cmd = paste(cmd, pandoc_arg(cfg))
  }
  cmd = paste(cmd, pandoc_arg(cmn), '-f markdown', '-t', format, '-o', out,
              paste(shQuote(input), collapse = ' '))
  message('executing ', cmd)
  if (system(cmd) == 0L) out else stop('conversion failed')
}

# infer output extension from format
pandoc_ext = function(format) {
  if (grepl('^html', format)) return('html')
  if (grepl('^latex|beamer|context|texinfo', format)) return('pdf')
  if (format %in% c('s5', 'slidy', 'slideous', 'dzslides')) return('html')
  if (grepl('^rst', format)) return('rst')
  if (format == 'opendocument') return('xml')
  format
}
# give me a vector of arguments, I turn them into commandline
pandoc_arg = function(x) {
  x = x[!is.na(x)]  # options not provided
  if (length(x) == 0L) return(NULL)
  nms = names(x)
  if (any(grepl('\n', x))) {
    # one argument used multiple times, e.g. --bibliography
    x = str_split(x, '\n')
    nms = rep(nms, sapply(x, length))
    x = unlist(x)
  }
  paste(ifelse(nchar(nms) == 1L, '-', '--'), nms,
        ifelse(x == '', '', '='), x, sep = '', collapse = ' ')
}
