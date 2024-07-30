#' A Pandoc wrapper to convert documents to other formats
#'
#' This function calls Pandoc to convert documents to other formats such as
#' HTML, LaTeX/PDF and Word, etc, (optionally) based on a configuration file or
#' in-file configurations which specify the options to use for Pandoc.
#'
#' There are two ways to input the Pandoc configurations -- through a config
#' file, or embed the configurations in the input file as special comments
#' between \verb{<!--pandoc} and \verb{-->}.
#'
#' The configuration file is a DCF file (see \code{\link{read.dcf}}). This file
#' must contain a field named \code{t} which means the output format. The
#' configurations are written in the form of \code{tag:value} and passed to
#' Pandoc (if no value is needed, just leave it empty, e.g. the option
#' \code{standalone} or \code{s} for short). If there are multiple output
#' formats, write each format and relevant configurations in a block, and
#' separate blocks with blank lines.
#'
#' If there are multiple records of the \code{t} field in the configuration, the
#' input markdown file will be converted to all these formats by default, unless
#' the \code{format} argument is specified as one single format.
#' @param input A character vector of Markdown filenames (must be encoded in
#'   UTF-8).
#' @param format Name of the output format (see References). This can be a
#'   character vector of multiple formats; by default, it is obtained from the
#'   \code{t} field in the configuration. If the configuration is empty or the
#'   \code{t} field is not found, the default output format will be
#'   \code{'html'}.
#' @param config Path to the Pandoc configuration file. If missing, it is
#'   assumed to be a file with the same base name as the \code{input} file and
#'   an extension \code{.pandoc} (e.g. for \file{foo.md} it looks for
#'   \file{foo.pandoc})
#' @param ext Filename extensions. By default, the extension is inferred from
#'   the \code{format}, e.g. \code{latex} creates \code{pdf}, \code{dzslides}
#'   creates \code{html}, and so on
#' @return The output filename(s) (or an error if the conversion failed).
#' @references Pandoc: \url{https://pandoc.org}; Examples and rules of the
#'   configurations: \url{https://yihui.org/knitr/demo/pandoc/}
#'
#'   Also see R Markdown (v2) at \url{https://rmarkdown.rstudio.com}. The
#'   \pkg{rmarkdown} package has several convenience functions and templates
#'   that make it very easy to use Pandoc. The RStudio IDE also has
#'   comprehensive support for it, so I'd recommend users who are not familiar
#'   with command-line tools to use the \pkg{rmarkdown} package instead.
#' @seealso \code{\link{read.dcf}}
#' @export
#' @examples system('pandoc -h') # see possible output formats
pandoc = function(input, format, config = getOption('config.pandoc'), ext = NA) {
  exec = tryCatch(rmarkdown::pandoc_exec(), error = function(e) Sys.which('pandoc'))
  if (length(exec) != 1 || exec == '')
    stop('Please install either RStudio or Pandoc (https://pandoc.org)')
  cfg = if (is.null(config)) with_ext(input[1L], 'pandoc') else config
  txt = pandoc_cfg(read_utf8(input[1]))
  if (file.exists(cfg)) txt = c(txt, '', read_utf8(cfg))
  con = textConnection(txt, encoding = 'UTF-8'); on.exit(close(con), add = TRUE)
  cfg = read.dcf(con)
  nms = colnames(cfg)
  if (length(nms) && 'format' %in% nms) {
    warning('the "format" field in the configuration must be renamed to "t"')
    colnames(cfg)[nms == 'format'] = 't'  # for backward compatibility
  }
  if (missing(format)) format = pandoc_fmt(cfg)
  mapply(
    pandoc_one, input, format, ext, exec, MoreArgs = list(cfg = cfg),
    USE.NAMES = FALSE
  )
}
# format is a scalar
pandoc_one = function(input, format, ext, exec, cfg) {
  cmn = NULL  # common arguments
  if (nrow(cfg) == 0L) cfg = character(0) else if (nrow(cfg) == 1L) {
    if ('t' %in% colnames(cfg)) {
      cfg = if (cfg[1L, 't'] == format) drop(cfg) else NA
    } else {cmn = drop(cfg); cfg = NA}
  } else {
    if (!('t' %in% colnames(cfg)))
      stop('for a config file with multiple output formats, there must be a field named "t"')
    if (sum(idx <- is.na(cfg[, 't'])) > 1L)
      stop('at most one "t" field can be NA')
    if (sum(idx) == 1L) cmn = cfg[idx, ]
    cfg = cfg[!idx, , drop = FALSE]
    cfg = cfg[cfg[, 't'] == format, ]
    if (!is.null(dim(cfg))) {
      if (nrow(cfg) > 1) stop('the output format is not unique in config')
      cfg = character(0) # nrow(cfg) == 0; format not found in cfg
    }
  }
  out = unname(if (!is.na(cfg['o'])) cfg['o'] else {
    if (!is.na(cfg['output'])) cfg['output'] else {
      with_ext(input, if (is.na(ext)) pandoc_ext(format) else ext)
    }
  })
  cfg = cfg[setdiff(names(cfg), c('o', 'output', 't'))]
  cmd = c(
    pandoc_arg(cfg), pandoc_arg(cmn), '-t', format, '-o', shQuote(out), shQuote(input)
  )
  message('Executing: pandoc ', paste(cmd, collapse = ' '))
  if (system2(exec, cmd) == 0L) out else stop('conversion failed')
}

# detect output format from config
pandoc_fmt = function(config) {
  fields = colnames(config)
  if (prod(dim(config)) == 0 || !('t' %in% fields)) return('html')
  na.omit(config[, 't'])
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

# give me a vector of arguments, I turn them into command-line
pandoc_arg = function(x) {
  if (length(x) == 0L || all(is.na(x))) return()
  x = x[!is.na(x)]  # options not provided
  nms = names(x)
  if (any(grepl('\n', x))) {
    # one argument used multiple times, e.g. --bibliography
    x = str_split(x, '\n')
    nms = rep(nms, sapply(x, length))
    x = unlist(x)
  }
  a1 = nchar(nms) == 1L
  paste0(ifelse(a1, '-', '--'), nms, ifelse(x == '', '', ifelse(a1, ' ', '=')), x)
}

# identify pandoc config in markdown comments
pandoc_cfg = function(x) {
  if (length(i1 <- grep('^<!--pandoc', x)) == 0L ||
        length(i2 <- grep('-->\\s*$', x)) == 0L) return(character(0))
  i1 = i1[1L]; if (all(i2 < i1)) return(character(0))
  i2 = i2[i2 >= i1][1L]
  cfg = x[i1:i2]
  cfg[1L] = gsub('^<!--pandoc\\s*', '', cfg[1L])
  cfg[length(cfg)] = gsub('-->\\s*$', '', cfg[length(cfg)])
  cfg
}
