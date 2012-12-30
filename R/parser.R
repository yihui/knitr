## adapted from Hadley's decumar: https://github.com/hadley/decumar

## split input document into groups containing chunks and other texts
## (may contain inline R code)
split_file = function(lines, set.preamble = TRUE) {
  n = length(lines)
  chunk.begin = knit_patterns$get('chunk.begin')
  chunk.end = knit_patterns$get('chunk.end')
  if (is.null(chunk.begin) || is.null(chunk.end)) {
    return(list(parse_inline(lines)))
  }

  if (!child_mode() && set.preamble) {
    set_preamble(lines)  # prepare for tikz option 'standAlone'
  }

  blks = str_detect(lines, chunk.begin)
  txts = str_detect(lines, chunk.end)
  if (opts_knit$get('filter.chunk.end')) txts = filter_chunk_end(blks, txts)

  tmp = logical(n); tmp[blks | txts] = TRUE; lines[txts] = ''

  groups = unname(split(lines, cumsum(tmp)))
  if (set.preamble)
    knit_concord$set(inlines = sapply(groups, length)) # input line numbers for concordance

  ## parse 'em all
  lapply(groups, function(g) {
    block = str_detect(g[1], chunk.begin)
    if (!set.preamble && !parent_mode()) {
      return(if (block) '' else g) # only need to remove chunks to get pure preamble
    }
    if (block) parse_block(g) else parse_inline(g)
  })
}

## a code manager to manage R code in all chunks
knit_code = new_defaults()

## strip the pattern in code
strip_block = function(x) {
  if (!is.null(prefix <- knit_patterns$get('chunk.code')) && (n <- length(x)) > 1) {
    x[-1L] = str_replace(x[-1L], prefix, "")
  }
  x
}

## an object to store chunk dependencies; dep_list$get() is of the form list(foo
## = c('chunk', 'labels', 'that', 'depend', 'on', 'chunk', 'foo'))
dep_list = new_defaults()

## separate params and R code in code chunks
parse_block = function(input) {
  block = strip_block(input)
  n = length(block); chunk.begin = knit_patterns$get('chunk.begin')
  params.src = if (group_pattern(chunk.begin)) {
    str_trim(gsub(chunk.begin, '\\1', block[1]))
  } else ''
  params = parse_params(params.src)
  if (nzchar(spaces <- gsub('^(\\s*).*', '\\1', block[1]))) {
    params$indent = spaces
    block = gsub(str_c('^', spaces), '', block) # remove indent for the whole chunk
  }

  label = params$label; .knitEnv$labels = c(.knitEnv$labels, label)
  code = block[-1L]
  if (length(code)) {
    if (label %in% names(knit_code$get())) stop("duplicated label '", label, "'")
    knit_code$set(setNames(list(code), label))
  }

  ## store dependencies
  if (!is.null(deps <- params$dependson)) {
    deps = sc_split(deps)
    if (is.numeric(deps)) {
      deps[deps < 0] = length(.knitEnv$labels) + deps[deps < 0]
      deps = .knitEnv$labels[deps[deps > 0]]
    }
    for (i in deps)
      dep_list$set(setNames(list(c(dep_list$get(i), label)), i))
  }

  structure(list(params = params, params.src = params.src), class = 'block')
}

## autoname for unnamed chunk
unnamed_chunk = function() str_c('unnamed-chunk-', chunk_counter())

## parse params from chunk header
parse_params = function(params, label = TRUE) {
  # TODO: remove support for label = FALSE here
  if (!label) {
    reminder('it is recommended to set global chunk options via opts_chunk$set(', params, ')
      instead of the ', knit_patterns$get('global.options'), 'syntax; the old syntax will be deprecated soon')
  }
  if (is_blank(params)) {
    return(if (!label) list() else list(label = unnamed_chunk()))
  }
  res = try(eval(parse(text = str_c("alist(", params, ")"))))
  if (!inherits(res, 'try-error') && valid_opts(params)) {
    ## good, you seem to be using valid R code
    idx = which(names(res) == '')  # which option is not named?
    if (is.null(names(res))) idx = 1L  # empty name, must be label
    if ((n <- length(idx)) > 1L) {
      stop("invalid chunk options: ", params,
           "\n(all options must be of the form 'tag=value' except the chunk label)")
    } else if (!label && n > 0L) stop('all global options must be of the form tag=value')
    if (n == 1L) names(res)[idx] = 'label' else if (label) {
      if (!('label' %in% names(res))) res$label = unnamed_chunk()
    }
    if (label && !is.character(res$label))
      res$label = gsub(' ', '', as.character(as.expression(res$label)))
    if (identical(res$label, '')) res$label = unnamed_chunk()
    return(res)
  }
  reminder('(*) NOTE: I saw options "', params,
          '"\n are you using the old Sweave syntax? go http://yihui.name/knitr/options',
          '\n (it is likely that you forgot to quote "character" options)')

  ## split by , (literal comma has to be escaped as \,) and then by =
  pieces = str_split(params, perl('(?<=[^\\\\]),'))[[1]]
  pieces = str_split(str_replace_all(pieces, fixed('\\,'), ','), '=', n = 2L)
  n = sapply(pieces, length)
  ## when global options are empty
  if (length(n) == 1 && length(pieces[[1]]) == 1) {
    return(if (label) list(label = pieces[[1]]) else list())
  }

  if (any(n == 1)) {
    if (label && length(idx <- which(n == 1)) == 1) {
      pieces[[idx]] = c('label', pieces[[idx]])
    } else stop("illegal tags in: ", params, "\n",
                "all options must be of the form 'tag=value' except the chunk label",
                call. = FALSE)
  } else if (label && !str_detect(params, '\\s*label\\s*=')) {
    pieces[[length(pieces) + 1]] = c('label', unnamed_chunk())
  }

  values = lapply(pieces, function(x) str_trim(x[2]))
  names(values) = str_trim(tolower(lapply(pieces, `[`, 1)))

  lapply(values, type.convert, as.is = TRUE)
}

## is the options list valid with knitr's new syntax?
.wrong.opts = c('results\\s*=\\s*(verbatim|tex|hide|asis|markup)',
                'fig.keep\\s*=\\s*(none|all|high|last|first)',
                'fig.show\\s*=\\s*(hold|asis|animate)',
                sprintf('dev\\s*=\\s*(%s)', paste(names(auto_exts), collapse = '|')),
                'fig.align\\s*=\\s*(default|left|center|right)')
valid_opts = function(x) {
  ## not a rigorous check; you should go to the new syntax finally!
  !any(str_detect(x, .wrong.opts))
}

print.block = function(x, ...) {
  params = x$params
  cat('label:', params$label)
  if (length(params) > 1L) {
    cat(' (with options) \n')
    str(params[setdiff(names(params), 'label')])
  }
  if (opts_knit$get('verbose')) {
    code = knit_code$get(params$label)
    if (length(code) && !all(is_blank(code))) {
      cat("\n  ", str_pad(" R code chunk ", getOption('width') - 10L, 'both', '~'), "\n")
      cat(str_c('   ', code, collapse = '\n'), '\n')
      cat('  ', str_dup('~', getOption('width') - 10L), '\n')
    }
    timestamp()
  }
  cat('\n')
}

## extract inline R code fragments (as well as global options)
parse_inline = function(input) {
  input.src = input  # keep a copy of the source
  inline.comment = knit_patterns$get('inline.comment')
  if (!is.null(inline.comment)) {
    idx = str_detect(input, inline.comment)
    # strip off inline code
    input[idx] = str_replace_all(input[idx], knit_patterns$get('inline.code'), '\\1')
  }
  input = str_c(input, collapse = '\n') # merge into one line

  locate_inline = function(input, pattern) {
    x = cbind(start = numeric(0), end = numeric(0))
    if (group_pattern(pattern))
      x = str_locate_all(input, pattern)[[1]]
    x
  }

  params = list(); global.options = knit_patterns$get('global.options')
  opts.line = locate_inline(input, global.options)
  if (nrow(opts.line)) {
    last = tail(opts.line, 1)
    opts = str_match(str_sub(input, last[1, 1], last[1, 2]), global.options)[, 2]
    params = parse_params(opts, label = FALSE)
    ## remove texts for global options
    text.line = t(matrix(c(1L, t(opts.line) + c(-1L, 1L), str_length(input)), nrow = 2))
    text.line = text.line[text.line[, 1] <= text.line[, 2], , drop = FALSE]
    input = str_c(str_sub(input, text.line[, 1], text.line[, 2]), collapse = '')
  }
  res1 = extract_inline(input, 'inline.code', locate_inline)
  res2 = extract_inline(input, 'input.doc', locate_inline)
  if (length(res2$code)) {
    reminder('please use the chunk option to input child documents: child=',
             sprintf('c(%s)',  str_c('"', res2$code, '"', collapse = ', ')))
    res2$code = sprintf("knit_child('%s')", res2$code)  # input child with knit_child()
  }
  loc = rbind(res1$location, res2$location)
  idx = order(loc[, 1L])

  structure(list(input = input, input.src = input.src, location = loc[idx, , drop = FALSE],
                 params = params, code = c(res1$code, res2$code)[idx]),
            class = 'inline')
}

## locate and extract inline R code
extract_inline = function(input, pat.name, locate.fun) {
  pattern = knit_patterns$get(pat.name)
  loc = locate.fun(input, pattern)
  code = character(0)
  if (nrow(loc)) code = str_match(str_sub(input, loc[, 1L], loc[, 2L]), pattern)
  code = if (NCOL(code) >= 2L) code[, NCOL(code)] else character(0)
  list(location = loc, code = code)
}

print.inline = function(x, ...) {
  if (nrow(x$location)) {
    cat('   ')
    if (opts_knit$get('verbose')) {
      cat(str_pad(" inline R code fragments ",
                  getOption('width') - 10L, 'both', '-'), '\n')
      cat(sprintf('    %s:%s %s', x$location[, 1], x$location[, 2], x$code),
          sep = '\n')
      cat('  ', str_dup('-', getOption('width') - 10L), '\n')
    } else cat('inline R code fragments\n')
  } else cat('  ordinary text without R code\n')
  cat('\n')
}

#' Read chunks from an external script
#'
#' Chunks can be put in an external script, and this function reads chunks into
#' the current \pkg{knitr} session; \code{read_demo()} is a convenience function
#' to read a demo script from a package.
#'
#' There are two approaches to read external code into the current session: (1)
#' Use a special separator of the from \code{## @@knitr chunk-label} in the
#' script; (2) Manually specify the labels, starting and ending positions of
#' code chunks in the script.
#'
#' The second approach will be used only when \code{labels} is not \code{NULL}.
#' For this approach, if \code{from} is \code{NULL}, the starting position is 1;
#' if \code{to} is \code{NULL}, each of its element takes the next element of
#' \code{from} minus 1, and the last element of \code{to} will be the length of
#' \code{lines} (e.g. when \code{from = c(1, 3, 8)} and the script has 10 lines
#' in total, \code{to} will be \code{c(2, 7, 10)}). Alternatively, \code{from}
#' and \code{to} can be character vectors as regular expressions to specify the
#' positions; when their length is 1, the single regular expression will be
#' matched against the \code{lines} vector, otherwise each element of
#' \code{from}/\code{to} is matched against \code{lines} and the match is
#' supposed to be unique so that the numeric positions returned from
#' \code{grep()} will be of the same length of \code{from}/\code{to}. Note
#' \code{labels} always has to match the length of \code{from} and \code{to}.
#' @param path the path to the R script
#' @param lines a character vector of the code lines (by default read from
#'   \code{path})
#' @param labels a character vector of chunk labels (default \code{NULL})
#' @param from,to a numeric vector specifying the starting/ending line numbers
#'   of code chunks, or a character vector; see Details
#' @param from.offset,to.offset an offset to be added to \code{from}/\code{to}
#' @return As a side effect, code chunks are read into the current session so
#'   that future chunks can (re)use the code by chunk label references.
#' @references \url{http://yihui.name/knitr/demo/reference/}
#' @note This function can only be used in a chunk which is \emph{not} cached
#'   (chunk option \code{cache = FALSE}), and the code is read and stored in the
#'   current session \emph{without} being executed (to actually run the code,
#'   you have to use a chunk with a corresponding label).
#' @author Yihui Xie; the idea of the second approach came from Peter
#'   Ruckdeschel (author of the \pkg{SweaveListingUtils} package)
#' @export
#' @examples ## put this in foo.R and read_chunk('foo.R')
#'
#' ## @@knitr my-label
#' 1+1
#' lm(y~x, data=data.frame(x=1:10,y=rnorm(10)))
#'
#' ## later you can use <<my-label>>= to reference this chunk
#'
#' ## the 2nd approach
#' code = c("#@@a", '1+1', "#@@b", "#@@a", 'rnorm(10)', "#@@b")
#' read_chunk(lines = code, labels = 'foo') # put all code into one chun named foo
#' read_chunk(lines = code, labels = 'foo', from = 2, to = 2) # line 2 into chunk foo
#' read_chunk(lines = code, labels = c('foo', 'bar'), from = c(1, 4), to = c(3, 6))
#' read_chunk(lines = code, labels = c('foo', 'bar'), from = c(1, 4)) # automatically figure out 'to'
#' read_chunk(lines = code, labels = c('foo', 'bar'), from = "^#@@a", to = "^#@@b")
#' read_chunk(lines = code, labels = c('foo', 'bar'), from = "^#@@a", to = "^#@@b", from.offset = 1, to.offset = -1)
#'
#' ## later you can use, e.g., <<foo>>=
#' knitr:::knit_code$get() # use this to check chunks in the current session
#' knitr:::knit_code$restore() # clean up the session
read_chunk = function(path, lines = readLines(path, warn = FALSE),
                      labels = NULL, from = NULL, to = NULL, from.offset = 0L, to.offset = 0L) {
  if (!length(lines)) {
    warning('code is empty')
    return(invisible())
  }
  lab = .sep.label
  if (is.null(labels)) {
    if (!group_pattern(lab)) return(invisible())
  } else {
    if (is.null(from)) from = 1L
    if (!is.numeric(from)) from = pattern_index(from, lines)
    if (is.null(to)) to = c(from[-1L] - 1L, length(lines))
    if (!is.numeric(to)) to = pattern_index(to, lines)
    stopifnot(length(labels) == length(from), length(from) == length(to))
    from = from + from.offset; to = to + to.offset
    code = list()
    for (i in seq_along(labels)) {
      code[[labels[i]]] = strip_white(lines[from[i]:to[i]])
    }
    knit_code$set(code)
    return(invisible())
  }
  idx = cumsum(str_detect(lines, lab))
  if (all(idx == 0)) return(invisible())
  groups = unname(split(lines[idx != 0], idx[idx != 0]))
  labels = str_trim(str_replace(sapply(groups, `[`, 1), lab, '\\1'))
  code = lapply(groups, strip_chunk)
  idx = nzchar(labels); code = code[idx]; labels = labels[idx]
  knit_code$set(setNames(code, labels))
}
#' @rdname read_chunk
#' @param topic,package name of the demo and the package see \code{\link[utils]{demo}}
#' @param ... arguments to be passed to \code{\link{read_chunk}}
#' @export
read_demo = function(topic, package = NULL, ...) {
  paths = list.files(file.path(find.package(package), 'demo'), full.names = TRUE)
  read_chunk(paths[file_path_sans_ext(basename(paths)) == topic], ...)
}

# convert patterns to numeric indices in a character vector
pattern_index = function(pattern, text) {
  if (length(pattern) == 1L) {
    idx = grep(pattern, text)
    if (length(idx) == 0L) stop('pattern ', pattern, ' not found')
    return(idx)
  }
  sapply(pattern, function(p) {
    idx = grep(p, text)
    if (length(idx) != 1L) stop('non-unique matches of ', p)
    idx
  })
}

strip_chunk = function(x) strip_white(x[-1])
# strip lines that are pure white spaces
strip_white = function(x) {
  if (!length(x)) return(x)
  while(is_blank(x[1])) {
    x = x[-1]; if (!length(x)) return(x)
  }
  while(is_blank(x[(n <- length(x))])) {
    x = x[-n]; if (n < 2) return(x)
  }
  x
}

## (recursively) parse chunk references inside a chunk
parse_chunk = function(x) {
  rc = knit_patterns$get('ref.chunk')
  if (!group_pattern(rc) || !any(idx <- str_detect(x, rc))) return(x)
  labels = str_replace(x[idx], rc, '\\1')
  code = knit_code$get(labels)
  if (length(labels) <= 1L) code = list(code)
  x[idx] = unlist(lapply(code, function(z) {
    str_c(parse_chunk(z), collapse = '\n')
  }), use.names = FALSE)
  x
}

## filter chunk.end lines that don't actually end a chunk
filter_chunk_end = function(chunk.begin, chunk.end) {
  in.chunk = FALSE
  fun = function(is.begin, is.end) {
    if (in.chunk && is.end) {
      in.chunk <<- FALSE
      return(TRUE)
    }
    if (!in.chunk && is.begin) in.chunk <<- TRUE
    FALSE
  }
  mapply(fun, chunk.begin, chunk.end)
}

#' Get all chunk labels in a document
#'
#' This function returns all chunk labels as a chracter vector.
#' @return A character vector.
#' @export
all_labels = function() names(knit_code$get())
