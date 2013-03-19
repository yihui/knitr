## adapted from Hadley's decumar: https://github.com/hadley/decumar

## split input document into groups containing chunks and other texts
## (may contain inline R code)
split_file = function(lines, set.preamble = TRUE, patterns = knit_patterns$get()) {
  n = length(lines)
  chunk.begin = patterns$chunk.begin; chunk.end = patterns$chunk.end
  if (is.null(chunk.begin) || is.null(chunk.end))
    return(list(parse_inline(lines, patterns)))

  if (!child_mode() && set.preamble) {
    set_preamble(lines, patterns)  # prepare for tikz option 'standAlone'
  }

  blks = str_detect(lines, chunk.begin)
  txts = filter_chunk_end(blks, str_detect(lines, chunk.end))
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
    if (block) parse_block(g, patterns) else parse_inline(g, patterns)
  })
}

## a code manager to manage R code in all chunks
knit_code = new_defaults()

## strip the pattern in code
strip_block = function(x, prefix = NULL) {
  if (!is.null(prefix) && (length(x) > 1)) x[-1L] = str_replace(x[-1L], prefix, '')
  x
}

## an object to store chunk dependencies; dep_list$get() is of the form list(foo
## = c('chunk', 'labels', 'that', 'depend', 'on', 'chunk', 'foo'))
dep_list = new_defaults()

## separate params and R code in code chunks
parse_block = function(input, patterns) {
  block = strip_block(input, patterns$chunk.code)
  n = length(block); chunk.begin = patterns$chunk.begin
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
    if (label %in% names(knit_code$get())) stop("duplicate label '", label, "'")
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
parse_params = function(params) {

  params = gsub('^\\s*,*|,*\\s*$', '', params) # rm empty options
  if (params == '') return(list(label = unnamed_chunk()))

  res = withCallingHandlers(
    eval(parse(text = str_c("alist(", quote_label(params), ")"), srcfile = NULL)),
    error = function(e) {
      message('(*) NOTE: I saw chunk options "', params,
              '"\n please go to http://yihui.name/knitr/options',
              '\n (it is likely that you forgot to quote "character" options)')
    })

  # good, now you seem to be using valid R code
  idx = which(names(res) == '')  # which option is not named?
  # remove empty options
  for (i in idx) if (identical(res[[i]], alist(,)[[1]])) res[[i]] = NULL
  idx = if (is.null(names(res)) && length(res) == 1L) 1L else which(names(res) == '')
  if ((n <- length(idx)) > 1L || (length(res) > 1L && is.null(names(res))))
    stop("invalid chunk options: ", params,
         "\n(all options must be of the form 'tag=value' except the chunk label)")
  if (is.null(res$label)) {
    if (n == 0L) res$label = unnamed_chunk() else names(res)[idx] = 'label'
  }
  if (!is.character(res$label))
    res$label = gsub(' ', '', as.character(as.expression(res$label)))
  if (identical(res$label, '')) res$label = unnamed_chunk()
  res
}

# quote the chunk label if necessary
quote_label = function(x) {
  x = gsub('^\\s*,?', '', x)
  if (grepl('^\\s*[^\'"](,|\\s*$)', x)){
    # <<a,b=1>>= ---> <<'a',b=1>>=
    x = gsub('^\\s*([^\'"])(,|\\s*$)', "'\\1'\\2", x)
  } else if (grepl('^\\s*[^\'"](,|[^=]*(,|\\s*$))', x)) {
    # <<abc,b=1>>= ---> <<'abc',b=1>>=
    x = gsub('^\\s*([^\'"][^=]*)(,|\\s*$)', "'\\1'\\2", x)
  }
  x
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
    if (length(code) && !is_blank(code)) {
      cat("\n  ", str_pad(" R code chunk ", getOption('width') - 10L, 'both', '~'), "\n")
      cat(str_c('   ', code, collapse = '\n'), '\n')
      cat('  ', str_dup('~', getOption('width') - 10L), '\n')
    }
    timestamp()
  }
  cat('\n')
}

## extract inline R code fragments (as well as global options)
parse_inline = function(input, patterns) {
  input.src = input  # keep a copy of the source

  inline.code = patterns$inline.code; inline.comment = patterns$inline.comment
  if (!is.null(inline.comment)) {
    idx = str_detect(input, inline.comment)
    # strip off inline code
    input[idx] = str_replace_all(input[idx], inline.code, '\\1')
  }
  input = str_c(input, collapse = '\n') # merge into one line

  loc = cbind(start = numeric(0), end = numeric(0))
  if (group_pattern(inline.code)) loc = str_locate_all(input, inline.code)[[1]]
  if (nrow(loc)) {
    code = str_match_all(input, inline.code)[[1L]]
    code = if (NCOL(code) >= 2L) code[, NCOL(code)] else character(0)
  } else code = character(0)

  structure(list(input = input, input.src = input.src, location = loc, code = code),
            class = 'inline')
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
#' @references \url{http://yihui.name/knitr/demo/externalization/}
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
  read_chunk(paths[sans_ext(basename(paths)) == topic], ...)
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
parse_chunk = function(x, rc = knit_patterns$get('ref.chunk')) {
  if (length(x) == 0L) return(x)
  if (!group_pattern(rc) || !any(idx <- str_detect(x, rc))) return(x)
  labels = str_replace(x[idx], rc, '\\1')
  code = knit_code$get(labels)
  if (length(labels) <= 1L) code = list(code)
  x[idx] = unlist(lapply(code, function(z) {
    str_c(parse_chunk(z, rc), collapse = '\n')
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
