## adapted from Hadley's decumar: https://github.com/hadley/decumar

# split input document into groups containing chunks and other texts
# (may contain inline R code)
split_file = function(lines, set.preamble = TRUE, patterns = knit_patterns$get()) {
  n = length(lines)
  chunk.begin = patterns$chunk.begin; chunk.end = patterns$chunk.end
  if (is.null(chunk.begin) || is.null(chunk.end))
    return(list(parse_inline(lines, patterns)))

  if (!child_mode() && set.preamble) {
    set_preamble(lines, patterns)  # prepare for tikz option 'standAlone'
  }

  blks = grepl(chunk.begin, lines)
  txts = filter_chunk_end(blks, grepl(chunk.end, lines))
  # tmp marks the starting lines of a code/text chunk by TRUE
  tmp = blks | head(c(TRUE, txts), -1)

  groups = unname(split(lines, cumsum(tmp)))
  if (set.preamble)
    knit_concord$set(inlines = sapply(groups, length)) # input line numbers for concordance

  # parse 'em all
  lapply(groups, function(g) {
    block = grepl(chunk.begin, g[1])
    if (!set.preamble && !parent_mode()) {
      return(if (block) '' else g) # only need to remove chunks to get pure preamble
    }
    if (block) {
      n = length(g)
      # remove the optional chunk footer
      if (n >= 2 && grepl(chunk.end, g[n])) g = g[-n]
      # remove the optional prefix % in code in Rtex mode
      g = strip_block(g, patterns$chunk.code)
      params.src = if (group_pattern(chunk.begin)) {
        stringr::str_trim(gsub(chunk.begin, '\\1', g[1]))
      } else ''
      parse_block(g[-1], g[1], params.src)
    } else parse_inline(g, patterns)
  })
}

# a code manager to manage R code in all chunks
knit_code = new_defaults()

# strip the pattern in code
strip_block = function(x, prefix = NULL) {
  if (!is.null(prefix) && (length(x) > 1)) x[-1L] = sub(prefix, '', x[-1L])
  x
}

# an object to store chunk dependencies; dep_list$get() is of the form list(foo
# = c('chunk', 'labels', 'that', 'depend', 'on', 'chunk', 'foo'))
dep_list = new_defaults()

# separate params and R code in code chunks
parse_block = function(code, header, params.src) {
  params = params.src
  engine = 'r'
  # consider the syntax ```{engine, opt=val} for chunk headers
  if (out_format('markdown')) {
    engine = sub('^([a-zA-Z]+).*$', '\\1', params)
    params = sub('^([a-zA-Z]+)', '', params)
  }
  params = gsub('^\\s*,*|,*\\s*$', '', params) # rm empty options
  # turn ```{engine} into ```{r, engine="engine"}
  if (tolower(engine) != 'r') {
    params = sprintf('%s, engine="%s"', params, engine)
    params = gsub('^\\s*,\\s*', '', params)
  }

  params.src = params
  params = parse_params(params.src)
  # remove indent (and possibly markdown blockquote >) from code
  if (nzchar(spaces <- gsub('^([\t >]*).*', '\\1', header))) {
    params$indent = spaces
    code = gsub(sprintf('^%s', spaces), '', code)
  }

  label = params$label; .knitEnv$labels = c(.knitEnv$labels, label)
  if (length(code)) {
    if (label %in% names(knit_code$get())) {
      if (identical(getOption('knitr.duplicate.label'), 'allow')) {
        params$label = label = unnamed_chunk(label)
      } else stop("duplicate label '", label, "'")
    }
    knit_code$set(setNames(list(structure(code, chunk_opts = params)), label))
  }

  # store dependencies
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

# autoname for unnamed chunk
unnamed_chunk = function(prefix = NULL, i = chunk_counter()) {
  if (is.null(prefix)) prefix = opts_knit$get('unnamed.chunk.label')
  paste(prefix, i, sep = '-')
}

# parse params from chunk header
parse_params = function(params) {

  if (params == '') return(list(label = unnamed_chunk()))

  res = withCallingHandlers(
    eval(parse_only(paste('alist(', quote_label(params), ')'))),
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
    stop('invalid chunk options: ', params,
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
  if (grepl('^\\s*[^\'"](,|\\s*$)', x)) {
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
      cat('\n  ', stringr::str_pad(' R code chunk ', getOption('width') - 10L, 'both', '~'), '\n')
      cat(paste('  ', code, collapse = '\n'), '\n')
      cat('  ', stringr::str_dup('~', getOption('width') - 10L), '\n')
    }
    cat(paste('##------', date(), '------##'), sep = '\n')
  }
  cat('\n')
}

# extract inline R code fragments (as well as global options)
parse_inline = function(input, patterns) {
  input.src = input  # keep a copy of the source

  inline.code = patterns$inline.code; inline.comment = patterns$inline.comment
  if (!is.null(inline.comment)) {
    idx = grepl(inline.comment, input)
    # strip off inline code
    input[idx] = gsub(inline.code, '\\1', input[idx])
  }
  input = paste(input, collapse = '\n') # merge into one line

  loc = cbind(start = numeric(0), end = numeric(0))
  if (group_pattern(inline.code)) loc = stringr::str_locate_all(input, inline.code)[[1]]
  if (nrow(loc)) {
    code = stringr::str_match_all(input, inline.code)[[1L]]
    code = if (NCOL(code) >= 2L) {
      code[is.na(code)] = ''
      apply(code[, -1L, drop = FALSE], 1, paste, collapse = '')
    } else character(0)
  } else code = character(0)

  structure(list(input = input, input.src = input.src, location = loc, code = code),
            class = 'inline')
}

print.inline = function(x, ...) {
  if (nrow(x$location)) {
    cat('   ')
    if (opts_knit$get('verbose')) {
      cat(stringr::str_pad(' inline R code fragments ',
                  getOption('width') - 10L, 'both', '-'), '\n')
      cat(sprintf('    %s:%s %s', x$location[, 1], x$location[, 2], x$code),
          sep = '\n')
      cat('  ', stringr::str_dup('-', getOption('width') - 10L), '\n')
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
#' Use a special separator of the from \code{## ---- chunk-label} (at least four
#' dashes before the chunk label) in the script; (2) Manually specify the
#' labels, starting and ending positions of code chunks in the script.
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
#' ## ---- my-label ----
#' 1+1
#' lm(y~x, data=data.frame(x=1:10,y=rnorm(10)))
#'
#' ## later you can use <<my-label>>= to reference this chunk
#'
#' ## the 2nd approach
#' code = c("#@@a", '1+1', "#@@b", "#@@a", 'rnorm(10)', "#@@b")
#' read_chunk(lines = code, labels = 'foo') # put all code into one chunk named foo
#' read_chunk(lines = code, labels = 'foo', from = 2, to = 2) # line 2 into chunk foo
#' read_chunk(lines = code, labels = c('foo', 'bar'), from = c(1, 4), to = c(3, 6))
#' # automatically figure out 'to'
#' read_chunk(lines = code, labels = c('foo', 'bar'), from = c(1, 4))
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
  idx = cumsum(grepl(lab, lines))
  if (idx[1] == 0) {
    idx = c(0, idx); lines = c('', lines)  # no chunk header in the beginning
  }
  groups = unname(split(lines, idx))
  labels = stringr::str_trim(gsub(lab, '\\2', sapply(groups, `[`, 1)))
  labels = gsub(',.*', '', labels)  # strip off possible chunk options
  code = lapply(groups, strip_chunk)
  for (i in which(!nzchar(labels))) labels[i] = unnamed_chunk()
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
  while (is_blank(x[1])) {
    x = x[-1]; if (!length(x)) return(x)
  }
  while (is_blank(x[(n <- length(x))])) {
    x = x[-n]; if (n < 2) return(x)
  }
  x
}

# (recursively) parse chunk references inside a chunk
parse_chunk = function(x, rc = knit_patterns$get('ref.chunk')) {
  if (length(x) == 0L) return(x)
  x = c(x)  # drop attributes of code (e.g. chunk_opts)
  if (!group_pattern(rc) || !any(idx <- grepl(rc, x))) return(x)

  labels = sub(rc, '\\1', x[idx])
  code = knit_code$get(labels)
  indent = gsub('^(\\s*).*', '\\1', x[idx])
  if (length(labels) <= 1L) code = list(code)
  code = mapply(indent_block, code, indent, SIMPLIFY = FALSE, USE.NAMES = FALSE)

  x[idx] = unlist(lapply(code, function(z) {
    paste(parse_chunk(z, rc), collapse = '\n')
  }), use.names = FALSE)
  x
}

# filter chunk.end lines that don't actually end a chunk
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
#' The function \code{all_labels()} returns all chunk labels as a chracter
#' vector. Optionally, you can specify a series of conditions to filter the
#' labels. The function `all_rcpp_labels()` is a wrapper function for
#' \code{all_labels(engine == 'Rcpp')}.
#'
#' For example, suppose the condition expression is \code{engine == 'Rcpp'}, the
#' object \code{engine} is the local chunk option \code{engine}; if an
#' expression fails to be evaluated (e.g. when a certain object does not exist),
#' \code{FALSE} is returned and the label for this chunk will be filtered out.
#' @param ... a series of R expressions, each of which should return \code{TRUE}
#'   or \code{FALSE}; the expressions are evaluated using the local chunk
#'   options of each code chunk as the environment
#' @note Empty code chunks are always ignored, including those chunks that are
#'   empty originally in the document but filled with code using chunk options
#'   such as \code{ref.label} or \code{code}.
#' @return A character vector.
#' @export
#' @examples # the examples below are meaningless unless you put them in a knitr document
#' all_labels()
#' all_labels(engine == 'Rcpp')
#' all_labels(echo == FALSE && results != 'hide')
#' # or separate the two conditions
#' all_labels(echo == FALSE, results != 'hide')
all_labels = function(...) {
  cond = as.list(match.call())[-1]
  code = knit_code$get()
  labels = names(code)

  if (length(cond) == 0) return(labels)

  params = lapply(code, attr, 'chunk_opts')
  idx = rep_len(TRUE, length(labels))
  for (i in seq_along(cond)) {
    for (j in seq_along(params)) {
      # need tryCatch() because the expression cond[[i]] may trigger an error
      # when any variable is not found, e.g. not all chunks have the engine
      # option when the condition is engine == 'Rcpp'
      try_eval = function(expr) tryCatch(
        eval(expr, envir = params[[j]], enclos = knit_global()),
        error = function(e) FALSE
      )
      if (idx[j]) {
        res = try_eval(cond[[i]])
        # the condition could be evaluated to an expression; see all_rcpp_labels()
        if (is.expression(res)) res = try_eval(res)
        idx[j] = res
      }
    }
  }

  labels[idx]
}

#' @rdname all_labels
#' @export
all_rcpp_labels = function(...) all_labels(expression(engine == 'Rcpp'), ...)

#' Wrap code using the inline R expression syntax
#'
#' This is a convenience function to write the "source code" of inline R
#' expressions. For example, if you want to write \samp{`r 1+1`} literally in an
#' R Markdown document, you may write \samp{`` `r knitr::inline_expr('1+1')`
#' ``}; for Rnw documents, this may be
#' \samp{\verb|\Sexpr{knitr::inline_expr{'1+1'}}|}.
#' @param code a character string of the inline R source code
#' @param syntax a character string to specify the syntax, e.g. \code{rnw},
#'   \code{html}, or \code{md}, etc; if not specified, it will be guessed from
#'   the knitting context
#' @return A character string marked up using the inline R code syntax.
#' @export
#' @examples library(knitr)
#' inline_expr('1+1', 'rnw'); inline_expr('1+1', 'html'); inline_expr('1+1', 'md')
inline_expr = function(code, syntax) {
  if (!is.character(code) || length(code) != 1)
    stop('The inline code must be a character string')
  if (!missing(syntax)) pat = syntax else {
    inline = knit_patterns$get('inline.code')
    if (is.null(inline)) stop('inline_expr() must be called in a knitting process')
    pat = NULL
      for (i in names(all_patterns)) {
        if (inline == all_patterns[[i]][['inline.code']]) {
          pat = i; break
        }
      }
  }
  if (is.null(pat)) stop('Unknown document format')
  sprintf(switch(
    pat, rnw = '\\Sexpr{%s}', tex = '\\rinline{%s}', html = '<!--rinline %s -->',
    md = '`r %s`', rst = ':r:`%s`', asciidoc = '`r %s`', textile = '@r %s@',
    stop('Unknown syntax ', pat)
  ), code)
}
