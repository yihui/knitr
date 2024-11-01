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

  markdown_mode = identical(patterns, all_patterns$md)
  groups = divide_chunks(lines, chunk.begin, chunk.end, markdown_mode)

  if (set.preamble)
    knit_concord$set(inlines = sapply(groups, length)) # input line numbers for concordance

  # parse 'em all
  lapply(seq_along(groups), function(i) {
    knit_concord$set(block = i)
    g = groups[[i]]
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
        extract_params_src(chunk.begin, g[1])
      } else ''
      parse_block(g[-1], g[1], params.src, markdown_mode)
    } else parse_inline(g, patterns)
  })
}

# divide lines of input into code/text chunks
divide_chunks = function(x, begin, end, md = TRUE) {
  i = group_indices(grepl(begin, x), grepl(end, x), x, md)
  unname(split(x, i))
}

extract_params_src = function(chunk.begin, line) {
  trimws(gsub(chunk.begin, '\\1', line))
}

#' The code manager to manage code in all chunks
#'
#' This object provides methods to manage code (as character vectors) in all
#' chunks in \pkg{knitr} source documents. For example,
#' \code{knitr::knit_code$get()} returns a named list of all code chunks (the
#' names are chunk labels), and \code{knitr::knit_code$get('foo')} returns the
#' character vector of the code in the chunk with the label \code{foo}.
#' @note The methods on this object include the \code{set()} method (i.e., you
#'   could do something like \code{knitr::knit_code$set(foo = "'my precious new
#'   code'")}), but we recommend that you do not use this method to modify the
#'   content of code chunks, unless you are
#'   \href{https://emitanaka.rbind.io/post/knitr-knitr-code/}{as creative as Emi
#'   Tanaka} and know what you are doing.
#' @export
knit_code = new_defaults()

# strip the pattern in code
strip_block = function(x, prefix = NULL) {
  if (!is.null(prefix) && (length(x) > 1)) {
    x[-1L] = sub(prefix, '', x[-1L])
    spaces = min(attr(regexpr("^ *", x[-1L]), "match.length"))
    if (spaces > 0) x[-1L] = substring(x[-1L], spaces + 1)
  }
  x
}

# an object to store chunk dependencies; dep_list$get() is of the form list(foo
# = c('chunk', 'labels', 'that', 'depend', 'on', 'chunk', 'foo'))
dep_list = new_defaults()

# separate params and R code in code chunks
parse_block = function(code, header, params.src, markdown_mode = out_format('markdown')) {
  params = params.src
  engine = 'r'
  # consider the syntax ```{engine, opt=val} for chunk headers
  if (markdown_mode) {
    engine = get_chunk_engine(params)
    params = get_chunk_params(params)
  }
  params = clean_empty_params(params) # rm empty options
  # turn ```{engine} into ```{r, engine="engine"}
  if (tolower(engine) != 'r') {
    params = sprintf('%s, engine="%s"', params, engine)
    params = gsub('^\\s*,\\s*', '', params)
  }

  # for quarto, preserve the actual original params.src and do not remove the engine
  if (!is_quarto() || opts_knit$get('tangle')) params.src = params
  params = xfun::csv_options(params)
  if (is.null(params$label)) params$label = unnamed_chunk()

  # remove indent (and possibly markdown blockquote >) from code
  if (nzchar(spaces <- get_chunk_indent(header))) {
    params$indent = spaces
    code = gsub(sprintf('^%s', spaces), '', code)
    # in case the trailing spaces of the indent string are trimmed on certain
    # lines (e.g. in blockquotes https://github.com/yihui/knitr/issues/1446)
    code = gsub(sprintf('^%s', gsub('\\s+$', '', spaces)), '', code)
  }

  # merge with possible chunk options written as (YAML or CSV) metadata in
  # chunk, and remove metadata from code body
  parts = partition_chunk(engine, code)
  params = merge_list(params, parts$options)
  code = parts$code

  label = params$label; .knitEnv$labels = c(.knitEnv$labels, label)
  if (length(code) || length(params[['file']]) || length(params[['code']])) {
    if (label %in% names(knit_code$get())) {
      if (identical(getOption('knitr.duplicate.label'), 'allow')) {
        params$label = label = unnamed_chunk(label)
      } else stop(
        "Duplicate chunk label '", label, "', which has been used for the chunk:\n",
        one_string(knit_code$get(label))
      )
    }
    code = as.character(code)
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

  # for quarto only
  if (is_quarto()) {
    params$original.params.src = params.src
    params$chunk.echo = isTRUE(params[['echo']])
    params$yaml.code = parts$src
    attr(params, 'quarto_options') = c('original.params.src', 'chunk.echo', 'yaml.code')
    # alias 'warning' explicitly set in chunk metadata to the 'message' option
    if (!is.null(parts$options[['warning']])) {
      params$message = parts$options[['warning']]
    }
  }

  structure(class = 'block', list(
    params = params, params.src = params.src, params.chunk = parts$src)
  )
}

get_chunk_indent = function(header) {
  gsub('^([\t >]*).*', '\\1', header)
}

get_chunk_engine = function(params) {
  sub('^([a-zA-Z0-9_]+).*$', '\\1', params)
}

get_chunk_params = function(params) {
  sub('^([a-zA-Z0-9_]+)', '', params)
}

clean_empty_params = function(params) {
  gsub('^\\s*,*\\s*|\\s*,*\\s*$', '', params) # rm empty options
}

# autoname for unnamed chunk
unnamed_chunk = function(prefix = NULL, i = chunk_counter()) {
  if (is.null(prefix)) prefix = opts_knit$get('unnamed.chunk.label')
  paste(prefix, i, sep = '-')
}

# this internal function is still used in RStudio IDE, otherwise can be removed
parse_params = function(params, label = TRUE) {
  res = xfun::csv_options(params)
  if (label && (!is.character(res$label) || identical(res$label, '')))
    res$label = unnamed_chunk()
  res
}

#' Partition chunk options from the code chunk body
#'
#' This is a wrapper function calling \code{xfun::\link[xfun]{divide_chunk}()}
#' under the hood.
#' @export
#' @keywords internal
partition_chunk = function(engine, code) {
  opts = options(xfun.handle_error.loc_fun = get_loc)
  on.exit(options(opts))
  # the code has been moved to the xfun package
  xfun::divide_chunk(engine, code)
}

print_block = function(x) {
  params = x$params
  if (opts_knit$get('verbose')) {
    code = knit_code$get(params$label)
    if (length(code) && !is_blank(code)) {
      cat('\n')
      cat(one_string('  |  ', code), '\n')
    }
  }
}

# extract inline R code fragments (as well as global options)
parse_inline = function(input, patterns) {
  inline.code = patterns$inline.code; inline.comment = patterns$inline.comment
  if (!is.null(inline.comment)) {
    idx = grepl(inline.comment, input)
    # strip off inline code
    input[idx] = gsub(inline.code, '\\1', input[idx])
  }
  input = one_string(input) # merge into one line

  loc = cbind(start = numeric(0), end = numeric(0))
  if (group_pattern(inline.code)) loc = str_locate(input, inline.code)[[1]]
  code1 = code2 = character()
  if (nrow(loc)) {
    code = t(str_match(input, inline.code))
    if (NCOL(code) >= 2L) {
      code1 = code[, 1L]
      code2 = apply(code[, -1L, drop = FALSE], 1, paste, collapse = '')
    }
  }

  structure(
    list(input = input, location = loc, code = code2, code.src = code1),
    class = 'inline'
  )
}

print_inline = function(x) {
  if (opts_knit$get('verbose')) {
    cat('\n')
    if (nrow(x$location)) {
      cat(sprintf('  |  %s  #%s:%s', x$code, x$location[, 1], x$location[, 2]), sep = '\n')
    }
  }
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
#' @param path Path to the R script.
#' @param lines Character vector of lines of code. By default, this is read from
#'   \code{path}.
#' @param labels Character vector of chunk labels (default \code{NULL}).
#' @param from,to Numeric vector specifying the starting/ending line numbers of
#'   code chunks, or a character vector; see Details.
#' @param from.offset,to.offset Offsets to be added to \code{from}/\code{to}.
#' @param roxygen_comments Logical dictating whether to keep trailing
#'   roxygen-style comments from code chunks in addition to whitespace
#' @return As a side effect, code chunks are read into the current session so
#'   that future chunks can (re)use the code by chunk label references. If an
#'   external chunk has the same label as a chunk in the current session, chunk
#'   label references by future chunks will refer to the external chunk.
#' @references \url{https://yihui.org/knitr/demo/externalization/}
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
#' knitr::knit_code$get() # use this to check chunks in the current session
#' knitr::knit_code$restore() # clean up the session
read_chunk = function(
  path, lines = read_utf8(path), labels = NULL, from = NULL, to = NULL,
  from.offset = 0L, to.offset = 0L, roxygen_comments = TRUE
) {
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
  labels = trimws(gsub(lab, '\\3', sapply(groups, `[`, 1)))
  labels = gsub(',.*', '', labels)  # strip off possible chunk options
  code = lapply(groups, strip_chunk, roxygen_comments)
  for (i in which(!nzchar(labels))) labels[i] = unnamed_chunk()
  knit_code$set(setNames(code, labels))
}

#' @rdname read_chunk
#' @param topic,package Name of the demo and the package. See
#'   \code{utils::\link{demo}}.
#' @param ... Arguments passed to \code{\link{read_chunk}}.
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

strip_chunk = function(x, roxygen_comments = TRUE) {
  x = x[-1]
  strip_white(x, if (roxygen_comments) is_blank else function(line) {
    is_blank(line) || grepl("^#+'[ ]?", line)
  })
}

# strip lines that are pure white spaces or
# that match the test_strip condition(s)
strip_white = function(x, test_strip = is_blank) {
  if (!length(x)) return(x)
  while (test_strip(x[1])) {
    x = x[-1]; if (!length(x)) return(x)
  }
  while (test_strip(x[(n <- length(x))])) {
    x = x[-n]; if (n < 2) return(x)
  }
  x
}

# (recursively) parse chunk references inside a chunk
parse_chunk = function(x, rc = knit_patterns$get('ref.chunk')) {
  if (length(x) == 0L) return(x)
  x = c(x)  # drop attributes of code (e.g. chunk_opts)
  if (!group_pattern(rc) || length(idx <- grep(rc, x)) == 0) return(x)

  labels = sub(rc, '\\1', x[idx])
  code = knit_code$get()
  i = labels %in% names(code)
  idx = idx[i]; code = code[labels[i]]
  indent = gsub('^(\\s*).*', '\\1', x[idx])
  code = mapply(indent_block, code, indent, SIMPLIFY = FALSE, USE.NAMES = FALSE)

  x = as.list(x)
  x[idx] = lapply(code, function(z) parse_chunk(z, rc))
  unlist(x, use.names = FALSE)
}

# split text lines into groups of code and text chunks
group_indices = function(chunk.begin, chunk.end, lines = NA, is.md = FALSE) {
  in.chunk = FALSE  # whether inside a chunk now
  pattern.end = NA  # the expected chunk end pattern (derived from header)
  b = NA  # the last found chunk header
  g = NA  # group index: odd - text; even - chunk
  fun = function(is.begin, is.end, line, i) {
    if (i == 1) {
      g <<- if (is.begin) {
        in.chunk <<- TRUE
        b <<- i
        0
      } else 1
      return(g)
    }
    # begin of another chunk is found while the previous chunk is not complete yet
    if (in.chunk && is.begin) {
      if (!is.md || match_chunk_begin(pattern.end, line)) {
        g <<- g + 2  # same amount of ` as previous chunk, so should be a new chunk
        if (is.md) b <<- i
      }  # otherwise ignore the chunk header
      return(g)
    }
    if (in.chunk && is.end && match_chunk_end(pattern.end, line, i, b, lines)) {
      in.chunk <<- FALSE
      g <<- g + 1
      return(g - 1)  # don't use incremented g yet; use it in the next step
    }
    if (!in.chunk && is.begin) {
      in.chunk <<- TRUE
      if (is.md) {
        pattern.end <<- sub('(^[\t >]*```+).*', '^\\1\\\\s*$', line)
        b <<- i
      }
      g <<- g + 2 - g%%2  # make sure g is even
    }
    g
  }
  mapply(fun, chunk.begin, chunk.end, lines, seq_along(chunk.begin))
}

match_chunk_begin = function(pattern.end, x, pattern = '^\\1\\\\{') {
  grepl(gsub('^([^`]*`+).*', pattern, pattern.end), x)
}

match_chunk_end = function(pattern, line, i, b, lines) {
  if (is.na(pattern) || grepl(pattern, line)) return(TRUE)
  n = length(lines)
  # if the exact match was not found, look ahead to see if there is another
  # chunk end that is an exact match before the next chunk begin
  if (i < n && length(k <- grep(pattern, lines[(i + 1):n]))) {
    k = k[1]
    if (k == 1) return(FALSE)  # the next line is real chunk end
    # no other chunk headers before the new next exact chunk end
    if (!any(match_chunk_begin(pattern, lines[i + 1:(k - 1)], '^\\1`*\\\\{')))
      return(FALSE)
  }
  stop2(
    'The closing fence on line ', i, ' ("', line, '") in ', current_input(),
    ' does not match the opening fence "',
    gsub('\\^(\\s*`+).*', '\\1', pattern), '" on line ', b, '. You are recommended to ',
    'fix either the opening or closing fence of the code chunk to use exactly ',
    'the same numbers of backticks and same level of indentation (or blockquote). ',
    'See https://yihui.org/en/2021/10/unbalanced-delimiters/ for more info.'
  )
}

#' Get all chunk labels in a document
#'
#' The function \code{all_labels()} returns all chunk labels as a character
#' vector. Optionally, you can specify a series of conditions to filter the
#' labels. The function `all_rcpp_labels()` is a wrapper function for
#' \code{all_labels(engine == 'Rcpp')}.
#'
#' For example, suppose the condition expression is \code{engine == 'Rcpp'}, the
#' object \code{engine} is the local chunk option \code{engine}. If an
#' expression fails to be evaluated (e.g. when a certain object does not exist),
#' \code{FALSE} is returned and the label for this chunk will be filtered out.
#' @param ... A vector of R expressions, each of which should return \code{TRUE}
#'   or \code{FALSE}. The expressions are evaluated using the \emph{local} chunk
#'   options of each code chunk as the environment, which means global chunk
#'   options are not considered when evaluating these expressions. For example,
#'   if you set the global chunk option \code{opts_chunk$set(purl = TRUE)},
#'   \code{all_labels(purl == TRUE)} will \emph{not} return the labels of all
#'   code chunks, but will only return the labels of those code chunks that have
#'   local chunk options \code{purl = TRUE}.
#' @note Empty code chunks are always ignored, including those chunks that are
#'   empty in the original document but filled with code using chunk options
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
#' @param code Character string of the inline R source code.
#' @param syntax A character string to specify the syntax, e.g. \code{rnw},
#'   \code{html}, or \code{md}. If not specified, this will be guessed from
#'   the knitting context.
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


#' Convert the in-header chunk option syntax to the in-body syntax
#'
#' This is a helper function for moving chunk options from the chunk header to
#' the chunk body using the new syntax.
#' @param input File path to the document with code chunks to convert.
#' @param output The default \code{NULL} will output to console. Other values
#'   can be a file path to write the converted content into or a function which
#'   takes \code{input} as argument and returns a file path to write into (e.g.,
#'   \code{output = identity} to overwrite the input file).
#' @param type This determines how the in-body options will be formatted.
#'   \code{"mutiline"} (the default, except for \file{qmd} documents, for which
#'   the default is \code{"yaml"}) will write each chunk option on a separate
#'   line. Long chunk option values will be wrapped onto several lines, and you
#'   can use \code{width = 0} to keep one line per option only. \code{"wrap"}
#'   will wrap all chunk options together using
#'   \code{\link[base:strwrap]{base::strwrap}()}. \code{"yaml"} will convert
#'   chunk options to YAML.
#' @param width An integer passed to \code{base::strwrap()} for \code{type =
#'   "wrap"} and \code{type = "multiline"}. If set to \code{0}, deactivate the
#'   wrapping (for \code{type = "multiline"} only).
#' @return A character vector of converted \code{input} when \code{output =
#'   NULL}. The output file path with converted content otherwise.
#' @note Learn more about the new chunk option syntax in
#'   \url{https://yihui.org/en/2022/01/knitr-news/}
#' @section About \pkg{knitr} option syntax:
#'
#' Historical chunk option syntax have chunk option in the chunk header using
#' valid R syntax. This is an example for \verb{.Rmd} document
#' \preformatted{
#' ```\{r, echo = FALSE, fig.width: 10\}
#' ```
#' }
#'
#' New syntax allows to pass option inside the chunk using several variants
#' \itemize{
#' \item Passing options one per line using valid R syntax. This corresponds to \code{convert_chunk_header(type = "multiline")}.
#' \preformatted{
#' ```\{r\}
#' #| echo = FALSE,
#' #| fig.width = 10
#' ```
#' }
#'
#' \item Passing option part from header in-chunk with several line if wrapping is
#' needed. This corresponds to \code{convert_chunk_header(type = "wrap")}
#' \preformatted{
#' ```\{r\}
#' #| echo = FALSE, fig.width = 10
#' ```
#' }
#' \item Passing options key value pairs in-chunk using YAML syntax. Values are no
#' more R expression but valid YAML syntax. This corresponds to
#' \code{convert_chunk_header(type = "yaml")} (not implement yet).
#' \preformatted{```\{r\}
#' #| echo: false,
#' #| fig.width: 10
#' ```
#' }
#' }
#' @examples
#' knitr_example = function(...) system.file('examples', ..., package = 'knitr')
#' # Convert a document for multiline type
#' convert_chunk_header(knitr_example('knitr-minimal.Rmd'))
#' # Convert a document for wrap type
#' convert_chunk_header(knitr_example('knitr-minimal.Rmd'), type = "wrap")
#' # Reduce default wrapping width
#' convert_chunk_header(knitr_example('knitr-minimal.Rmd'), type = "wrap", width = 0.6 * getOption('width'))
#' \dontrun{
#' # Explicitly name the output
#' convert_chunk_header('test.Rmd', output = 'test2.Rmd')
#' # Overwrite the input
#' convert_chunk_header('test.Rmd', output = identity)
#' # Use a custom function to name the output
#' convert_chunk_header('test.Rmd', output = \(f) sprintf('%s-new.%s', xfun::sans_ext(f), xfun::file_ext(f)))
#' }
#' @export
convert_chunk_header = function(
  input, output = NULL, type = c('multiline', 'wrap', 'yaml'),
  width = 0.9 * getOption('width')
) {

  # extract fenced header information
  text = xfun::read_utf8(input)
  ext  = xfun::file_ext(input)
  if (missing(type) && ext == 'qmd') type = 'yaml'  # default to yaml for Quarto
  type = match.arg(type)
  pattern = detect_pattern(text, ext)
  # no code chunk in brew file
  if (pattern == 'brew') return()
  markdown_mode = pattern == 'md'
  chunk_begin = all_patterns[[pattern]]$chunk.begin

  # counter for inserted lines
  nb_added = 0L
  new_text = text
  for (i in grep(chunk_begin, text)) {
    # transform each chunk one by one
    indent = get_chunk_indent(text[i])
    header = extract_params_src(chunk_begin, text[i])
    engine = if (markdown_mode) get_chunk_engine(header) else 'r'
    params = if (markdown_mode) get_chunk_params(header) else header
    # if no params nothing to format
    if (params == '') next
    params2 = clean_empty_params(params)
    params2 = trimws(clean_empty_params(params2))

    # select the correct prefix char (e.g `#|`)
    opt_chars = xfun:::get_option_comment(engine)
    prefix = paste0(indent, opt_chars$start)

    # clean old chunk keeping only engine
    new_text[i + nb_added] = gsub(params, '', text[i], fixed = TRUE)

    # format new chunk
    if (type == 'wrap') {
      # simple line wrapping of R code
      params3 = strwrap(params2, width, prefix = prefix)
    } else if (type == 'multiline') {
      # one option per line of the form `key = value,`
      res = xfun::csv_options(params2)
      params3 = sprintf('%s = %s,', names(res), deparsed_string(res))

      # remove trailing for last element
      last = length(params3)
      params3[last] = gsub(',$', '', params3[last])

      # wrap long single line and add prefix
      params3 = if (width <= 0) paste0(prefix, params3) else {
        strwrap(params3, width, prefix = prefix)
      }
    } else {
      params3 = xfun::csv_options(params2)

      # fix un-evaluated options for yaml by transforming to !expr val
      params3 = lapply(params3, function(x) {
        if (is.symbol(x) || is.language(x)) {
          x = deparse(x, 500L)
          attr(x, 'tag') = '!expr'
        }
        x
      })
      # transform dot option to dash option
      params3 = dash_names(params3)
      # convert to yaml and add prefix
      params3 = strsplit(yaml::as.yaml(
        params3, handlers = list(
          # true / false instead of no
          logical = function(x) {
            x = tolower(x)
            class(x) = 'verbatim'
            x
          },
          # use character with verbatim for no quotes
          # so that integers as kept unchanged (without changing precision)
          # fig.width = 10, should not be fig-width: 10.0
          numeric = function(x) {
            if (length(x) != 1) return(x)
            x2 = as.integer(x)
            if (x2 == x) x2 else x
          }), line.sep = '\n'), '\n')[[1]]
      params3 = paste0(prefix, params3)
    }

    if (nzchar(opt_chars$end)) params3 = paste0(params3, opt_chars$end)

    # insert new chunk header
    new_text = append(new_text, params3, after = i + nb_added)
    nb_added = nb_added + length(params3)
  }

  if (is.null(output)) return(new_text)
  # otherwise write to file
  if (is.function(output)) output = output(input)
  xfun::write_utf8(new_text, output)
  invisible(output)
}

# TODO: when R 4.0.0 is minimal version, switch to deparse1()
deparsed_string = function(exprs) {
  unlist(lapply(exprs, function(x) paste(deparse(x, 500), collapse = ' ')))
}
