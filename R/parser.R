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
      p = strip_chunk_group(g, chunk.begin, chunk.end, patterns$chunk.code)
      parse_block(p$code, p$header, p$params.src, markdown_mode)
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

# trim a code chunk group (as returned by divide_chunks()) into its header,
# body, and header options source: drop the optional chunk footer, strip the
# optional code prefix (e.g. the leading % in Rtex), and extract params.src
strip_chunk_group = function(g, begin, end, code.prefix = NULL) {
  n = length(g)
  if (n >= 2 && grepl(end, g[n])) g = g[-n]  # remove the optional chunk footer
  g = strip_block(g, code.prefix)  # remove the optional code prefix (e.g. % in Rtex)
  params.src = if (group_pattern(begin)) extract_params_src(begin, g[1]) else ''
  list(header = g[1], code = g[-1], params.src = params.src)
}

#' The code manager to manage code in all chunks
#'
#' This object provides methods to manage code (as character vectors) in all
#' chunks in \pkg{knitr} source documents. For example,
#' `knitr::knit_code$get()` returns a named list of all code chunks (the
#' names are chunk labels), and `knitr::knit_code$get('foo')` returns the
#' character vector of the code in the chunk with the label `foo`.
#' @note The methods on this object include the `set()` method (i.e., you
#'   could do something like `knitr::knit_code$set(foo = "'my precious new
#'   code'")`), but we recommend that you do not use this method to modify the
#'   content of code chunks, unless you are
#'   [as creative as Emi
#'   Tanaka](https://emitanaka.rbind.io/post/knitr-knitr-code/) and know what you are doing.
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
  dup = intersect(names(params), names(parts$options))
  params = merge_list(params, parts$options)
  if (is.null(params$label)) params$label = unnamed_chunk()
  label = params$label
  if (length(dup)) warning(
    "Duplicated chunk option(s) ", paste0("'", dup, "'", collapse = ', '),
    " in both chunk header and pipe comments of the chunk '", label, "'.", call. = FALSE
  )

  code = parts$code
  .knitEnv$labels = c(.knitEnv$labels, label)
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
    if (is.character(deps)) {
      for (i in deps) dep_list$set(setNames(list(c(dep_list$get(i), label)), i))
    } else warning2("Invalid chunk option 'dependson' (must be character or numeric)")
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
#' This is a wrapper function calling [xfun::divide_chunk()]
#' under the hood.
#' @export
#' @keywords internal
partition_chunk = function(engine, code) {
  opts = options(xfun.handle_error.loc_fun = get_loc)
  on.exit(options(opts))
  # the code has been moved to the xfun package
  xfun::divide_chunk(engine, code, strict = is_quarto())
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
  lines = integer()
  if (nrow(loc)) {
    code = t(str_match(input, inline.code))
    if (NCOL(code) >= 2L) {
      code1 = code[, 1L]
      code2 = apply(code[, -1L, drop = FALSE], 1, paste, collapse = '')
      nl = gregexpr('\n', input, fixed = TRUE)[[1]]
      lines = if (length(nl) > 1 || nl > -1) findInterval(loc, nl) else 0L
    }
  }

  structure(list(
    input = input, location = loc, code = code2, code.src = code1,
    lines = matrix(lines, ncol = 2, nrow = nrow(loc))
  ), class = 'inline')
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
#' the current \pkg{knitr} session; `read_demo()` is a convenience function
#' to read a demo script from a package.
#'
#' There are two approaches to read external code into the current session: (1)
#' Use a special separator of the from `## ---- chunk-label` (at least four
#' dashes before the chunk label) in the script; (2) Manually specify the
#' labels, starting and ending positions of code chunks in the script.
#'
#' Alternatively, the external file can be a \pkg{knitr} source document (e.g.,
#' an R Markdown `.Rmd` or R Sweave `.Rnw` file) instead of an R script. In this
#' case (detected automatically from the file extension or content, when
#' `labels` is not specified), the code chunks in the document are read in and
#' keyed by their chunk labels; chunks without a label are given an automatic
#' label. Text and inline code outside code chunks are ignored.
#'
#' The second approach will be used only when `labels` is not `NULL`.
#' For this approach, if `from` is `NULL`, the starting position is 1;
#' if `to` is `NULL`, each of its element takes the next element of
#' `from` minus 1, and the last element of `to` will be the length of
#' `lines` (e.g. when `from = c(1, 3, 8)` and the script has 10 lines
#' in total, `to` will be `c(2, 7, 10)`). Alternatively, `from`
#' and `to` can be character vectors as regular expressions to specify the
#' positions; when their length is 1, the single regular expression will be
#' matched against the `lines` vector, otherwise each element of
#' `from`/`to` is matched against `lines` and the match is
#' supposed to be unique so that the numeric positions returned from
#' `grep()` will be of the same length of `from`/`to`. Note
#' `labels` always has to match the length of `from` and `to`.
#' @param path Path to the R script, or a \pkg{knitr} source document such as an
#'   `.Rmd` or `.Rnw` file.
#' @param lines Character vector of lines of code. By default, this is read from
#'   `path`.
#' @param labels Character vector of chunk labels (default `NULL`).
#' @param from,to Numeric vector specifying the starting/ending line numbers of
#'   code chunks, or a character vector; see Details.
#' @param from.offset,to.offset Offsets to be added to `from`/`to`.
#' @param roxygen_comments Whether to keep trailing roxygen-style comments from
#'   code chunks in addition to whitespace.
#' @return As a side effect, code chunks are read into the current session so
#'   that future chunks can (re)use the code by chunk label references. If an
#'   external chunk has the same label as a chunk in the current session, chunk
#'   label references by future chunks will refer to the external chunk.
#' @references <https://yihui.org/knitr/demo/externalization/>
#' @note This function can only be used in a chunk which is *not* cached
#'   (chunk option `cache = FALSE`), and the code is read and stored in the
#'   current session *without* being executed (to actually run the code,
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
  # if the source is a knitr document (e.g., .Rmd or .Rnw) instead of an R
  # script, extract its code chunks by chunk label (#2041)
  if (is.null(labels) && length(
    code <- chunks_from_doc(lines, if (!missing(path)) path)
  )) {
    knit_code$set(code)
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
#'   [utils::demo()].
#' @param ... Arguments passed to [read_chunk()].
#' @export
read_demo = function(topic, package = NULL, ...) {
  paths = list.files(file.path(find.package(package), 'demo'), full.names = TRUE)
  read_chunk(paths[sans_ext(basename(paths)) == topic], ...)
}

# extract code chunks (as a named list, keyed by chunk label) from a knitr
# source document such as .Rmd or .Rnw; return NULL if the input does not look
# like a knitr document (so read_chunk() can fall back to R-script parsing)
chunks_from_doc = function(lines, path = NULL) {
  ext = if (length(path)) file_ext(path)
  type = if (length(ext)) detect_pattern(lines, ext) else detect_pattern(lines)
  if (is.null(type)) return()
  pat = all_patterns[[type]]
  begin = pat$chunk.begin; end = pat$chunk.end
  if (is.null(begin) || is.null(end)) return()

  md = type %in% c('md', 'typst')
  groups = divide_chunks(lines, begin, end, md = md)
  code = list()
  for (g in groups) {
    if (!grepl(begin, g[1])) next  # a text (non-code) group
    p = strip_chunk_group(g, begin, end, pat$chunk.code)
    # parse the engine and header options (e.g. ```{r label, echo=FALSE})
    params.src = p$params.src; engine = 'r'
    if (md) {
      engine = get_chunk_engine(params.src)
      params.src = get_chunk_params(params.src)
    }
    params.src = clean_empty_params(params.src)
    params = tryCatch(xfun::csv_options(params.src), error = function(e) list())
    # separate any in-body options (e.g. YAML `#| label: foo`) from the code
    parts = partition_chunk(engine, p$code)
    label = merge_list(params, parts$options)$label
    body = strip_white(parts$code)
    if (!length(body)) next
    if (is.null(label) || !nzchar(label)) label = unnamed_chunk()
    code[[label]] = as.character(body)
  }
  code
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
  if (!group_pattern(rc)) return(x)

  code = knit_code$get()

  # first expand references that occupy a whole line, e.g. `  <<foo>>`; these may
  # bring in multi-line chunks and their indentation is preserved
  if (length(idx <- grep(rc, x))) {
    labels = sub(rc, '\\1', x[idx])
    i = labels %in% names(code)
    idx = idx[i]; block = code[labels[i]]
    indent = gsub('^(\\s*).*', '\\1', x[idx])
    block = mapply(indent_block, block, indent, SIMPLIFY = FALSE, USE.NAMES = FALSE)
    x = as.list(x)
    x[idx] = lapply(block, function(z) parse_chunk(z, rc))
    x = unlist(x, use.names = FALSE)
  }

  # then expand references embedded in a line together with other code, e.g.
  # `mtcars %>% <<foo>>` (#2034); a multi-line chunk is spliced into the line
  unlist(lapply(x, fill_inline_ref, rc = rc, code = code), use.names = FALSE)
}

# expand chunk references `<<label>>` that appear in a line together with other
# code (references occupying a whole line are handled separately in
# parse_chunk()); returns a character vector that may be longer than one line
# when a referenced chunk spans multiple lines
fill_inline_ref = function(x, rc, code) {
  loc = gregexpr('<<[^<>\n]+>>', x)[[1]]
  if (loc[1L] == -1L) return(x)
  len = attr(loc, 'match.length')
  # split the line into alternating literal text and reference pieces
  pieces = list(); pos = 1L
  for (k in seq_along(loc)) {
    s = loc[k]; e = s + len[k] - 1L
    if (s > pos) pieces = c(pieces, list(substr(x, pos, s - 1L)))
    z = code[[substr(x, s + 2L, e - 2L)]]  # chunk code for this label (or NULL)
    # keep an unknown reference verbatim; recursively expand a known one
    pieces = c(pieces, list(if (length(z) == 0L) substr(x, s, e) else parse_chunk(z, rc)))
    pos = e + 1L
  }
  if (pos <= nchar(x)) pieces = c(pieces, list(substr(x, pos, nchar(x))))
  out = paste_lines(pieces)
  # indent continuation lines to the leading whitespace of the host line
  if (length(out) > 1L && nzchar(indent <- gsub('^(\\s*).*', '\\1', x)))
    out[-1L] = paste0(indent, out[-1L])
  out
}

# concatenate character vectors "horizontally": the last line of one piece is
# joined with the first line of the next (so multi-line pieces splice in place)
paste_lines = function(pieces) {
  Reduce(function(a, b) {
    if (length(a) == 0L) return(b)
    if (length(b) == 0L) return(a)
    n = length(a)
    c(a[-n], paste0(a[n], b[1L]), b[-1L])
  }, pieces)
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
#' The function `all_labels()` returns all chunk labels as a character
#' vector. Optionally, you can specify a series of conditions to filter the
#' labels. The function `all_rcpp_labels()` is a wrapper function for
#' `all_labels(engine == 'Rcpp')`.
#'
#' For example, suppose the condition expression is `engine == 'Rcpp'`, the
#' object `engine` is the local chunk option `engine`. If an
#' expression fails to be evaluated (e.g. when a certain object does not exist),
#' `FALSE` is returned and the label for this chunk will be filtered out.
#' @param ... A vector of R expressions, each of which should return `TRUE`
#'   or `FALSE`. The expressions are evaluated using the *local* chunk
#'   options of each code chunk as the environment, which means global chunk
#'   options are not considered when evaluating these expressions. For example,
#'   if you set the global chunk option `opts_chunk$set(purl = TRUE)`,
#'   `all_labels(purl == TRUE)` will *not* return the labels of all
#'   code chunks, but will only return the labels of those code chunks that have
#'   local chunk options `purl = TRUE`.
#' @note Empty code chunks are always ignored, including those chunks that are
#'   empty in the original document but filled with code using chunk options
#'   such as `ref.label` or `code`.
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
#' expressions. For example, if you want to write `` `r 1+1` `` literally in an
#' R Markdown document, you may write ``` `` `r knitr::inline_expr('1+1')` `` ```;
#' for Rnw documents, this may be `\Sexpr{knitr::inline_expr{'1+1'}}`.
#' @param code Character string of the inline R source code.
#' @param syntax A character string to specify the syntax, e.g., `rnw`,
#'   `html`, or `md`. If not specified, this will be guessed from the
#'   knitting context.
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
#' @param output The default `NULL` will output to console. Other values
#'   can be a file path to write the converted content into or a function which
#'   takes `input` as argument and returns a file path to write into (e.g.,
#'   `output = identity` to overwrite the input file).
#' @param type This determines how the in-body options will be formatted.
#'   `"mutiline"` (the default, except for \file{qmd} documents, for which
#'   the default is `"yaml"`; the extension of `output` takes precedence over
#'   that of `input`, e.g., converting an `.Rmd` to a `.qmd`) will write each
#'   chunk option on a separate
#'   line. Long chunk option values will be wrapped onto several lines, and you
#'   can use `width = 0` to keep one line per option only. `"wrap"`
#'   will wrap all chunk options together using
#'   [base::strwrap()]. `"yaml"` will convert
#'   chunk options to YAML.
#' @param width An integer passed to `base::strwrap()` for `type =
#'   "wrap"` and `type = "multiline"`. If set to `0`, deactivate the
#'   wrapping (for `type = "multiline"` only).
#' @return A character vector of converted `input` when `output =
#'   NULL`. The output file path with converted content otherwise.
#' @note Learn more about the new chunk option syntax in
#'   <https://yihui.org/en/2022/01/knitr-news/>
#' @section About \pkg{knitr} option syntax:
#'
#' Historical chunk option syntax have chunk option in the chunk header using
#' valid R syntax. This is an example for \verb{.Rmd} document
#'
#' ````
#' ```{r, echo = FALSE, fig.width = 10}
#' ```
#' ````
#'
#' New syntax allows to pass option inside the chunk using several variants
#' \itemize{
#' \item Passing options one per line using valid R syntax. This corresponds to `convert_chunk_header(type = "multiline")`.
#'
#' ````
#' ```{r}
#' #| echo = FALSE,
#' #| fig.width = 10
#' ```
#' ````
#'
#' \item Passing option part from header in-chunk with several line if wrapping is
#' needed. This corresponds to `convert_chunk_header(type = "wrap")`
#'
#' ````
#' ```{r}
#' #| echo = FALSE, fig.width = 10
#' ```
#' ````
#' \item Passing options key value pairs in-chunk using YAML syntax. Values are no
#' more R expression but valid YAML syntax. This corresponds to
#' `convert_chunk_header(type = "yaml")`.
#'
#' ````
#' ```{r}
#' #| echo: false,
#' #| fig.width: 10
#' ```
#' ````
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
  # resolve the output path early so its extension can inform the default type;
  # the output extension takes precedence over the input's (e.g. Rmd -> qmd)
  if (is.function(output)) output = output(input)
  ext2 = xfun::file_ext(if (is.character(output)) output else input)
  if (missing(type) && ext2 == 'qmd') type = 'yaml'  # default to yaml for Quarto
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
  # otherwise write to file (output was already resolved to a path above)
  xfun::write_utf8(new_text, output)
  invisible(output)
}

# TODO: when R 4.0.0 is minimal version, switch to deparse1()
deparsed_string = function(exprs) {
  unlist(lapply(exprs, function(x) paste(deparse(x, 500), collapse = ' ')))
}

#' Assign labels to unnamed code chunks in a document
#'
#' Find code chunks that do not have a label and insert a generated label into
#' their chunk headers. This can be useful because \pkg{knitr} refers to
#' unnamed chunks by an automatically generated label like \samp{unnamed-chunk-1}
#' in messages and warnings, which does not tell you much about where the chunk
#' is. Giving each chunk an explicit label makes such messages more informative,
#' and also makes the cache and figure file names more meaningful.
#'
#' Only chunks in \pkg{knitr}'s Markdown syntax (e.g., \verb{```\{r\}}) are
#' processed; existing labels are left untouched, and the labels of other chunks
#' are taken into account so that the newly generated labels do not clash with
#' them.
#' @inheritParams convert_chunk_header
#' @param text A character vector of the document content. If provided, `input`
#'   is ignored, and the document is assumed to use the Markdown syntax.
#' @param label A function to generate a label for an unnamed chunk. It is
#'   called with two arguments: the chunk's options (a list, without the
#'   `label`), and the index of the unnamed chunk in the document (an integer
#'   starting from 1). It should return a character string. The default function
#'   generates labels of the form \samp{prefix-i} using the `prefix` argument.
#'   If a generated label happens to be already in use, a numeric suffix is
#'   appended to make it unique.
#' @param prefix The prefix for the default `label` function. Defaults to the
#'   global option `unnamed.chunk.label` (see [opts_knit]), i.e.,
#'   \samp{unnamed-chunk}.
#' @return A character vector of the document with labels assigned when `output
#'   = NULL`, otherwise the output file path (invisibly), with the labeled
#'   content written to it.
#' @seealso [convert_chunk_header()]
#' @export
#' @examples
#' # a document with two unnamed chunks and one named chunk
#' doc = c(
#'   '```{r}', '1 + 1', '```', '',
#'   '```{r, echo=FALSE}', 'plot(cars)', '```', '',
#'   '```{r named}', 'summary(cars)', '```'
#' )
#' cat(knitr::label_chunks(text = doc), sep = '\n')
#'
#' # use a custom label function based on the chunk options
#' cat(knitr::label_chunks(
#'   text = doc, label = function(options, i) paste0('fig', i)
#' ), sep = '\n')
label_chunks = function(
  input, text = NULL, output = NULL, label = NULL,
  prefix = opts_knit$get('unnamed.chunk.label')
) {
  if (is.null(text)) {
    text = xfun::read_utf8(input); ext = xfun::file_ext(input)
    if (is.function(output)) output = output(input)
  } else {
    text = split_lines(text); ext = 'Rmd'
  }
  pattern = detect_pattern(text, ext)
  # only knitr's Markdown chunk syntax is supported
  if (!identical(pattern, 'md')) stop(
    'label_chunks() only supports documents using the Markdown chunk syntax, ',
    'e.g., ```{r}.'
  )
  chunk_begin = all_patterns[['md']]$chunk.begin
  if (is.null(label)) label = function(options, i) paste(prefix, i, sep = '-')
  if (!is.function(label)) stop('The `label` argument must be a function')

  begins = grep(chunk_begin, text)
  # collect existing labels first so generated ones will not clash with them
  chunk_label = function(line) {
    header = extract_params_src(chunk_begin, line)
    params = get_chunk_params(header)
    res = tryCatch(xfun::csv_options(params), error = function(e) list())
    if (is.character(res$label) && res$label != '') res$label else NA_character_
  }
  used = Filter(Negate(is.na), lapply(begins, function(i) chunk_label(text[i])))
  used = unlist(used)

  i_unnamed = 0L
  for (i in begins) {
    header = extract_params_src(chunk_begin, text[i])
    engine = get_chunk_engine(header)
    params = get_chunk_params(header)
    opts = tryCatch(xfun::csv_options(params), error = function(e) list())
    if (is.character(opts$label) && opts$label != '') next  # already labeled
    i_unnamed = i_unnamed + 1L
    opts$label = NULL
    lab = label(opts, i_unnamed)
    if (!is.character(lab) || length(lab) != 1) stop(
      'The `label` function must return a single character string'
    )
    # ensure the generated label is unique in the document
    lab0 = lab; j = 1L
    while (lab %in% used) { lab = paste(lab0, j, sep = '-'); j = j + 1L }
    used = c(used, lab)
    inside = paste0(engine, ' ', insert_chunk_label(params, lab))
    text[i] = sub(header, inside, text[i], fixed = TRUE)
  }

  if (is.null(output)) return(text)
  xfun::write_utf8(text, output)
  invisible(output)
}

# insert a label at the front of a chunk's option string (the part after the
# engine name), adding a comma separator only when other options are present
insert_chunk_label = function(params, label) {
  params = sub('^\\s+', '', params)  # drop leading whitespace
  if (params == '') return(label)
  if (grepl('^,', params)) paste0(label, params) else paste0(label, ', ', params)
}
