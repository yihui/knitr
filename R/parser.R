## adapted from Hadley's decumar: https://github.com/hadley/decumar

## split input document into groups containing chunks and other texts
## (may contain inline R code)
split_file = function(path) {
    lines = readLines(path, warn = FALSE)
    n = length(lines)
    chunk.begin = knit_patterns$get('chunk.begin')
    chunk.end = knit_patterns$get('chunk.end')
    if (is.null(chunk.begin) || is.null(chunk.end)) {
        return(list(parse_inline(lines)))
    }

    set_tikz_opts(lines, chunk.begin, chunk.end)  # prepare for tikz option 'standAlone'

    blks = str_detect(lines, chunk.begin)
    txts = str_detect(lines, chunk.end)

    tmp = logical(n); tmp[blks | txts] = TRUE; lines[txts] = ''

    groups = unname(split(lines, cumsum(tmp)))

    ## parse 'em all
    lapply(groups, function(g) {
        block = str_detect(g[1], chunk.begin)
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

## an object to store chunk dependencies
dep_list = new_defaults()

## separate params and R code in code chunks
parse_block = function(input) {
    block = strip_block(input)
    n = length(block); chunk.begin = knit_patterns$get('chunk.begin')
    params = if (group_pattern(chunk.begin)) gsub(chunk.begin, '\\1', block[1]) else ''
    params = parse_params(params)

    label = params$label
    code = block[-1L]
    if (length(code)) {
        if (label %in% names(knit_code$get())) warning("duplicated label '", label, "'")
        knit_code$set(structure(list(code), .Names = label))
    }

    ## store dependencies
    if (!is.null(deps <- params$dependson)) {
        for (i in sc_split(deps))
            dep_list$set(structure(list(c(dep_list$get(i), label)), .Names = i))
    }

    structure(list(params = params), class = 'block')
}

## parse params from chunk header
parse_params = function(params, label = TRUE) {
    pieces = str_split(str_split(params, ',')[[1]], '=')
    n = sapply(pieces, length)
    ## when global options are empty
    if (length(n) == 1 && length(pieces[[1]]) == 1) {
        if (!label) {
            return(list())
        } else {
            return(list(label = if (is_blank(pieces[[1]]))
                        str_c('unnamed-chunk-', chunk_counter()) else pieces[[1]]))
        }
    }

    if (any(n == 1)) {
        if (label && length(idx <- which(n == 1)) == 1) {
            pieces[[idx]] = c('label', pieces[[idx]])
        } else stop("illegal tags: ", str_c(names(pieces)[idx[-1]], collapse = ', '), "\n",
                    "all options must be of the form 'tag=value' except the chunk label",
                    call. = FALSE)
    } else if (label && !str_detect(params, '\\s*label\\s*=')) {
        pieces[[length(pieces) + 1]] = c('label', str_c('unnamed-chunk-', chunk_counter()))
    }

    values = lapply(pieces, function(x) str_trim(x[2]))
    names(values) = str_trim(tolower(lapply(pieces, `[`, 1)))

    lapply(values, type.convert, as.is = TRUE)
}

print.block = function(x, ...) {
    if (length(params <- x$params) > 0)
        idx = setdiff(names(params), 'label')
        cat(str_c(strwrap(str_c(params$label, ": ", if (length(idx)) {
            str_c(idx, "=", unlist(params[idx]), collapse = ", ")
        } else ''), indent = 2, exdent = 4), collapse = '\n'), "\n")
    if (opts_knit$get('verbose')){
        code = knit_code$get(params$label)
        if (length(code) && !all(is_blank(code))) {
            cat("\n  ", str_pad(" R code chunk ", getOption('width') - 10L, 'both', '~'), "\n")
            cat(str_c('   ', code, collapse = '\n'), '\n')
            cat('  ', str_dup('~', getOption('width') - 10L), '\n')
        }
    }
    cat('\n')
}

## extract inline R code fragments (as well as global options)
parse_inline = function(input) {
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
        res2$code = sprintf("knit_child('%s')", res2$code)  # input chide with knit_child()
    }
    loc = rbind(res1$location, res2$location)
    idx = order(loc[, 1L])

    structure(list(input = input, location = loc[idx, , drop = FALSE],
                   params = params, code = c(res1$code, res2$code)[idx]),
              class = 'inline')
}

## locate and extract inline R code
extract_inline = function(input, pat.name, locate.fun) {
    pattern = knit_patterns$get(pat.name)
    loc = locate.fun(input, pattern)
    code = character(0)
    if (nrow(loc)) code = str_match(str_sub(input, loc[, 1L], loc[, 2L]), pattern)
    code = if (NCOL(code) == 2L) code[, 2L] else character(0)
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

##' Read chunks from an external R script
##'
##' Chunks can be put in an external R script, and this function reads
##' chunks into the current \pkg{knitr} session.
##'
##' The \code{ref.label} component in the pattern list
##' (\code{knit_patterns$get('ref.label')}) defines the format of code
##' chunks.
##' @param path the path to the R script
##' @return Code chunks are read into the current session so that
##' future chunks can use the R code.
##' @references \url{http://yihui.github.com/knitr/demo/reference/}
##' @note This function can only be used in a chunk which is
##' \emph{not} cached (chunk option \code{cache = FALSE}).
##' @export
##' @examples ## the default format
##'
##' ## @@knitr my-label
##' 1+1
##' lm(y~x, data=data.frame(x=1:10,y=rnorm(10)))
##'
##' ## later you can use <<my-label>>= to reference this chunk
read_chunk = function(path) {
    lines = readLines(path, warn = FALSE)
    lab = knit_patterns$get('ref.label')
    if (!group_pattern(lab)) return()
    groups = unname(split(lines, cumsum(str_detect(lines, lab))))
    labels = str_trim(str_replace(sapply(groups, `[`, 1), lab, '\\1'))
    code = lapply(groups, strip_chunk)
    idx = nzchar(labels); code = code[idx]; labels = labels[idx]
    names(code) = labels
    knit_code$set(code)
}

strip_chunk = function(x) {
    x = x[-1]; if (!length(x)) return(x)
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
