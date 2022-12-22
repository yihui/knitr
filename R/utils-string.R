# replace parts of a string with new values; `pos` is a matrix of positions and
# each row is a pair of [start, end]
str_replace = function(x, pos, value) {
  if (length(x) != 1) stop("Only a character scalar is supported.")
  # extract parts of the string that are outside [start, end]
  m = rbind(pos[, 1] - 1, pos[, 2] + 1)
  m = matrix(c(1, m, nchar(x)), nrow = 2)
  y = substring(x, m[1, ], m[2, ])
  paste(rbind(y, c(value, '')), collapse = '')
}

# a wrapper function to make strwrap() return a character vector of the same
# length as the input vector; each element of the output vector is a string
# formed by concatenating wrapped strings by \n
str_wrap = function(...) {
  res = strwrap(..., simplify = FALSE)
  unlist(lapply(res, one_string))
}

# a simplified replacement for stringr::str_locate() that returns an integer
# matrix having a row for each element of the input `string`, and two columns:
# 'start' and 'end'. If the match is of length 0, 'end' will have one character
# less than 'start'. Depends on the `location()` util function, also defined
# in this file.
str_single_locate = function(string, pattern) {
  out = regexpr(pattern, string, perl = TRUE)
  location(out)
}

# a simplified replacement for stringr::str_locate_all() that returns a list
# having an element for every element of 'string'; every list element is an
# integer matrix having a row per match, and two columns: 'start' and 'end'.
# Depends on the location() util function, also defined in this file.
str_complete_locate = function(string, pattern) {
  out = gregexpr(pattern, string, perl = TRUE)
  lapply(out, location, all = TRUE)
}

# a replacement for stringr::str_extract_all()
str_complete_extract = function(string, pattern) {
  loc = str_complete_locate(string, pattern)
  lapply(seq_along(string), function(i) {
    loc = loc[[i]]
    str_substitute(rep(string[[i]], nrow(loc)), loc)
  })
}

# replacement for stringr::str_match_all()
str_get_all_matches = function(string, pattern) {
  res = gregexec_knitr(pattern, string, perl = TRUE)
  loc = lapply(res, FUN = function(x) {
    if (all(is.na(x))) return(list(NA_character_))
    match_attr = attr(x, "match.length")
    if (!is.matrix(match_attr) && match_attr == -1) return(list())
    matches = list()
    if (!is.null(ncol(x))) {
      for (i in seq_len(ncol(x))) {
        start_mat = x[, i, drop = FALSE]
        end_mat = attr(x, "match.length")[, i, drop = FALSE] + start_mat - 1
        matches = c(matches, list(cbind(start_mat, end_mat)))
      }
    }
    matches
  })
  out = lapply(seq_along(loc), function(i) {
    loc = loc[[i]]
    mat_missing = matrix(rep(NA_character_, 2), nrow = 1)
    mat_empty = matrix(rep(NA_character_, 2), nrow = 1)[-1, ]
    if (is.list(loc) && length(loc) > 0 && !is.matrix(loc[[1]]) && is.na(loc[[1]])) {
      return(mat_missing)
    }
    if (length(loc) < 1) return(mat_empty)
    for (j in seq_along(loc)) {
      loc_j = loc[[j]]
      loc_j[loc_j < 0] = NA_real_
      subst = str_substitute(rep(string[[i]], nrow(loc_j)), loc_j)
      if (!all(is.na(subst))) {
        if (j == 1) {
          mat = matrix(rep(NA_character_, nrow(loc_j)), nrow = 1)[-1, ]
        }
        mat_j = t(as.matrix(subst))
        mat = rbind(mat, mat_j)
      }
    }
    mat
  })
  cols_mat = vapply(out, FUN.VALUE = integer(1), USE.NAMES = FALSE, FUN = NCOL)
  max_cols_mat = max(cols_mat, na.rm = TRUE)
  out = lapply(seq_along(loc), function(i) {
    elem = out[[i]]
    if (ncol(elem) < max_cols_mat) {
      return(matrix(rep(NA_character_, max_cols_mat), nrow = 1)[-1, ])
    }
    elem
  })
  out
}

gregexec_knitr = function(pattern, text, perl = FALSE) {
  if (is.factor(text) && length(levels(text)) < length(text)) {
    out = gregexec_knitr(pattern, c(levels(text), NA_character_), perl)
    outna = out[length(out)]
    out = out[text]
    out[is.na(text)] = outna
    return(out)
  }
  dat = gregexpr(pattern = pattern, text = text, perl = perl)
  if (perl) {
    capt.attr = c('capture.start', 'capture.length', 'capture.names')
    process = function(x) {
      if (anyNA(x) || any(x < 0)) y = x
      else {
        y = t(cbind(x, attr(x, "capture.start")))
        attributes(y)[names(attributes(x))] = attributes(x)
        ml = t(cbind(attr(x, "match.length"), attr(x, "capture.length")))
        nm = attr(x, 'capture.names')
        dimnames(ml) = dimnames(y) = if (any(nzchar(nm))) list(c("", nm), NULL)
        attr(y, "match.length") = ml
        y
      }
      attributes(y)[capt.attr] = NULL
      y
    }
    lapply(dat, process)
  } else {
    m1 = lapply(regmatches(text, dat), regexec, pattern = pattern, perl = perl)
    mlen = lengths(m1)
    res = vector("list", length(m1))
    im = mlen > 0
    res[!im] = dat[!im]
    res[im] = Map(
      function(outer, inner) {
        tmp = do.call(cbind, inner)
        attributes(tmp)[names(attributes(inner))] = attributes(inner)
        attr(tmp, 'match.length') = do.call(cbind, lapply(inner, `attr`, 'match.length'))
        attr(tmp, 'useBytes') = attr(outer, 'useBytes')
        attr(tmp, 'index.type') = attr(outer, 'index.type')
        tmp + rep(outer - 1L, each = nrow(tmp))
      },
      dat[im],
      m1[im]
    )
    res
  }
}

# replacement for stringr::str_sub() and used internally in other string
# functions provided here
str_substitute = function(string, start = 1L, end = -1L) {
  if (is.matrix(start)) {
    end = start[, 2]
    start = start[, 1]
  }
  start = recycler(start, string)
  end = recycler(end, string)
  n = nchar(string)
  start = ifelse(start < 0, start + n + 1, start)
  end = ifelse(end < 0, end + n + 1, end)
  substr(string, start, end)
}

location = function(x) {
  len = attr(x, 'match.length')
  if (length(x) == 1 && x == -1) x = integer()
  cbind(start = x, end = x + len - 1L)
}

recycler = function(x, to, arg = deparse(substitute(x))) {
  if (length(x) == length(to)) return(x)
  if (length(x) != 1) {
    stop("Cannot recycle `", arg, "` to length ", length(to))
  }
  rep(x, length(to))
}
