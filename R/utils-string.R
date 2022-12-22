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

# replacement for stringr::str_match()
str_get_match = function(string, pattern) {
  loc = regexec(pattern, string, perl = TRUE)
  loc = lapply(loc, location)
  out = lapply(seq_along(string), function(i) {
    loc = loc[[i]]
    str_substitute(rep(string[[i]], nrow(loc)), loc)
  })
  do.call("rbind", out)
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

location = function(x, all = FALSE) {
  start = as.vector(x)
  if (all && identical(start, -1L)) {
    return(cbind(start = integer(), end = integer()))
  }
  end = as.vector(x) + attr(x, "match.length") - 1
  no_match = start == -1L
  start[no_match] = NA
  end[no_match] = NA
  cbind(start = start, end = end)
}

recycler = function(x, to, arg = deparse(substitute(x))) {
  if (length(x) == length(to)) return(x)
  if (length(x) != 1) {
    stop("Cannot recycle `", arg, "` to length ", length(to))
  }
  rep(x, length(to))
}
