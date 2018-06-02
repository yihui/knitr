

paste00 <- function(...) paste0(..., collapse = "")

use_stringr <- function() {
  if (identical(Sys.getenv("KNITR_USE_STRINGR"), "FALSE")) {
    return(FALSE)
  } else {
    getOption("knitr.use.stringr", TRUE) &&
      requireNamespace("stringr", quietly = TRUE)
  }
}

stri_sub_no_stringi <- function(str, from, to) {
  out <- str
  nchar_out <- nchar(out) + 1L
  if (length(from) != 1L || length(to) != 1L) {
    stop("Internal error: stringi::stri_sub replacement can only handle ",
         "length-one froms and tos. \n\t",
         "length(from) = ", length(from), "\n\t",
         "length(to) = ", length(to), "\n",
         "If you are a user, please file an issue. ",
         "As a workaround, install.packages('stringr') and try again.")
  }
  FROM <- rep_len(from, length(out))
  TO <- rep_len(to, length(out))
  if (from < 0L) {
    FROM <- nchar_out + from
  } else {
    FROM <- rep_len(from, length(out))
  }
  if (to < 0L) {
    TO <- nchar_out + to
  } else {
    TO <- rep_len(to, length(out))
  }
  substr(out, FROM, TO)
}

stri_sub <- function(str, from = 1L, to = -1L, .len) {
  if (requireNamespace("stringi", quietly = TRUE)) {
    if (missing(.len)) {
      stringi::stri_sub(str = str, from = from, to = to)
    } else {
      stringi::stri_sub(str = str, from = from, to = to, length = .len)
    }
  } else {
    stri_sub_no_stringi(str = str, from = from, to = to)
  }
}

# These are taken from stringr source code.
stringr__str_sub <- function (string, start = 1L, end = -1L) {
  if (is.matrix(start)) {
    stri_sub(string, from = start)
  }
  else {
    stri_sub(string, from = start, to = end)
  }
}

stringr__str_sub_assign <- function (string, start = 1L, end = -1L, omit_na = FALSE, value) {
  # if (requireNamespace("stringr", quietly = TRUE)) {
  #   stringr::str_sub(string, start, end) <- value
  #   return(string)
  # }

  out <- string
  from <- start
  to <- end
  nchar_out <- nchar(out) + 1L
  if (length(from) != 1L || length(to) != 1L) {
    stop("Internal error: stringi::stri_sub replacement can only handle ",
         "length-one froms and tos. \n\t",
         "length(from) = ", length(from), "\n\t",
         "length(to) = ", length(to), "\n",
         "If you are a user, please file an issue. ",
         "As a workaround, install.packages('stringr') and try again.")
  }
  FROM <- rep_len(from, length(out))
  TO <- rep_len(to, length(out))
  if (from < 0L) {
    FROM <- nchar_out + from
  } else {
    FROM <- rep_len(from, length(out))
  }
  if (to < 0L) {
    TO <- nchar_out + to
  } else {
    TO <- rep_len(to, length(out))
  }

  # stringr::str_sub<- differs from substr()<-
  # when the nchar(replacement) of the substr

  # y <- "abc"
  # z <- "xyz"
  # stringr::str_sub(y, 2, 2) <- z
  # y
  # => "axyzc"

  out_split <- strsplit(out, split = "")[[1L]]
  out <- paste00(c(if (FROM > 1) paste00(out_split[seq_len(FROM - 1L)]),
                   value,
                   if (length(out_split) - TO >= 1) {
                     paste00(out_split[seq.int(TO + 1L, length(out_split))])
                   }))




  out
}


.wrapper <- function(ss, width) {
  # + 1 for spaces
  sswidths <- cumsum(nchar(ss) + 1L)
  if (length(sswidths) <= 1L ||
      sswidths[length(sswidths)] < width) {
    return(ss)
  }
  for (i in seq_along(ss)) {

    if (sswidths[i] > width) {
      return(paste(paste0(ss[seq_len(i)],
                          collapse = " "),
                   paste0(.wrapper(ss[-seq_len(i)], width),
                          collapse = " "),
                   sep = "\n"))
    }
  }
}


stringr__str_wrap <- function(string, width = 80, indent = 0, exdent = 0) {
  if (use_stringr()) {
    stringr::str_wrap(string, width, indent, exdent)
  } else {
    unlist(
      lapply(
        string,
        function(s) {
          ss <- strsplit(s, split = " ", fixed = TRUE)[[1L]]
          out <- .wrapper(ss, width = width)
          c(formatC(trimws(out[1]), width = width),
            if (length(out) > 1) {
              out[-1]
            })
        }),
      use.names = FALSE,
      recursive = TRUE)
  }
}

stri_locate_all_regex_no_stri <- function(str, pattern) {
  row_has_pattern <- grepl(pattern, str, perl = TRUE)
  ans <-
    lapply(seq_along(str), function(i) {
      if (row_has_pattern[i]) {
        gregexprs <-
          gregexpr(pattern = pattern,
                   text = str[i],
                   perl = TRUE)
        nchar_pattern <- attr(gregexprs[[1L]], "match.length")[1L]
        out <- matrix(NA_integer_, nrow = length(gregexprs[[1L]]), ncol = 2L)
        for (j in seq_along(gregexprs[[1L]])) {
          out[j, 1L] <- gregexprs[[1L]][j]
        }
        out[, 2L] <- out[, 1L] + nchar_pattern - 1L

      } else {
        out <- matrix(NA_integer_, nrow = 1L, ncol = 2L)
      }
      # Conformance with stringi
      attr(out, "dimnames") <- list(NULL, c("start", "end"))
      out
    })
  ans
}

stringr__str_locate_all <- function(string, pattern) {
  if (use_stringr()) {
    stringr::str_locate_all(string, pattern)
  } else {
    if (length(grep(pattern, string, perl = TRUE))) {
      if (length(pattern) == 1L) {
        stri_locate_all_regex_no_stri(string, pattern)
      } else if (length(string) == length(pattern)) {
        lapply(seq_along(string), function(i) {
          stri_locate_all_regex_no_stri(string[i], pattern[i])
        })
      } else {
        stop("Internal error: string and pattern had unexpected length.")
      }
    } else {
      # stringr not the same as strini
      list(structure(integer(0),
                     .Dim = c(0L, 2L),
                     .Dimnames = list(NULL,
                                      c("start", "end")))))
    }
  }
}

stringr__str_locate <- function(...) {
  if (use_stringr()) {
    stringr::str_locate(...)
  } else {
    as.matrix(stringr__str_locate_all(...))
  }
}


stringr__str_extract_all <- function(string, pattern, simplify = FALSE, .use_stringr = TRUE) {
  if (.use_stringr && use_stringr()) {
    stringr::str_extract_all(string, pattern, simplify)
  } else {
    G <- gregexpr(pattern = pattern,
                  text = string,
                  # Must be perl to enable possessive regex
                  perl = TRUE)
    lapply(seq_along(string),
           function(i) {
             s <- string[i]
             if (!grepl(pattern, s)) {
               character(0L)
             } else {
               g <- G[[i]]
               mlens <- attr(g, "match.length")
               n <- length(g)
               out <- character(n)
               for (j in seq_along(out)) {
                 out[j] <- substr(s, g[j], g[j] + mlens[j] - 1L)
               }
               out
             }
           })
  }
}

stringr__str_trim <- function(x, side = c("both", "left", "right")) {
  side <- match.arg(side)
  str_trimws(x, side)

}

str_trimws <- function(x, side) {
  ws = intToUtf8(c(5760L,
                   8192:8202,
                   8232:8233,
                   8239L,
                   8287L,
                   12288L))

  has_left_ws <- function(x) {
    substr(x, 1, 1) %in% ws
  }

  has_right_ws <- function(x) {
    stri_sub(x, -1, -1) %in% ws
  }
  # en quads
  ans <- base::trimws(x, which = side)

  while (side != "right" && any(has_left_ws(ans))) {
    ans[has_left_ws(ans)] <-
      stri_sub(ans[has_left_ws(ans)], 2)
  }
  while (side != "left" && any(has_right_ws(ans))) {
    ans[has_right_ws(ans)] <-
      stri_sub(ans[has_right_ws(ans)], to = -2)
  }
  ans
}




stringr__str_dup <- function(string, times) {
  if (use_stringr()) {
    stringr::str_dup(string, times)
  } else {
    out <- character(pmax.int(length(string), length(times)))
    if (length(string) == 1L) {
      if (length(times) == 1L) {
        out <- paste00(rep_len(string, times))
      } else {
        for (i in seq_along(times)) {
          out[i] <- paste00(rep_len(string, times[i]))
        }
      }
    } else {
      if (length(times) == 1L) {
        for (i in seq_along(string)) {
          out[i] <- paste00(rep_len(string[i], times))
        }
      } else {
        if (length(times) > length(string)) {
          if (length(times) %% length(string)) {
            warning("longer object length is not a multiple of shorter object length ")
          }
        } else {
          if (length(string) %% length(times)) {
            warning("longer object length is not a multiple of shorter object length ")
          }
        }
        for (i in seq_along(out)) {
          js <- {{i - 1L} %% length(string)} + 1L
          jt <- {{i - 1L} %% length(times)} + 1L
          out[i] <- paste00(rep_len(string[js], times[jt]))
        }
      }
    }
    out
  }
}




