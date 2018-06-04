library(testit)

# Test that out disposal of stringr
# was safe

if (requireNamespace("stringr", quietly = TRUE)) {
  a <- b <- paste0(letters, collapse = "")
  stringr_oui_1 <- stringr::str_sub(a, 2, 4)
  stringr_non_1 <- stringr__str_sub(a, 2, 4)
  assert("stringr str_sub same as before",
         identical(stringr_oui_1,
                   stringr_non_1))

  # Substitution was quite difficult to mimc

  test_stringr_sub_assign <- function(string, start, stop, value,
                                      verbose = getOption("_test_stringr_verbose_", FALSE)) {
    # avoid race condition: string will be modified in-place

    if (verbose) {
      stringA <- stringB <- string
      stringr::str_sub(stringA, start, stop) <- value
      stringB <- stringr__str_sub_assign(stringB, start, stop, value = value)
      cat("\n", stringA, "\n", stringB, "\n\n")
    }
    stringA <- stringB <- string
    assert(paste("Test conformance with stringr", string, start, stop, value),
           identical({
             stringr::str_sub(stringA, start, stop) <- value
             stringA
           },
           {
             stringr__str_sub_assign(stringB, start, stop, value = value)
           }))


  }


  test_stringr_sub_assign(a, 0, 0, "x")
  test_stringr_sub_assign(a, 3, 3, "x")
  test_stringr_sub_assign(a, 26, 26, "x")
  test_stringr_sub_assign(a, 3, 6, "x")
  test_stringr_sub_assign(a, 3, -3, "x")
  test_stringr_sub_assign(a, 3, 3, "XY")
  test_stringr_sub_assign(a, 3, 6, "XY")
  test_stringr_sub_assign(a, 3, -3, "XY")
  test_stringr_sub_assign(a, 3, 3, "0123")
  test_stringr_sub_assign(a, 3, 6, "0123")
  test_stringr_sub_assign(a, 3, -3, "0123")
  test_stringr_sub_assign(a, 0, 0, "0123")
  test_stringr_sub_assign(a, 0, 2, "0123")
  test_stringr_sub_assign(a, 0, -3, "0123")
  test_stringr_sub_assign(a, 0, 0, "x")
  test_stringr_sub_assign(a, 0, 2, "x")
  test_stringr_sub_assign(a, 0, -3, "x")

  test_stringr_sub_assign("# Regression on {{i}}\n```{r lm-{{i}}}\nlm(mpg~{{i}}, data=mtcars)",
                          17,
                          21,
                          value = "gear")

  test_stringr_sub_assign("()()()!", 5, 6, value = "!%")

  test_stringr_sub_assign("()()()!", 5, 6, value = "")
  test_stringr_sub_assign("()()()!", 7, 7, value = "")
  test_stringr_sub_assign("()()()!", 8, 8, value = "")


} else {
  assert("(stringr conformance not tested)", TRUE, TRUE)
}


if (requireNamespace("stringr", quietly = TRUE)) {
  string  = c("sdfoihsdfoi", "read_chunk('salve');(read_chunk('read_chunk')))", "read_chunk()")
  pattern = "read_chunk\\(([^)]+)\\)"
  assert("read chunks extracted without stringr",
         identical(stringr__str_extract_all(string = string, pattern = pattern, .use_stringr = FALSE),
                   stringr::str_extract_all(string = string, pattern = pattern)))

  string  = '# Regression on {{i}}NN```{r lm-{{i}}}NNlm(mpg~{{i}}, data=mtcars)NN```'
  pattern = '\\{\\{((.|NN)+?)\\}\\}'
  assert("read chunks extracted without stringr",
         identical(stringr__str_extract_all(string = string, pattern = pattern, .use_stringr = FALSE),
                   stringr::str_extract_all(string = string, pattern = pattern)))
}

if (requireNamespace("stringr", quietly = TRUE)) {
  prev_use_stringr <- getOption("knitr.use.stringr")
  test_str_dup <- function(string, times, ignore.warn = FALSE) {
    options("knitr.use.stringr" = FALSE)
    on.exit(options("knitr.use.stringr" = prev_use_stringr))
    if (ignore.warn) {
      suppressWarnings({
        oui <- stringr::str_dup(string, times)
        non <- stringr__str_dup(string, times)
      })
    } else {
      oui <- stringr::str_dup(string, times)
      non <- stringr__str_dup(string, times)
    }
    assert("str_dup works without stringr", identical(oui, non))
  }

  test_str_dup("x", 5)
  test_str_dup(c("x", "--"), 5)
  test_str_dup(c("", "x"), 5)
  test_str_dup(c("a", "b", "c"), c(2, 5, 7))
  test_str_dup(c("a", "b", "c"), c(2, 5), ignore.warn = TRUE)


  options("knitr.use.stringr" = prev_use_stringr)
}

if (requireNamespace("stringr", quietly = TRUE)) {
  prev_use_stringr <- getOption("knitr.use.stringr")
  options("knitr.use.stringr" = FALSE)
  # No match:
  with_stringr <- stringr::str_locate_all("<<echo=TRUE>>=", '(^|\n)\\s*\\\\documentclass[^}]+\\}')
  wout_stringr <- stringr__str_locate_all("<<echo=TRUE>>=", '(^|\n)\\s*\\\\documentclass[^}]+\\}')

  assert("str_locate_all returns correct null result",
         identical(with_stringr,
                   wout_stringr))

  options("knitr.use.stringr" = prev_use_stringr)
}

if (requireNamespace("stringi", quietly = TRUE)) {
  prev_use_stringr <- getOption("knitr.use.stringr")
  options("knitr.use.stringr" = FALSE)
  pattern <- "AX*B"
  string <- "AB AXXB AXXXB"
  assert("str_locate_all matches when matches are of different length in the same line",
         identical(stringr::str_locate_all(string, pattern),
                   stringr__str_locate_all(string, pattern)))
  options("knitr.use.stringr" = prev_use_stringr)
}

if (requireNamespace("stringr", quietly = TRUE)) {
  prev_use_stringr <- getOption("knitr.use.stringr")
  options("knitr.use.stringr" = FALSE)
  phone <- "([2-9][0-9]{2})[- .]([0-9]{3})[- .]([0-9]{4})"
  strings <- c(" 219 733 8965",
               "329-293-8753 ",
               "banana",
               "595 794 7569",
               "387 287 6718",
               "apple",
               "233.398.9187  ",
               "482 952 3315",
               "239 923 8115 and 842 566 4692",
               "Work: 579-499-7527",
               "$1000",
               "Home: 543.355.3679")
  assert("str_match identical stringr::",
         identical(stringr__str_match(strings, phone),
                   stringr::str_match(strings, phone)))
  options("knitr.use.stringr" = prev_use_stringr)
}











