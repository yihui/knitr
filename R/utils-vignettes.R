#' Package vignette engines
#'
#' Since R 3.0.0, package vignettes can use non-Sweave engines, and \pkg{knitr}
#' has provided a few engines to compile vignettes via \code{\link{knit}()} with
#' different templates. See \url{https://yihui.org/knitr/demo/vignette/} for
#' more information.
#' @name vignette_engines
#' @note If you use the \code{knitr::rmarkdown} engine, please make sure that
#'   you put \pkg{rmarkdown} in the \samp{Suggests} field of your
#'   \file{DESCRIPTION} file. Also make sure \command{pandoc} is available
#'   during \command{R CMD build}. If you build your package from RStudio, this
#'   is normally not a problem. If you build the package outside RStudio, run
#'   \code{rmarkdown::find_pandoc()} in an R session to check if Pandoc can be
#'   found.
#'
#'   When the \pkg{rmarkdown} package is not installed or not available, or
#'   \command{pandoc} cannot be found, the \code{knitr::rmarkdown} engine will
#'   fall back to the \code{knitr::knitr} engine, which uses R Markdown v1 based
#'   on the \pkg{markdown} package.
#' @examples library(knitr)
#' vig_list = tools::vignetteEngine(package = 'knitr')
#' str(vig_list)
#' vig_list[['knitr::knitr']][c('weave', 'tangle')]
#' vig_list[['knitr::knitr_notangle']][c('weave', 'tangle')]
#' vig_list[['knitr::docco_classic']][c('weave', 'tangle')]
NULL

vweave = function(file, driver, syntax, encoding = 'UTF-8', quiet = FALSE, ...) {
  {
    on.exit({opts_chunk$restore(); knit_hooks$restore()}, add = TRUE)
    oopts = options(markdown.html.header = NULL, knitr.knit2html.force_v1 = TRUE)
    on.exit(options(oopts), add = TRUE)
  }
  opts_chunk$set(error = FALSE)  # should not hide errors
  knit_hooks$set(purl = function(...) {
    # run some hooks for vignettes
    hook_purl(...)  # write out code while weaving
    # optimize PNG images if tools exist and hooks not set
    for (i in c('optipng', 'pngquant'))
      if (!is.function(knit_hooks$get(i)) && Sys.which(i) != '') {
        switch(i, optipng = hook_optipng(...), pngquant = hook_pngquant(...))
      }
  })
  (if (grepl('\\.[Rr]md$', file)) knit2html else if (grepl('\\.[Rr]rst$', file)) knit2pandoc else knit)(
    file, encoding = encoding, quiet = quiet, envir = globalenv(), ...
  )
}

vtangle = function(file, ..., encoding = 'UTF-8', quiet = FALSE) {
  purl(file, encoding = encoding, quiet = quiet, ...)
}

vweave_docco_linear = vweave
body(vweave_docco_linear)[5L] = expression(docco_linear(
  file, encoding = encoding, quiet = quiet, envir = globalenv(), ...
))

vweave_docco_classic = vweave
body(vweave_docco_classic)[5L] = expression(rocco(
  file, encoding = encoding, quiet = quiet, envir = globalenv(), ...
))

vweave_rmarkdown = vweave
body(vweave_rmarkdown)[5L] = expression(rmarkdown::render(
  file, encoding = encoding, quiet = quiet, envir = globalenv(),
  output_dir = getwd(), ...
))

# do not tangle R code from vignettes
untangle_weave = function(vig_list, eng) {
  weave = vig_list[[c(eng, 'weave')]]
  # remove the purl hook from the weave function, but the rmarkdown engine
  # function is different (not vweave_rmarkdown above, but the function(...)
  # defined below in vig_engine('rmarkdown'), and it is not straightforward to
  # remove the purl hook there)
  if (eng != 'knitr::rmarkdown') body(weave)[4L] = expression({})
  weave
}
vtangle_empty = function(file, ...) {
  unlink(with_ext(file, 'R'))
  return()
}

# when neither Pandoc nor markdown is available, just silently skip the vignette
vweave_empty = function(file, ..., .reason = 'Pandoc') {
  out = with_ext(file, 'html')
  writeLines(sprintf('The vignette could not be built because %s is not available.', .reason), out)
  out
}

register_vignette_engines = function(pkg) {
  # the default engine
  vig_engine('knitr', vweave, '[.]([rRsS](nw|tex)|[Rr](md|html|rst))$')
  vig_engine('docco_linear', vweave_docco_linear, '[.][Rr](md|markdown)$')
  vig_engine('docco_classic', vweave_docco_classic, '[.][Rr]mk?d$')
  vig_engine('rmarkdown', function(...) {
    if (is_cran_check() && !has_package('rmarkdown'))
      return(vweave_empty(..., .reason = 'rmarkdown'))
    if (pandoc_available()) {
      vweave_rmarkdown(...)
    } else {
      (if (is_R_CMD_check()) message else stop2)(
        'Pandoc is required to build R Markdown vignettes but not available. ',
        'Please make sure it is installed.'
      )
      if (has_package('markdown')) vweave(...) else vweave_empty(...)
    }
  }, '[.][Rr](md|markdown)$')
  # vignette engines that disable tangle
  vig_list = tools::vignetteEngine(package = 'knitr')
  engines  = grep('_notangle$', names(vig_list), value = TRUE, invert = TRUE)
  for (eng in engines) vig_engine(
    paste(sub('^knitr::', '', eng), 'notangle', sep = '_'),
    untangle_weave(vig_list, eng),
    tangle = vtangle_empty,
    pattern = vig_list[[c(eng, 'pattern')]]
  )
}
# all engines use the same tangle and package arguments, so factor them out
vig_engine = function(..., tangle = vtangle) {
  tools::vignetteEngine(..., tangle = tangle, package = 'knitr', aspell = list(
    filter = knit_filter
  ))
}

#' Spell check filter for source documents
#'
#' When performing spell checking on source documents, we may need to skip R
#' code chunks and inline R expressions, because many R functions and symbols
#' are likely to be identified as typos. This function is designed for the
#' \code{filter} argument of \code{\link{aspell}()} to filter out code chunks
#' and inline expressions.
#' @param ifile Filename of the source document.
#' @param encoding Ignored (the file \code{ifile} must be encoded in UTF-8).
#' @return A character vector of the file content, excluding code chunks and
#'   inline expressions.
#' @export
#' @examples library(knitr)
#' knitr_example = function(...) system.file('examples', ..., package = 'knitr')
#' \donttest{
#' if (Sys.which('aspell') != '') {
#' # -t means the TeX mode
#' utils::aspell(knitr_example('knitr-minimal.Rnw'), knit_filter, control = '-t')
#'
#' # -H is the HTML mode
#' utils::aspell(knitr_example('knitr-minimal.Rmd'), knit_filter, control = '-H -t')
#' }}
knit_filter = function(ifile, encoding = 'UTF-8') {
  x = read_utf8(ifile)
  n = length(x); if (n == 0) return(x)
  p = detect_pattern(x, tolower(file_ext(ifile)))
  if (is.null(p)) return(x)
  p = all_patterns[[p]]; p1 = p$chunk.begin; p2 = p$chunk.end
  m = group_indices(grepl(p1, x), grepl(p2, x))
  i = m %% 2 == 0
  x[i] = ''  # remove code chunks
  x[!i] = gsub(p$inline.code, '', x[!i], perl = TRUE)  # remove inline code
  structure(x, control = '-H -t')
}

pandoc_available = function() {
  rmarkdown::pandoc_available('1.12.3')
}

html_vignette = function(
  ..., fig_caption = TRUE, theme = NULL, highlight = NULL,
  includes = list(
    in_header = system.file('misc', 'vignette.html', package = 'knitr')
  )
) {
  rmarkdown::html_document(
    ..., fig_caption = fig_caption, theme = theme, highlight = highlight,
    includes = includes
  )
}
