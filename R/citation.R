#' Generate BibTeX bibliography databases for R packages
#'
#' A wrapper function of `xfun::pkg_bib()`.
#' @param ...,prefix Arguments passed to [xfun::pkg_bib()].
#' @export
write_bib = function(..., prefix = getOption('knitr.bib.prefix', 'R-')) {
  xfun::pkg_bib(..., prefix = prefix)
}
