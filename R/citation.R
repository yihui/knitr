#' Generate BibTeX bibliography databases for R packages
#'
#' A wrapper function of \code{xfun::pkg_bib()}.
#' @param ...,prefix Arguments passed to \code{xfun::\link[xfun]{pkg_bib}()}.
#' @export
write_bib = function(..., prefix = getOption('knitr.bib.prefix', 'R-')) {
  xfun::pkg_bib(..., prefix = prefix)
}
