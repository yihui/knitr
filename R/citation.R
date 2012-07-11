#' Generate BibTeX bibliography databases for R packages
#'
#' This function uses \code{\link[utils]{citation}} and
#' \code{\link[utils]{toBibtex}} to create bib entries for R packages and write
#' them in a file. Only the auto-generated citations are included for a package.
#' This function can facilitate the auto-generation of bibliography databases
#' for R packages, and it is easy to regenerate all the citations after updating
#' R packages.
#'
#' The citation is forced to be generated from the DESCRIPTION file of the
#' package, the keyword \samp{R-pkgname} is used for the bib item where
#' \samp{pkgname} is the name of the package.
#' @param x package names (packages which are not installed are ignored)
#' @param file the (\file{.bib}) file to write (by default writes to the R
#'   console; ignored if it is \code{NULL})
#' @param tweak whether to fix some known problems in the citations, especially
#'   non-standard format of authors
#' @return a list containing the citations (also written to the \code{file} as a
#'   side effect)
#' @note Some packages on CRAN do not have standard bib entries, which was once
#'   reported by Michael Friendly at
#'   \url{https://stat.ethz.ch/pipermail/r-devel/2010-November/058977.html}. I
#'   find this a real pain, and there are no easy solutions except contacting
#'   package authors to modify their DESCRIPTION files. Anyway, the argument
#'   \code{tweak} has provided ugly hacks to deal with packages which are known
#'   to be non-standard in terms of the format of citations; \code{tweak = TRUE}
#'   is by no means intended to hide or modify the original citation
#'   information. It is just due to the loose requirements on package authors
#'   for the DESCRIPTION file. On one hand, I apologize if it really mangles the
#'   information about certain packages; on the other, I strongly recommend
#'   package authors to consider the \samp{Authors@@R} field (see the manual
#'   \emph{Writing R Extensions}) to make it easier for other people to cite R
#'   packages. See \code{knitr:::.tweak.bib} for details of tweaks. Also note
#'   this is subject to future changes since R packages are being updated.
#' @export
#' @examples write_bib(c('RGtk2', 'gWidgets'), file = 'R-GUI-pkgs.bib')
#' write_bib(c('animation', 'rgl', 'knitr', 'ggplot2'))
#' write_bib(c('base', 'parallel', 'MASS'))  # base and parallel are identical
#' write_bib(c('rpart', 'survival'))
#' write_bib(c('rpart', 'survival'), tweak = FALSE) # original version
#'
#' # what tweak=TRUE does
#' str(knitr:::.tweak.bib)
write_bib = function(x = .packages(), file = '', tweak = TRUE) {
  idx = mapply(system.file, package = x) == ''
  if (any(idx)) {
    warning('package(s) ', paste(x[idx], collapse = ', '), ' not found')
    x = x[!idx]
  }
  x = setdiff(x, .base.pkgs) # remove base packages
  bib = sapply(x, function(pkg) {
    cite = citation(pkg, auto = if (pkg == 'base') NULL else TRUE)
    entry = toBibtex(cite)
    entry[1] = sub('\\{,$', sprintf('{R-%s,', pkg), entry[1])
    gsub('', '', entry)
  }, simplify = FALSE)
  if (tweak) {
    for (i in intersect(names(.tweak.bib), x)) {
      message('tweaking ', i)
      b = bib[[i]]; items = .tweak.bib[[i]]
      b[names(items)] = items
      idx = which(names(b) == '')
      bib[[i]] = c(b[idx[1L]], b[-idx], b[idx[2L]])
    }
  }
  bib = bib[sort(x)]
  if (!is.null(file)) cat(unlist(bib), sep = '\n', file = file)
  invisible(bib)
}

.this.year = sprintf('  year = {%s},', format(Sys.Date(), '%Y'))
# hack non-standard entries; to be updated...
.tweak.bib = list(
  cacheSweave = c(author = '  author = {Roger D. Peng},'),
  cluster = c(author = '  author = {Martin Maechler},'),
  evaluate = c(year = .this.year),
  gWidgets = c(author = '  author = {John Verzani},'),
  maps = c(author = '  author = {Ray Brownrigg},'),
  Rcmdr = c(author = '  author = {John Fox},'),
  RGtk2 = c(author = '  author = {Michael Lawrence and Duncan {Temple Lang}},'),
  roxygen2 = c(year = .this.year),
  rpart = c(author = '  author = {Terry M Therneau and Beth Atkinson},'),
  sm = c(author = '  author = {Adrian Bowman and Adelchi Azzalini},'),
  survival = c(author = '  author = {Terry Therneau},'),
  tuneR = c(author = '  author = {Uwe Ligges},')
)
# no need to write bib for these packages
.base.pkgs = setdiff(rownames(installed.packages(priority = 'base')), 'base')
