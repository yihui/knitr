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
#' @param prefix a prefix string for keys in BibTeX entries; by default, it is
#'   \samp{R-} unless \code{\link{option}('knitr.bib.prefix')} has been set to
#'   another string
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
#' @author Yihui Xie and Michael Friendly
#' @examples write_bib(c('RGtk2', 'gWidgets'), file = 'R-GUI-pkgs.bib')
#' write_bib(c('animation', 'rgl', 'knitr', 'ggplot2'))
#' write_bib(c('base', 'parallel', 'MASS'))  # base and parallel are identical
#' write_bib('cluster', prefix = '')  # a empty prefix
#' write_bib('digest', prefix = 'R-pkg-')  # a new prefix
#' write_bib(c('rpart', 'survival'))
#' write_bib(c('rpart', 'survival'), tweak = FALSE) # original version
#'
#' # what tweak=TRUE does
#' str(knitr:::.tweak.bib)
write_bib = function(x = .packages(), file = '', tweak = TRUE,
                     prefix = getOption('knitr.bib.prefix', 'R-')) {
  idx = mapply(system.file, package = x) == ''
  if (any(idx)) {
    warning('package(s) ', paste(x[idx], collapse = ', '), ' not found')
    x = x[!idx]
  }
  x = setdiff(x, .base.pkgs) # remove base packages
  bib = sapply(x, function(pkg) {
    cite = citation(pkg, auto = if (pkg == 'base') NULL else TRUE)
    entry = toBibtex(cite)
    entry[1] = sub('\\{,$', sprintf('{%s%s,', prefix, pkg), entry[1])
    gsub('', '', entry)
  }, simplify = FALSE)
  if (tweak) {
    for (i in intersect(names(.tweak.bib), x)) {
      message('tweaking ', i)
      bib[[i]] = merge_list(bib[[i]], .tweak.bib[[i]])
    }
    bib = lapply(bib, function(b) {
      b['author'] = sub('Duncan Temple Lang', 'Duncan {Temple Lang}', b['author'])
      if (!('year' %in% names(b))) b['year'] = .this.year
      idx = which(names(b) == '')
      structure(c(b[idx[1L]], b[-idx], b[idx[2L]]), class = 'Bibtex')
    })
  }
  bib = bib[sort(x)]
  if (!is.null(file)) cat(unlist(bib), sep = '\n', file = file)
  invisible(bib)
}

.this.year = sprintf('  year = {%s},', format(Sys.Date(), '%Y'))
# hack non-standard entries; to be updated...
.tweak.bib = list(
  akima = c(author = '  author = {H. Akima and Albrecht Gebhardt and Thomas Petzoldt and Martin Maechler},'),
  ash = c(author = '  author = {David W. Scott and Albrecht Gebhardt and Stephen Kaluzny},'),
  bcpa = c(author = '  author = {Jose Claudio Faria and Clarice Garcia Borges Demetrio},'),
  bitops = c(author = '  author = {Steve Dutky and Martin Maechler and Steve Dutky},'),
  cacheSweave = c(author = '  author = {Roger D. Peng},'),
  cat = c(author = '  author = {Ted Harding and Fernando Tusell and Joseph L. Schafer},'),
  CircStats = c(author = '  author = {Ulric Lund and Claudio Agostinelli},'),
  cluster = c(author = '  author = {Martin Maechler},'),
  contrast = c(author = '  author = {Max Kuhn and Steve Weston and Jed Wing and James Forester},'),
  date = c(author = '  author = {Terry Therneau and Thomas Lumley and Kjetil Halvorsen and Kurt Hornik},'),
  digest = c(author = '  author = {Dirk Eddelbuettel},'),
  fortunes = c(author = '  author = {Achim Zeileis and the R community},'),
  gWidgets = c(author = '  author = {John Verzani},'),
  hexbin = c(author = '  author = {Dan Carr and Nicholas Lewin-Koh and Martin Maechler},'),
  Hmisc =  c(author = '  author = {Harrell, Jr., Frank E},'),
  leaps = c(author = '  author = {Thomas Lumley},'),
  maps = c(author = '  author = {Ray Brownrigg},'),
  oz = c(author = '  author = {Bill Venables and Kurt Hornik},'),
  pbivnorm = c(author = '  author = {Alan Genz and Brenton Kenkel},'),
  pscl = c(author = '  author = {Simon Jackman and Alex Tahk and Achim Zeileis and Christina Maimone and Jim Fearon},'),
  quadprog = c(author = '  author = {Berwin A. Turlach and Andreas Weingessel},'),
  randomForest = c(author = '  author = {Leo Breiman and Adele Cutler and Andy Liaw and Matthew Wiener},'),
  Rcpp = c(author = '  author = {Dirk Eddelbuettel and Romain Francois},'),
  rgl = c(author = '  author = {Daniel Adler and Duncan Murdoch},'),
  RgoogleMaps = c(author = '  author = {Markus Loecher},'),
  robustbase = c(author = '  author = {Valentin Todorov and Andreas Ruckstuhl and Matias Salibian-Barrera and Tobias Verbeke and Manuel Koller and Martin Maechler},'),
  RODBC = c(author = '  author = {Brian Ripley and Michael Lapsley},'),
  rpart = c(author = '  author = {Terry M Therneau and Beth Atkinson},'),
  shiny = c(author = '  author = {{RStudio,}{ Inc.}},'),
  Sleuth2 = c(author = '  author = {F. L. Ramsey and D. W. Schafer and Jeannie Sifneos and Berwin A. Turlach},'),
  sm = c(author = '  author = {Adrian Bowman and Adelchi Azzalini},'),
  survival = c(author = '  author = {Terry Therneau},'),
  tuneR = c(author = '  author = {Uwe Ligges},')
)
# no need to write bib for these packages
.base.pkgs = setdiff(rownames(installed.packages(priority = 'base')), 'base')
