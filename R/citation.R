#' Generate BibTeX bibliography databases for R packages
#'
#' This function uses \code{\link[utils]{citation}} and
#' \code{\link[utils]{toBibtex}} to create bib entries for R packages and write
#' them in a file. Only the auto-generated citations are included for a package.
#' This function can facilitate the auto-generation of bibliography databases
#' for R packages, and it is easy to regenerate all the citations after updating
#' R packages.
#'
#' The citation is forced to be generated from the \file{DESCRIPTION} file of
#' the package (\code{citation(auto = TRUE)}). The keyword \samp{R-pkgname} is
#' used for the bib item, where \samp{pkgname} is the name of the package. All
#' references specified in the \file{CITATION} file of the package are ignored.
#' The main purpose of this function is to automate the generation of the
#' package citation information because it often changes (e.g. author, year,
#' package version, ...). By comparison, paper/book citations don't change too
#' often, so it is not a lot of work even if you just cut and paste such
#' bibliography entries from \code{toBibtex(citation())}.
#'
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
#'   this is subject to future changes since R packages are being updated. If
#'   you want to contribute more tweaks, please edit the file
#'   \file{inst/misc/tweak_bib.csv} in the source package.
#' @export
#' @author Yihui Xie and Michael Friendly
#' @examples write_bib(c('RGtk2', 'gWidgets'), file = 'R-GUI-pkgs.bib')
#' write_bib(c('animation', 'rgl', 'knitr', 'ggplot2'))
#' write_bib(c('base', 'parallel', 'MASS'))  # base and parallel are identical
#' write_bib('cluster', prefix = '')  # a empty prefix
#' write_bib('digest', prefix = 'R-pkg-')  # a new prefix
#' write_bib('digest', tweak = FALSE)  # original version
#'
#' # what tweak=TRUE does
#' str(knitr:::.tweak.bib)
write_bib = function(
  x = .packages(), file = '', tweak = TRUE, width = NULL,
  prefix = getOption('knitr.bib.prefix', 'R-')
) {
  idx = mapply(system.file, package = x) == ''
  if (any(idx)) {
    warning('package(s) ', paste(x[idx], collapse = ', '), ' not found')
    x = x[!idx]
  }
  x = setdiff(x, .base.pkgs) # remove base packages
  bib = sapply(x, function(pkg) {
    cite = citation(pkg, auto = if (pkg == 'base') NULL else TRUE)
    if (tweak) {
      # e.g. gpairs has "gpairs: " in the title
      cite$title = gsub(sprintf('^(%s: )(\\1)', pkg), '\\1', cite$title)
      # e.g. KernSmooth has & in the title
      cite$title = gsub(' & ', ' \\\\& ', cite$title)
    }
    entry = toBibtex(cite)
    entry[1] = sub('\\{,$', sprintf('{%s%s,', prefix, pkg), entry[1])
    entry
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
      if (!is.null(width)) b[-idx] = stringr::str_wrap(b[-idx], width, 2, 4)
      structure(c(b[idx[1L]], b[-idx], b[idx[2L]]), class = 'Bibtex')
    })
  }
  bib = bib[sort(x)]
  if (!is.null(file)) cat(unlist(bib), sep = '\n', file = file)
  invisible(bib)
}

.this.year = sprintf('  year = {%s},', format(Sys.Date(), '%Y'))

#' @include utils.R

# hack non-standard author fields
.tweak.bib = local({
  x = read.csv(inst_dir('misc/tweak_bib.csv'), stringsAsFactors = FALSE)
  x = x[order(x$package), , drop = FALSE]  # reorder entries by package names
  write.csv(x, inst_dir('misc/tweak_bib.csv'), row.names = FALSE)
  setNames(
    lapply(x$author, function(a) c(author = sprintf('  author = {%s},', a))),
    x$package
  )
})

# no need to write bib for these packages
.base.pkgs = setdiff(rownames(installed.packages(priority = 'base')), 'base')
