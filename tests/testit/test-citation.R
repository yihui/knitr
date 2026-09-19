library(testit)

assert('bib_nocite_keys() extracts the citation keys from write_bib() entries', {
  # a package with multiple BibTeX entries: the auto-generated R- key plus two
  # extra preferred citations, each with its own key
  bib = list(pkg = c(
    '@Manual{R-pkg,', '  title = {A},', '}', '',
    '@Book{pkg2020,', '  title = {B},', '}', '',
    '@Article{pkg2019,', '  title = {C},', '}'
  ))
  (bib_nocite_keys(bib) %==% '@R-pkg, @pkg2020, @pkg2019')
})

# an end-to-end check against a real package (knitr itself is always available)
assert('pkg_bib_nocite() returns @-prefixed keys for an installed package', {
  s = suppressWarnings(pkg_bib_nocite('knitr'))
  (grepl('^@R-knitr(,|$)', s))
})
