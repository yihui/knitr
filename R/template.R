#' Automatically create a report based on an R script and a template
#'
#' This is a convenience function for small-scale automatic reporting based on
#' an R script and a template. The default template is an Rnw file (LaTeX);
#' \code{stitch_rhtml()} and \code{stitch_rmd()} are wrappers on top of
#' \code{stitch()} using the R HTML and R Markdown templates respectively.
#'
#' The first two lines of the R script can contain the title and author of the
#' report in comments of the form \samp{## title:} and \samp{## author:}. The
#' template must have a token \samp{\%sCHUNK_LABEL_HERE}, which will be used to
#' input all the R code from the script. See the examples below.
#'
#' The R script may contain chunk headers of the form \samp{## ---- label,
#' opt1=val1, opt2=val2}, which will be copied to the template; if no chunk
#' headers are found, the whole R script will be inserted into the template as
#' one code chunk.
#' @param script path to the R script
#' @param template path of the template to use (by default the Rnw template in
#'   this package; there is also an HTML template in \pkg{knitr})
#' @param output the output filename (passed to \code{\link{knit}}); by default
#'   it uses the base filename of the script
#' @inheritParams knit
#' @return path of the output document
#' @export
#' @seealso \code{\link{spin}} (turn a specially formatted R script to a report)
#' @examples s = system.file('misc', 'stitch-test.R', package = 'knitr')
#' if (interactive()) stitch(s)  # compile to PDF
#'
#' # HTML report
#' stitch(s, system.file('misc', 'knitr-template.Rhtml', package = 'knitr'))
#'
#' # or convert markdown to HTML
#' stitch(s, system.file('misc', 'knitr-template.Rmd', package = 'knitr'))
stitch = function(script,
                  template = system.file('misc', 'knitr-template.Rnw', package = 'knitr'),
                  output = NULL, text = NULL, envir = parent.frame()) {
  lines = if (nosrc <- is.null(text)) readLines(script, warn = FALSE) else split_lines(text)
  # extract title and author from first two lines
  if (comment_to_var(lines[1L], '.knitr.title', '^#+ *title:', envir)) lines = lines[-1L]
  if (comment_to_var(lines[1L], '.knitr.author', '^#+ *author:', envir)) lines = lines[-1L]
  input = basename(template)
  input = sub_ext(basename(if (nosrc) script else tempfile()), file_ext(input))
  txt = readLines(template, warn = FALSE)
  i = grep('%sCHUNK_LABEL_HERE', txt)
  if (length(i) != 1L) stop('Wrong template for stitch: ', template)
  h = sub('CHUNK_LABEL_HERE', '', txt[i])
  j = grep(.sep.label, lines)
  if (length(j) == 0) {
    lines = c(sprintf(h, 'auto-report'), lines)
  } else {
    lines[j] = sprintf(h, gsub(.sep.label, '\\2', lines[j]))
    if (j[1] != 1L) lines = c(sprintf(h, ''), lines)
  }
  txt[i] = paste(lines, collapse = '\n')
  opts_chunk$set(
    fig.align = 'center', par = TRUE, fig.width = 6, fig.height = 6,
    fig.path = paste('figure', gsub('[^[:alnum:]]', '-', input), sep = '/')
  )
  on.exit(opts_chunk$restore(), add = TRUE)
  knit_hooks$set(par = function(before, options, envir) {
    if (before) par(mar = c(4, 4, .1, .1), cex.lab = .95, cex.axis = .9,
                    mgp = c(2, .7, 0), tcl = -.3, las = 1)
  })
  on.exit(knit_hooks$restore(), add = TRUE)

  out = knit(input, output, envir = envir, text = txt)
  switch(file_ext(out), tex = {
    tools::texi2pdf(out, clean = TRUE)
    message('PDF output at: ', sub_ext(out, 'pdf'))
  }, md = {
    out.html = sub_ext(out, 'html')
    markdown::markdownToHTML(out, out.html)
    message('HTML output at: ', out.html)
  })
  out
}
#' @rdname stitch
#' @param ... arguments passed to \code{stitch()}
#' @export
stitch_rhtml = function(...) {
  stitch(..., template = system.file('misc', 'knitr-template.Rhtml', package = 'knitr'))
}
#' @rdname stitch
#' @export
stitch_rmd = function(...) {
  stitch(..., template = system.file('misc', 'knitr-template.Rmd', package = 'knitr'))
}

#' A simple macro preprocessor for templating purposes
#'
#' This function expands a template based on the R expressions in \code{{{}}}
#' (this tag can be customized by the \code{delim} argument). These expressions
#' are extracted, evaluated and replaced by their values in the original
#' template.
#' @param file the template file
#' @param ... a list of variables to be used for the code in the template; note
#'   the variables will be searched in the parent frame as well
#' @param text an alternative way to \code{file} to specify the template code
#'   directly (if provided, \code{file} will be ignored)
#' @param delim the (opening and ending) delimiters for the templating tags
#' @return A character vector, with the tags evaluated and replaced by their
#'   values.
#' @references This function was inspired by the pyexpander
#'   (\url{http://pyexpander.sourceforge.net}) and m4
#'   (\url{http://www.gnu.org/software/m4/}), thanks to Frank Harrell.
#' @export
#' @examples # see the knit_expand vignette
#' if (interactive()) browseVignettes(package='knitr')
knit_expand = function(file, ..., text = readLines(file, warn = FALSE),
                       delim = c('{{', '}}') ){

  # check if delim is a pair, escaping regex as necessary
  if (length(delim) != 2L) stop('"delim" must be of length 2')
  delim = gsub('([.|()\\^{}+$*?]|\\[|\\])', '\\\\\\1', delim)
  delim = paste(delim[1L], '((.|\n)+?)', delim[2L], sep = '')

  txt = paste(text, collapse = '\n')
  if (packageVersion('stringr') <= '0.9.0') delim = stringr::perl(delim)
  loc = stringr::str_locate_all(txt, delim)[[1L]]
  if (nrow(loc) == 0L) return(txt) # no match
  mat = stringr::str_extract_all(txt, delim)[[1L]]
  mat = sub(delim, '\\1', mat)
  env = list(...)
  env = if (length(env)) list2env(env, parent = parent.frame()) else parent.frame()
  inline_exec(list(code = mat, input = txt, location = loc),
              envir = env, hook = identity)
}
