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
#' The R script may contain chunk headers of the form \samp{## @@knitr label},
#' which will be copied to the template; if no chunk headers are found, the
#' whole R script will be inserted into the template as one code chunk.
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
#' \dontrun{stitch(s)}
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
  ## extract title and author from first two lines
  if (comment_to_var(lines[1L], '.knitr.title', '^#+ *title:', envir)) lines = lines[-1L]
  if (comment_to_var(lines[1L], '.knitr.author', '^#+ *author:', envir)) lines = lines[-1L]
  read_chunk(lines = lines)
  if (length(knit_code$get()) == 0L) knit_code$set(`auto-report` = lines)
  input = basename(template)
  input = sub_ext(basename(if (nosrc) script else tempfile()), file_ext(input))
  txt = readLines(template, warn = FALSE)
  i = grep('%sCHUNK_LABEL_HERE', txt)
  if (length(i) != 1L) stop('Wrong template for stitch: ', template)
  txt[i] = paste(sprintf(sub('CHUNK_LABEL_HERE', '', txt[i]), names(knit_code$get())),
                 unlist(lapply(knit_code$get(), paste, collapse = '\n')),
                 sep = '\n', collapse = '\n')
  knit_code$restore()
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
    texi2pdf(out, clean = TRUE)
    message('PDF output at: ', str_replace(out, '\\.tex$', '.pdf'))
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
  stitch(..., template = system.file('misc', 'knitr-template.Rhtml', package = 'knitr'))
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
#' @examples knit_expand(text = 'The value of pi is {{pi}}.')
#' knit_expand(text = 'The value of a is {{a}}, so a + 1 is {{a+1}}.', a = rnorm(1))
#' knit_expand(text = 'The area of a circle with radius {{r}} is {{pi*r^2}}', r = 5)
#' ## any number of variables
#' knit_expand(text = 'a is {{a}} and b is {{b}}, with my own pi being {{pi}} instead of {{base::pi}}', a=1, b=2, pi=3)
#' ## custom delimiter <% %>
#' knit_expand(text = 'I do not like curly braces, so use % with <> instead: a is <% a %>.', a = 8, delim = c("<%", "%>"))
#' ## the pyexpander delimiter
#' knit_expand(text = 'hello $(LETTERS[24]) and $(pi)!', delim = c("$(", ")"))
#' ## arbitrary R code
#' knit_expand(text = 'you cannot see the value of x {{x=rnorm(1)}}but it is indeed created: x = {{x}}')
#' knit_expand(text = c(' x | x^2', '{{x=1:5;paste(sprintf("%2d | %3d", x, x^2), collapse = "\n")}}'))
#'
#' ## the m4 example: http://en.wikipedia.org/wiki/M4_(computer_language)
#' knit_expand(text = c('{{i=0;h2=function(x){i<<-i+1;sprintf("<h2>%d. %s</h2>", i, x)} }}<html>',
#' '{{h2("First Section")}}', '{{h2("Second Section")}}', '{{h2("Conclusion")}}', '</html>'))
#'
#' ## build regression models based on a template; loop through all vars in mtcars
#' src = lapply(names(mtcars)[-1], function(i) {
#' knit_expand(text=c("# Regression on {{i}}", '```{r lm-{{i}}}', 'lm(mpg~{{i}}, data=mtcars)', '```'))
#' })
#' # knit the source
#' cat(knit(text = unlist(src)))
knit_expand = function(file, ..., text = readLines(file, warn = FALSE),
                       delim = c("{{", "}}") ){

  # check if delim is a pair, escaping regex as necessary
  if (length(delim) != 2L) stop('"delim" must be of length 2')
  delim = gsub('([.|()\\^{}+$*?]|\\[|\\])', '\\\\\\1', delim)
  delim = str_c(delim[1L], '((.|\n)+?)', delim[2L])

  txt = str_c(text, collapse = '\n'); delim = perl(delim)
  loc = str_locate_all(txt, delim)[[1L]]
  if (nrow(loc) == 0L) return(txt) # no match
  mat = str_extract_all(txt, delim)[[1L]]
  mat = str_replace(mat, delim, '\\1')
  env = list(...)
  env = if (length(env)) list2env(env, parent = parent.frame()) else parent.frame()
  inline_exec(list(code = mat, input = txt, location = loc),
              eval = TRUE, envir = env, stop_on_error = 2L, hook = identity)
}
