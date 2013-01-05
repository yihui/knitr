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
#' @param delim the delimiter for the templating tags (a Perl regular
#'   expression)
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
  env = if (length(env)) list2env(env) else parent.frame()
  inline_exec(list(code = mat, input = txt, location = loc),
              eval = TRUE, envir = env, stop_on_error = 2L, hook = identity)
}
