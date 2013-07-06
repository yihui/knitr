#' Download and convert a theme from eclipsecolorthemes.org to CSS
#'
#' This function uses the \pkg{XML} package to parse the theme as an XML file,
#' then converts to a CSS file using a brew template in the \pkg{knitr} package.
#' The CSS file can be further parsed with \code{knit_theme$get()}, and the
#' result will be ready for \code{knit_theme$set()} to set the highlighting
#' theme.
#' @param id id of theme to save as CSS
#' @return Path to the CSS file converted from the website.
#' @references \url{http://www.eclipsecolorthemes.org/}
#' @author Ramnath Vaidyanathan and Yihui Xie
#' @seealso \code{\link{knit_theme}}
#' @export
#' @examples ## http://www.eclipsecolorthemes.org/?view=theme&id=1
#' \dontrun{
#' opts_knit$set(out.format = 'latex')
#' (css = eclipse_theme(1))
#' thm = knit_theme$get(css)
#' knit_theme$set(thm)
#' }
eclipse_theme = function(id){
  library(XML)
  url = 'http://www.eclipsecolorthemes.org/?view=empty&action=download&theme=%s&type=xml'
  doc = xmlParse(sprintf(url, id))
  docname = xmlAttrs(xmlRoot(doc), 'colorTheme[@name]')['name']
  docname = make.names(tolower(docname))
  lst = xmlToList(doc)

  css = character(length(css2ecl))
  for (i in seq_along(css2ecl)) {
    m = css2ecl[i]; l = lst[[m]]
    # if not found, use the default style
    if (is.null(l)) l = lst[['foreground']]
    css[i] = paste(c(
      sprintf('.%s {', names(m)), sprintf('  color: %s;', l['color']),
      sprintf('  font-weight: %s;', if (identical(unname(l['bold']), 'true')) 'bold'),
      sprintf('  font-style: %s;', if (identical(unname(l['italic']), 'true')) 'italic'), '}'
    ), collapse = '\n')
  }

  css_file = file.path(tempdir(), sprintf('%s.css', docname))
  writeLines(css, css_file)
  message('Theme ', docname, ' saved to ', css_file)
  css_file

}

# mapping between CSS class and Eclipse theme elements
css2ecl = c(
  background = 'background', num = 'number', str = 'string', com = 'singleLineComment',
  opt = 'operator', std = 'foreground',
  kwa = 'keyword', kwb = 'localVariableDeclaration', kwc = 'parameterVariable', kwd = 'method'
)
