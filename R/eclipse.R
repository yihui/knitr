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
#' @author Ramnath Vaidyanathan
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
  xml_file = tempfile()
  url = "http://www.eclipsecolorthemes.org/?view=empty&action=download&theme=%s&type=xml"
  download.file(sprintf(url, id), xml_file)
  xml_to_css(xml_file)
}


#' Parse XML file of eclipse theme
#'
#' @param xml_file path to xml file of eclipse theme
#' @return return a list containing theme name and syntax highlighting tokens.
#' @noRd
xml_to_list = function(xml_file){
  library(XML)
  doc      = xmlParse(xml_file)
  docname  = xmlAttrs(xmlRoot(doc), 'colorTheme[@name]')['name']
  docname  = make.names(tolower(docname))
  doclist  = xmlToList(doc)
  list(doclist = doclist, docname = docname)
}

#' Convert XML file with eclipse theme to a CSS style file for use by highlight
#'
#' @param xml_file path to xml file of eclipse theme
#' @return saves css file of eclipse theme to the themes folder of knitr
#' @noRd
##  TODO: Parse more formatting variables like font-weight, font-style etc.
xml_to_css = function(xml_file){
  xml_list   = xml_to_list(xml_file)
  doccolors  = lapply(xml_list$doclist, "[", 'color')
  style      = xml_list$docname
  css_file   = file.path(tempdir(), sprintf("%s.css", style))
  template   = system.file('themes', 'eclipse.brew', package = 'knitr')
  with(doccolors, knit(template, css_file))
  message('Theme ', style, ' saved to ', css_file)
  css_file
}
