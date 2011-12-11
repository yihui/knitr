#' Download theme from http://www.eclipsecolorthemes.org as XML, given theme number
download_theme_xml <- function(theme_no){
  xml_file = tempfile()
  url <- "http://www.eclipsecolorthemes.org/?view=empty&action=download&theme=%s&type=xml"
  download.file(sprintf(url, theme_no), xml_file)
  return(xml_file)
}

#' Parse XML file of color theme from http://www.eclipsecolorthemes.org/ and
#' return a list containing theme name and syntax highlighting tokens.
xml_to_list <- function(xml_file){
  require(XML)
  doc      <- xmlParse(xml_file)
  docname  <- xmlAttrs(xmlRoot(doc), 'colorTheme[@name]')['name']
  docname  <- make.names(tolower(docname))
  doclist  <- xmlToList(doc) 
  return(list(doclist = doclist, docname = docname))
}

#' Convert XML style file downloaded from http://www.eclipsecolorthemes.org/
#' to a CSS style file using a template and save it to the themes folder.
#  TODO: Right now the css converter only uses the color specifications. It
#  would be useful to add other specs like font-weight, font-style etc.
xml_to_css <- function(xml_file){
  xml_list   <- xml_to_list(xml_file)
  doccolors  <- lapply(xml_list$doclist, "[", 'color')
  style      <- xml_list$docname
  css_folder <- system.file('themes', package = 'knitr')
  css_file   <- file.path(css_folder, sprintf("%s.css", style))
  template   <- system.file('themes', 'ecllipse_template.css', package = 'knitr')
  brew::brew(template, css_file)
  cat('Theme', style, 'saved to', css_file)
}

save_eclipse_theme <- function(theme_no){
  xml_file <- download_theme_xml(theme_no)
  xml_to_css(xml_file)
}


