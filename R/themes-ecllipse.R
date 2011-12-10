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

#' Download XML style file from http://www.eclipsecolorthemes.org/, convert it
#' to a CSS using a template and save it
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

style_to_latex <- function(style){
  css_file <- file.path('themes/css_files', sprintf("%s.css", style))
  css_out  <- highlight::css.parser(css_file)
  style    <- highlight::styler_assistant_latex(css_out)
  style    <- gsub("[%]", "", style)
  style    <- stringr::str_c(c(style, highlight::boxes_latex()), collapse = '\n')
  return(style)
}

col2knit <- function(color){
  paste(round(col2rgb(color)/255, 2), collapse = ";")
}




