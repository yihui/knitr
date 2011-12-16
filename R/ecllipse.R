#' Download, parse and save theme from www.eclipsecolorthemes.org to CSS
#'
#' @param theme_id id of theme to save as CSS
#' @return theme name it is saved as.
#' @export
save_eclipse_theme <- function(theme_id){
  xml_file <- download_eclipse_theme(theme_id)
  css_file <- xml_to_css(xml_file)
  return(file_path_sans_ext(basename(css_file)))
}

#' Get ids of top eclipse themes at www.eclipsecolorthemes.org
#'
#' @noRd
#  TODO: Add a filter argument that would allow getting ids by top picks based 
#        on bright, dark and highcontrast themes. The url has the format
#  http://www.eclipsecolorthemes.org/?list=toppicks&filter=highcontrast&q=
top_eclipse_themes <- function(){
  url <- "http://www.eclipsecolorthemes.org/?q="
  doc <- XML::htmlTreeParse(url, useInternalNodes = TRUE)
  theme_links <- XML::xpathSApply(doc, "//div[@onclick]", xmlGetAttr, 'onclick')
  theme_ids <- gsub("^.*id=([0-9]+).*$", "\\1", theme_links)
  return(theme_ids)
}

#' Download eclipse theme given theme_no
#'
#' @  noRd
download_eclipse_theme <- function(theme_id){
  xml_file <- tempfile()
  url <- "http://www.eclipsecolorthemes.org/?view=empty&action=download&theme=%s&type=xml"
  download.file(sprintf(url, theme_id), xml_file)
  return(xml_file)
}

#' Parse XML file of eclipse theme
#'
#' @param xml_file path to xml file of eclipse theme
#' @return return a list containing theme name and syntax highlighting tokens.
xml_to_list <- function(xml_file){
  require(XML)
  doc      <- xmlParse(xml_file)
  docname  <- xmlAttrs(xmlRoot(doc), 'colorTheme[@name]')['name']
  docname  <- make.names(tolower(docname))
  doclist  <- xmlToList(doc) 
  return(list(doclist = doclist, docname = docname))
}

#' Convert XML file with eclipse theme to a CSS style file for use by highlight
#' 
#' @param xml_file path to xml file of eclipse theme
#' @return saves css file of eclipse theme to the themes folder of knitr
#  TODO: Parse more formatting variables like font-weight, font-style etc.
xml_to_css <- function(xml_file){
  xml_list   <- xml_to_list(xml_file)
  doccolors  <- lapply(xml_list$doclist, "[", 'color')
  style      <- xml_list$docname
  css_folder <- fetch_css_folder()
  css_file   <- file.path(css_folder, sprintf("%s.css", style))
  template   <- file.path(css_folder, 'ecllipse.brew')
  brew::brew(template, css_file)
  cat('Theme', style, 'saved to', css_file)
  return(css_file)
}


get_eclipse_themes <- function(page = 1){
  require(XML)
  url <- "http://www.eclipsecolorthemes.org/?list=all&q="
  doc <- htmlParse(url)
  themes <- getNodeSet(doc, "//a[contains(@href, 'view=theme')]")
  theme_names  <- sapply(themes, xmlValue)
  theme_links  <- sapply(themes, xmlGetAttr, 'href') 
  theme_ids     <- gsub("^.*id=([0-9]+).*$", "\\1", theme_links)
  return(data.frame(theme = theme_names, id= theme_ids))
}

# TODO: Write a function that scrapes ecllipsecolorthemes pages and 
# PAGES = 1:15
# eclipse_themes <- plyr::ldply(PAGES, failwith(NULL, get_eclipse_themes), .progress = 'text')

