#' Fetches path to style file from the highlight package
style_to_file <- function(style){
  style_file <- sprintf("%s.style", style)
  return(system.file('highlight', 'themes', style_file, package = 'highlight'))
}

#' Reads contents of style file and returns it as a list
file_to_list <- function(style_file){
  style  <- readLines(style_file)
  style  <- style[grep("$", style, fixed = TRUE)]
  style  <- sub("$", "", style, fixed = TRUE)
  style1 <- lapply(strsplit(style, "=|\\s"), "[", 2)
  names(style1) <- lapply(strsplit(style, "="), "[", 1)
  return(style1)
}

#' Generate css file for a style using a template
style_to_css <- function(style){
  style1     <- style %oo% style_to_file %oo% file_to_list
  css_folder <- system.file('themes', package = 'knitr')
  css_file   <- file.path(css_folder, sprintf("%s.css", style))
  template   <- system.file('themes', 'style_template.css', package = 'knitr')
  brew::brew(template, css_file)
  cat('Theme', style, 'saved to', css_file)
}

`%oo%` <- function(x, f){
  f(x)
}

#' Generate CSS files for all styles in the highlight package
generate_css <- function(){
  styles <- list.files(system.file('highlight', 'themes', package = 'highlight'))
  styles <- tools::file_path_sans_ext(styles)
  plyr::l_ply(styles, style_to_css, .progress = 'text', .print = TRUE)
}

