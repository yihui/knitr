#' Parses theme files from Highlight v3.06 by Andre Simon
#'
#' @param theme_name name of the theme to parse
#' @return a list containing the theme specifications
#' @keywords internal
theme_to_list <- function(theme_name){
  theme <- readLines(sprintf('themes-andre/%s.theme', theme_name))
  y <- theme[grep("\\{.*\\}", theme)]
  y <- gsub("[{]", 'list(', y)
  y <- gsub("^(.*)\\}.*$", '\\1)', y)
  y <- gsub("[;]", "", y)
  y <- paste(y, collapse = ",")
  y <- gsub("true", "TRUE", y, fixed = TRUE)
  y <- eval(parse(text = paste('list(', y, ')')))
  names(y)[names(y) == ""] <- paste('Keyword', 1:4, sep = "")
  return(y)
}

#' Convert theme files from Highlight v3.06 by Andre Simon to CSS 
#'
#' @param theme_name name of the theme to parse
#' @return NULL
#' @keywords internal
theme_to_css <- function(theme_name){
  theme      <- theme_to_list(theme_name)
  css_folder <- fetch_css_folder()
  css_file   <- file.path(css_folder, sprintf("%s.css", theme_name))
  template   <- file.path(css_folder, 'highlight3.brew')
  brew::brew(template, css_file)
  cat('Theme', theme_name, 'saved to', css_file)
}


#' Generate CSS files for all themes in the highlight3 package
generate_css <- function(){
  require(tools)
  require(plyr)
  
  `%oo%` <- function(x, f){
    f(x)
  }
  true <- TRUE
  themes_folder <- file.path('themes-andre')
  themes <- themes_folder %oo% list.files %oo% file_path_sans_ext
  l_ply(themes, failwith(NULL, theme_to_css), .progress = 'text', .print = TRUE)
  return()
}
