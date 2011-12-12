#' Sets code theme to use for syntax highlighting
#'
#' @export
#' @author Ramnath Vaidyanathan
#  TODO: change par()$col to foreground colour so that figure displays well.
set_code_theme <- function(theme){
  theme <- fetch_code_theme(theme)
  hi <- paste(c(theme$highlight, boxes_latex()), collapse = "\n")
  opts_chunk$set(background = theme$background)
  set_header(highlight = hi)
  # par(col = theme$foreground)
  return()
}

#' Gets details of code theme for syntax highlighting 
#'
#' @export
#' @author Ramnath Vaidyanathan
get_code_theme <- function(theme = NULL){
  # TODO: replace with system.file during installation
  # theme_folder <- system.file(package = 'knitr')
  theme_folder <- path.expand("~/Desktop/R_Projects/knitr/inst")
  if (is.null(theme)){
    message('List of Available Code Themes')
    theme_files <- list.files(file.path(theme_folder, 'codethemes'), 
      pattern = "css")
    return(file_path_sans_ext(basename(theme_files)))
  } else {
    fetch_code_theme(theme)
  }
}

#' Fetches theme file given theme name
#'
#' @keywords internal
#' @author Ramnath Vaidyanathan
fetch_theme_file <- function(theme){
  theme_file <- sprintf("%s.css", theme)
  # TODO: replace with system.file during installation
  theme_folder <- path.expand("~/Desktop/R_Projects/knitr/inst")
  # theme_folder <- system.file(package = 'knitr')
  return(file.path(theme_folder, 'codethemes', theme_file))
}

#' Generates latex header based on theme
#' 
#' @importFrom highlight css.parser styler_assistant_latex
#' @keywords internal
#' @author Ramnath Vaidyanathan
fetch_code_theme  <- function(theme){
  css_file   <- fetch_theme_file(theme)
  css_out    <-  css.parser(css_file)
  
  # get background and foreground colors
  background <- css_out$background$color
  foreground <- css_out$prompt$color
  
  # write latex highlight header
  fgheader <- color_def(foreground, 'fgcolor')
  highlight <- c(fgheader, styler_assistant_latex(css_out[-1]))
  
  return(list(
    highlight = highlight, background = background, foreground = foreground)
  )
}

