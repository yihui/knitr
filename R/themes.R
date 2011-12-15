#' Sets code theme to use for syntax highlighting
#'
#' @export
#' @author Ramnath Vaidyanathan
#  TODO: change par()$col to foreground colour so that figure displays well.
set_theme <- function(theme){
  header     <- theme_to_header(theme)
  highlight  <- paste(c(header$highlight, boxes_latex()), collapse = "\n")
  opts_chunk$set(background = header$background)
  set_header(highlight = highlight)
  # par(col = theme$foreground)
  return()
}

#' Gets details of code theme for syntax highlighting 
#'
#' @export
#' @author Ramnath Vaidyanathan
get_theme <- function(theme = NULL){
  css_folder <- fetch_css_folder()
  if (is.null(theme)){
    message('List of Available Code Themes')
    css_files <- list.files(css_folder, pattern = "css")
    return(file_path_sans_ext(basename(css_files)))
  } else {
    theme_to_header(theme)
  }
}

#' Fetches css file given theme name
#'
#' @keywords internal
#' @author Ramnath Vaidyanathan
fetch_css <- function(theme){
  css_file   <- sprintf("%s.css", theme)
  css_folder <- fetch_css_folder()
  return(file.path(css_folder, css_file))
}

#' Generates latex header based on theme
#' 
#' @importFrom highlight css.parser styler_assistant_latex
#' @keywords internal
#' @author Ramnath Vaidyanathan
theme_to_header  <- function(theme){
  css_file   <- fetch_css(theme)
  css_out    <- css.parser(css_file)
  
  # get background and foreground colors
  background <- css_out$background$color
  foreground <- css_out$prompt$color
  
  # write latex highlight header
  fgheader  <- color_def(foreground, 'fgcolor')
  highlight <- c(fgheader, styler_assistant_latex(css_out[-1]))
  
  return(list(
    highlight = highlight, background = background, foreground = foreground)
  )
}

