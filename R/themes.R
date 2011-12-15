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

add_theme_chunk <- function(rnw_file, theme){
  doc <- readLines(rnw_file)
  begin_line <- grep("begin{document}", doc, fixed = TRUE)
  theme_chunk  <- c(
    "<<set-theme, echo = FALSE, results = hide>>=",
    paste("set_theme(", "'", theme, "'",")", sep = ""),
    "@")
  doc <- c(doc[1:begin_line], theme_chunk, doc[-c(1:begin_line)])
  tf <- tempfile(tmpdir = dirname(rnw_file), fileext = ".rnw")
  writeLines(doc, tf)
  return(path.expand(tf))
}

#' Get path to the codethemes folder
fetch_css_folder <- function(){
  css_folder <- system.file('themes', package = 'knitr')
  # css_folder <- path.expand("~/Desktop/R_Projects/knitr/inst/themes")
  return(css_folder)
}

#' Insert line numbers for code lines
#'
#' @author Ramnath Vaidyanathan
insert_line_numbers <- function(tex_file){
  require(stringr)
  doc <- readLines(tex_file)
  code_lines   <- grep("^(\\\\hlstd|\\\\hlfunctioncall)", doc)
  max_lines    <- length(code_lines)
  line_numbers <- str_pad(seq_along(code_lines), width = 2, side = "left")

  # line_numbers <- str_c("\\1\\\\hlline\\{", line_numbers, "   \\}\\2")
  line_numbers <- str_c("\\\\hlline\\{", line_numbers, "   \\}\\1\\2")

  pat <- "^(\\\\hlstd\\{\\}|\\\\hlfunctioncall\\{.*\\})(.*)$"
  doc[code_lines] <- str_replace(doc[code_lines], pat, line_numbers)
  writeLines(doc, tex_file)
  return(path.expand(tex_file))
}
