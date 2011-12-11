#' Fetches theme file given theme name
#'
#' @keywords internal
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
fetch_code_theme  <- function(theme){
  css_file   <- fetch_theme_file(theme)
  css_out    <-  css.parser(css_file)
  
  # get background and foreground colors
  background <- css_out$background$color
  foreground <- css_out$prompt$color
  
  # write latex highlight header
  fg <- col2knit(foreground, ",")
  fgheader <- sprintf("\\definecolor{fgcolor}{rgb}{%s}", fg)
  highlight <- c(fgheader, styler_assistant_latex(css_out[-1]))
  
  return(list(
    highlight = highlight, background = background, foreground = foreground)
  )
}

#' Convert a color to rgb values and output in a format knitr understands
#' @keywords internal
col2knit <- function(color, split = ";"){
  paste(round(col2rgb(color)/255, 2), collapse = split)
}

#' Sets the syntax highlighting theme
#'
#' @export
#  TODO: remove extra line introduced in verbatim environment due to \color.
#  TODO: change par()$col to foreground colour so that figure displays well.
set_code_theme <- function(theme){
  theme <- fetch_code_theme(theme)
  bg    <- col2knit(theme$background, ";")
  hi    <- paste(c(theme$highlight, boxes_latex()), collapse = "\n")
    opts_chunk$set(background = bg)
  set_header(highlight = hi)
  knit_hooks$set(output = function(x, options) {
    paste('{\\color{fgcolor}\\begin{verbatim}', x, '\\end{verbatim}}', sep = "")
  })
  # par(col = theme$foreground)
  return(invisible())
}


#' Generate a pdf of a rnw file using knit and texi2dvi
#'
#' @export
#' @importFrom tools file_path_sans_ext texi2dvi
knit_to_pdf <- function(rnw_file){
  # require(knitr)
  knit(rnw_file)
  tex_file <- sprintf("%s.tex", file_path_sans_ext(rnw_file))
  texi2dvi(tex_file, pdf = TRUE, clean = TRUE)
}


