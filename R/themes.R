#' Set or get theme to use for syntax highlighting
#' @noRd
#' @author Ramnath Vaidyanathan
set_theme = function(theme) {
	fmt = opts_knit$get('out.format')
  header = if (is.list(theme)) theme else theme_to_header(theme)
  highlight = paste(header$highlight, collapse = "\n")
  if(fmt == 'latex') {
  	opts_chunk$set(background = header$background)
  }
  set_header(highlight = highlight)
  ## par(col = theme$foreground)
}
get_theme = function(theme = NULL) {
  if (is.null(theme)) {
    theme_dir = system.file("themes", package = "knitr")
    theme_files = list.files(theme_dir,  pattern = "\\.css$")
    gsub("\\.css$", "", basename(theme_files))
  } else {
    theme_to_header(theme)
  }
}

#' Syntax highlighting themes
#'
#' This object can be used to set or get themes in \pkg{knitr} for syntax
#' highlighting.
#'
#' We can use \code{knit_theme$set(theme)} to set the theme, and
#' \code{knit_theme$get(theme)} to get a theme. The \code{theme} is a character
#' string for both methods (either the name of the theme, or the path to the CSS
#' file of a theme), and for the \code{set()} method, it can also be a list
#' returned by the \code{get()} method. See examples below.
#' @author Ramnath Vaidyanathan and Yihui Xie
#' @seealso \code{\link{eclipse_theme}} (use Eclipse themes)
#' @references \url{https://github.com/downloads/yihui/knitr/knitr-themes.pdf}
#' (its Rnw source is at
#' \url{https://github.com/yihui/knitr/blob/master/inst/examples/knitr-themes.Rnw})
#' @export
#' @examples knit_theme$set('edit-vim')
#'
#' knit_theme$get()  # names of all available themes
#'
#' thm = knit_theme$get('acid')  # parse the theme to a list
#' knit_theme$set(thm)
knit_theme = list(set = set_theme, get = get_theme)


#' Generates header based on a theme and output format of document
#' @author Ramnath Vaidyanathan
#' @noRd
theme_to_header = function(theme, format = opts_knit$get('out.format')){
  if (format == 'latex') {
    theme_to_header_latex(theme)
  } else theme_to_header_html(theme)
}

#' Generates latex header based on a theme
#' @importFrom highlight css.parser styler_assistant_latex
#' @author Ramnath Vaidyanathan
#' @noRd
theme_to_header_latex = function(theme) {
  css_file = if (file.exists(theme)) theme else {
    system.file("themes", sprintf("%s.css", theme), package = "knitr")
  }
  css_out = css.parser(css_file)

  ## get background and foreground colors
  background = css_out$background$color
  foreground = css_out$prompt$color

  ## write latex highlight header
  fgheader = color_def(foreground, "fgcolor")
  highlight = c(fgheader, styler_assistant_latex(css_out[-1]), boxes_latex())
  list(highlight = highlight, background = background, foreground = foreground)
}

#' Generates css header based on a theme
#' @author Ramnath Vaidyanathan
#' @noRd
#  HACK: replace ugly sub hack to match knitr background with theme
#  TODO: warning, error, source etc. are still black, an issue for dark themes
#  TODO: might be a good idea to regenerate the css files appending the
#        .knitr.css template to the existing templates
theme_to_header_html = function(theme){
  css_file = if (file.exists(theme)) theme else {
    system.file("themes", sprintf("%s.css", theme), package = "knitr")
  }
  bgcolor = css.parser(css_file)$background$color
  css_knitr = system.file('themes', '.knitr.css', package = 'knitr')
  css_knitr_lines = readLines(css_knitr)
  css_knitr_lines = sub('^([[:space:]]+background-color:\\s+)(.*)$',
                          sprintf('\\1%s;', bgcolor), css_knitr_lines)
  css = c(css_knitr_lines, readLines(css_file))
  return(list(highlight = css))
}

