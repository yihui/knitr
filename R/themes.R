#' Set or get theme to use for syntax highlighting
#' @noRd
#' @author Ramnath Vaidyanathan
set_theme = function(theme) {
  header = if (is.list(theme)) theme else theme_to_header(theme)
  opts_chunk$set(background = header$background)
  set_header(highlight = header$highlight)
  # par(col = theme$foreground)
}
get_theme = function(theme = NULL) {
  if (is.null(theme)) {
    theme_dir = system.file('themes', package = 'knitr')
    theme_files = list.files(theme_dir,  pattern = '\\.css$')
    gsub('\\.css$', '', basename(theme_files))
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
#' @references For a preview of all themes, see
#'   \url{https://gist.github.com/yihui/3422133}.
#' @note The syntax highlighting here only applies to \file{.Rnw} (LaTeX) and
#'   \file{.Rhtml} (HTML) documents, and it does not work for other types of
#'   documents, such as \file{.Rmd} (R Markdown, which has its own syntax
#'   highlighting themes; see \url{http://rmarkdown.rstudio.com}).
#' @export
#' @examples opts_knit$set(out.format='latex'); knit_theme$set('edit-vim')
#'
#' knit_theme$get()  # names of all available themes
#'
#' thm = knit_theme$get('acid')  # parse the theme to a list
#' knit_theme$set(thm)
#'
#' opts_knit$set(out.format=NULL) # restore option
knit_theme = list(set = set_theme, get = get_theme)


#' Generates header based on a theme and output format of document
#' @author Ramnath Vaidyanathan
#' @noRd
theme_to_header = function(theme, format = out_format()){
  if (format == 'latex') {
    theme_to_header_latex(theme)
  } else theme_to_header_html(theme)
}

#' Generates latex header based on a theme
#' @author Ramnath Vaidyanathan
#' @noRd
theme_to_header_latex = function(theme) {
  css_file = if (file.exists(theme)) theme else {
    system.file('themes', sprintf('%s.css', theme), package = 'knitr', mustWork = TRUE)
  }
  css_out = css.parser(css_file)

  # get background and foreground colors
  background = css_out$background$color
  foreground = css_out$std$color

  # write latex highlight header
  fgheader = color_def(foreground, 'fgcolor')
  highlight = one_string(c(fgheader, styler_assistant_latex(css_out[-1])))
  list(highlight = highlight, background = background, foreground = foreground)
}

#' Generates css header based on a theme
#' @author Ramnath Vaidyanathan
#' @noRd
theme_to_header_html = function(theme){
  css_file = if (file.exists(theme)) theme else {
    system.file('themes', sprintf('%s.css', theme), package = 'knitr')
  }
  css = css.parser(css_file)
  bgcolor = css$background$color
  css_knitr = readLines(system.file('misc', 'knitr.css', package = 'knitr'))
  css_knitr[-2] = sub('^(\\s+background-color:\\s+)(.*)$', sprintf('\\1%s;', bgcolor), css_knitr[-2])
  css = c(css_knitr, gsub('^([.][a-z]{3} )', '.hl\\1', readLines(css_file)[-(1:3)]))
  list(highlight = one_string(css))
}

# parse a theme file from Highlight v3.x by Andre Simon to an R list of the form
# list(Colour = hex, Bold = TRUE, Italic = TRUE)
theme2list = function(theme.file) {
  y = readLines(theme.file, warn = FALSE)
  i = grep('^\\s*Description', y)
  if (i > 1) y = y[-seq_len(i - 1)]
  y = gsub('[{]', 'list(', y)
  y = gsub('[}]', ')', y)
  y = gsub(';', '', y)
  y = gsub('true', 'TRUE', y)
  y = one_string(y)
  y = gsub(',\\s*)', ')', y)
  env = new.env()
  #cat(y, sep = '\n')
  eval(parse(text = y), envir = env)
  y = as.list(env)
  for (i in seq_along(y$Keywords)) {
    y[[paste0('Keyword', i)]] = y$Keywords[[i]]
  }
  y$Keywords = NULL
  y
}

# mapping between CSS class and Highlight theme elements
cls2thm = c(
  background = 'Canvas', num = 'Number', str = 'String', com = 'BlockComment',
  opt = 'Operator', std = 'Default',
  kwa = 'Keyword1', kwb = 'Keyword2', kwc = 'Keyword3', kwd = 'Keyword4'
)

# turn a list from theme2list() to CSS code
list2css = function(lst) {
  css = character(length(cls2thm))
  for (i in seq_along(cls2thm)) {
    m = cls2thm[i]; l = lst[[m]]
    # if not found, use the default style
    if (!is.list(l)) l = lst[['Default']]
    css[i] = one_string(c(
      sprintf('.%s {', names(m)), sprintf('  color: %s;', l$Colour),
      sprintf('  font-weight: %s;', if (isTRUE(l$Bold)) 'bold'),
      sprintf('  font-style: %s;', if (isTRUE(l$Italic)) 'italic'), '}'
    ))
  }
  css
}

# generate CSS files for all themes in Andre Simon's Highlight package, e.g.
# themes2css('~/tmp/highlight/themes', '~/downloads/knitr/inst/themes')
themes2css = function(theme.path, css.path) {
  for (f in list.files(theme.path, pattern = '[.]theme$', full.names = TRUE)) {
    theme.name = sub('[.]theme$', '', basename(f))
    css.file = file.path(css.path, sprintf('%s.css', theme.name))
    writeLines(list2css(theme2list(f)), css.file)
    message('theme ', theme.name, ' saved to ', css.file)
  }
}
