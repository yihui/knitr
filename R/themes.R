##' Set or get theme to use for syntax highlighting
##' @noRd
##' @author Ramnath Vaidyanathan
set_theme = function(theme) {
    header = if (is.list(theme)) theme else theme_to_header(theme)
    highlight = paste(c(header$highlight, boxes_latex()), collapse = "\n")
    opts_chunk$set(background = header$background)
    set_header(highlight = highlight)
    ## par(col = theme$foreground)
}
get_theme = function(theme = NULL) {
    if (is.null(theme)) {
        f = list.files(system.file("themes", package = "knitr"), pattern = "\\.css$")
        gsub("\\.css$", "", basename(f))
    } else {
        theme_to_header(theme)
    }
}

##' Syntax highlighting themes
##'
##' This object can be used to set or get themes in \pkg{knitr} for
##' syntax highlighting.
##'
##' We can use \code{knit_theme$set(theme)} to set the theme, and
##' \code{knit_theme$get(theme)} to get a theme. The \code{theme} is a
##' character string for both methods (either the name of the theme,
##' or the path to the CSS file of a theme), and for the \code{set()}
##' method, it can also be a list returned by the \code{get()}
##' method. See examples below.
##' @author Ramnath Vaidyanathan and Yihui Xie
##' @seealso \code{\link{eclipse_theme}} (use Eclipse themes)
##' @references
##' \url{https://github.com/downloads/yihui/knitr/knitr-themes.pdf}
##' (its Rnw source is at
##' \url{https://github.com/yihui/knitr/blob/master/inst/examples/knitr-themes.Rnw})
##' @export
##' @examples knit_theme$set('edit-vim')
##'
##' knit_theme$get()  # names of all available themes
##'
##' thm = knit_theme$get('acid')  # parse the theme to a list
##' knit_theme$set(thm)
knit_theme = list(set = set_theme, get = get_theme)

##' Generates latex header based on a theme
##' @importFrom highlight css.parser styler_assistant_latex
##' @author Ramnath Vaidyanathan
##' @noRd
theme_to_header = function(theme) {
    css_file = if (file.exists(theme)) theme else {
        system.file("themes", sprintf("%s.css", theme), package = "knitr")
    }
    css_out = css.parser(css_file)

    ## get background and foreground colors
    background = css_out$background$color
    foreground = css_out$prompt$color

    ## write latex highlight header
    fgheader = color_def(foreground, "fgcolor")
    highlight = c(fgheader, styler_assistant_latex(css_out[-1]))

    list(highlight = highlight, background = background, foreground = foreground)
}
