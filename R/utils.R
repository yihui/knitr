## copy objects in one environment to the other
copy_env = function(from, to) {
    x = ls(envir = from, all = TRUE)
    for (i in x) {
        assign(i, get(i, envir = from, inherits = FALSE), envir = to)
    }
}


knit_counter = function(init = 0L) {
    n = init
    function(reset = FALSE) {
        if (reset) return(n <<- init)
        n <<- n + 1L
        n - 1L
    }
}

plot_counter = knit_counter(1L)
chunk_counter = knit_counter(1L)

## add a prefix to output
comment_out = function(x, options) {
    prefix = options$comment
    if (!is.null(prefix) && nzchar(prefix) && !is.na(prefix)) {
        prefix = str_c(prefix, ' ')
        evaluate:::line_prompt(x, prompt = prefix, continue = prefix)
    } else x
}

hiren_latex = renderer_latex(document = FALSE)
hiren_html = renderer_html(document = FALSE, header = function() '', footer = function() '')

hilight_source = function(x, format, options) {
    if (!(format %in% c('latex', 'html'))) return(x)
    con = textConnection(x)
    on.exit(close(con))
    r = if (format == 'latex') hiren_latex else hiren_html
    enc = getOption('encoding')
    options(encoding = 'native.enc')  # make sure parser() writes with correct enc
    on.exit(options(encoding = enc), add = TRUE)
    out = capture.output(highlight(con, renderer = r, showPrompts = options$prompt, size = options$size))
    str_c(out, collapse = '\n')
}

is_blank = function(x) {
    str_detect(x, '^\\s*$')
}
valid_prefix = function(x) {
    if (length(x) == 0 || is.na(x) || x == 'NA') return('')
    x
}

## define a color variable in TeX
color_def = function(col, variable = 'shadecolor') {
    x = sc_split(col)
    if ((n <- length(x)) != 3L) {
        if (n == 1L) x = drop(col2rgb(x)/255) else {
            x = switch(variable, shadecolor = rep(.97, 3), fgcolor = rep(0, 3))
            warning("the color '", col, "' is invalid;",
                    "using default color...",
                    "see http://yihui.github.com/knitr/options")
        }
    }
    sprintf('\\definecolor{%s}{rgb}{%s, %s, %s}', variable, x[1], x[2], x[3])
}

## split by semicolon
sc_split = function(string) {
    str_split(string, fixed(';'))[[1]]
}

## extract LaTeX packages for tikzDevice
set_tikz_opts = function(input, cb, ce) {
    if (opts_knit$get('out.format') != 'latex') return()
    hb = knit_patterns$get('header.begin')
    if (length(hb) == 1L) {
        idx = str_detect(input, hb)
        if (any(idx)) {
            options(tikzDocumentDeclaration = input[idx][1])
            db = knit_patterns$get('document.begin')
            if (length(db) == 1L) {
                idx2 = str_detect(input, db)
                if (any(idx2)) {
                    idx = which(idx)[1]; idx2 = which(idx2)[1]
                    if (idx2 - idx > 1) {
                        preamble = pure_preamble(input[seq(idx + 1, idx2 - 1)], cb, ce)
                        .knitEnv$tikzPackages = c(preamble, '\n')
                    }
                }
            }
        }
    }
}
## filter out code chunks from preamble if they exist (they do in LyX/Sweave)
pure_preamble = function(preamble, chunk.begin, chunk.end) {
    blks = which(str_detect(preamble, chunk.begin))
    if (!length(blks)) return(preamble)
    ends = which(str_detect(preamble, chunk.end))
    idx = unlist(mapply(seq, from = blks, to = ends, SIMPLIFY = FALSE))
    preamble[-idx]
}
## whether to write results as-is?
output_asis = function(x, options) {
    is_blank(x) || options$results %in% c('tex', 'asis')
}

## path relative to dir of the input file
input_dir = function() {
    id = opts_knit$get('input.dir')
    if (is.null(id)) return('.')
    id
}

## scientific notation in TeX
format_sci = function(x, format = 'latex', d = getOption('digits')) {
    if (!is.numeric(x)) return(x)
    if (any(abs(lx <- floor(log(abs(x), 10))) >= d)) {
        b = formatC(x/10^lx)
        b[b %in% c('1', '-1')] = ''
        if (format == 'latex')
            return(sprintf('$%s%s10^{%s}$', b, ifelse(b == '', '', '\\times '), floor(lx)))
        if (format == 'html')
            return(sprintf('%s%s10<sup>%s</sup>', b, ifelse(b == '', '', ' &times; '), floor(lx)))
    }
    formatC(x)
}

## absolute path?
is_abs_path = function(x) {
    if (.Platform$OS.type == 'windows')
        grepl(':', x, fixed = TRUE) || grepl('^\\\\', x) else grepl('^[/~]', x)
}

## compatibility with Sweave and old beta versions of knitr
fix_options = function(options) {
    ## compatibility with Sweave
    for (dev in c('pdf', 'eps', 'jpeg', 'png')) {
        if (isTRUE(options[[dev]])) {
            options$dev = dev
            warning("chunk option ", dev,
                    "=TRUE deprecated in knitr; use new option 'dev' please")
            break
        }
    }
    if (options$dev == 'eps') options$dev = 'postscript'

    ## compatibility with old version of knitr
    fig = options$fig
    if (identical(fig, FALSE)) {
        warning("option 'fig' deprecated; use fig.keep=none please")
        options$fig.keep = 'none'
    } else if (identical(fig, TRUE)) {
        if (isTRUE(options$fig.last)) {
            warning("option 'fig.last' deprecated; use fig.keep=last please")
            options$fig.keep = 'last'
        }
        if (isTRUE(options$fig.low)) {
            warning("option 'fig.low' deprecated; use fig.keep=all please")
            options$fig.keep = 'all'
        }
    }
    hold = options$fig.hold
    if (identical(hold, FALSE)) {
        warning("option 'fig.hold' deprecated; use fig.show=asis please")
        options$fig.show = 'asis'
    } else if (identical(hold, TRUE)) {
        warning("option 'fig.hold' deprecated; use fig.show=hold please")
        options$fig.show = 'hold'
    }
    if (isTRUE(options$animate)) {
        warning("option 'animate' deprecated; use fig.show=animate please")
        options$fig.show = 'animate'
    }

    align = options$align
    if (!is.null(align)) {
        warning("option 'align' deprecated; use fig.align instead")
        options$fig.align = align
    }
    width = options$width
    if (!is.null(width)) {
        warning("option 'width' deprecated; use fig.width instead")
        options$fig.width = width
    }
    height = options$height
    if (!is.null(height)) {
        warning("option 'height' deprecated; use fig.height instead")
        options$fig.height = height
    }

    prefix = options$prefix.string
    if (!is.null(prefix)) {
        warning("option 'prefix.string' deprecated; use fig.path instead")
        options$fig.path = prefix
    }
    prefix = options$prefix.cache
    if (!is.null(prefix)) {
        warning("option 'prefix.cache' deprecated; use cache.path instead")
        options$cache.path = prefix
    }

    options
}

## try eval an option (character) to an expected result
eval_opt = function(x, expect_class = is.logical) {
    if (expect_class(x)) return(x)
    res = eval(parse(text = x), envir = globalenv())
    if (!expect_class(res)) stop('option value ', x, ' did not give an expected result')
    res
}

##' Path for figure files
##'
##' The filename of figure files is the combination of options
##' \code{fig.path} and \code{label}. This function returns the path
##' of figures for the current chunk by default.
##' @param options a list of options; by default the options of the
##' current chunk
##' @return A character string (path)
##' @note When there are multiple figures in a chunk, this function
##' only provides a prefix of the filenames, and the actually
##' filenames are of the form \file{prefix1}, \file{prefix2},
##' ... where \file{prefix} is the string returned by this function.
##' @export
##' @examples fig_path(list(fig.path='figure/abc-', label='first-plot'))
fig_path = function(options = opts_current$get()) {
    str_c(valid_prefix(options$fig.path), options$label)
}

#' Generate a pdf of a rnw file using knit and texi2dvi
#'
#' @author Ramnath Vaidyanathan
#' @export
#' @importFrom tools file_path_sans_ext texi2dvi
knit2pdf <- function(rnw_file, theme = NULL, line_numbers = FALSE){
  # require(knitr)
  tex_file_name <- sprintf("%s.tex", file_path_sans_ext(rnw_file))
  if (!missing(theme)){
    rnw_file <- add_theme_chunk(rnw_file, theme)
    on.exit(unlink(rnw_file))
  }
  knit(rnw_file)
  tex_file <- sprintf("%s.tex", file_path_sans_ext(rnw_file))
  if (line_numbers) {
    tex_file <- insert_line_numbers(tex_file)
    # on.exit(unlink(tex_file))
  }
  file.rename(tex_file, tex_file_name)
  oldwd <- getwd()
  setwd(dirname(tex_file_name)) 
  texi2dvi(basename(tex_file_name), pdf = TRUE, clean = TRUE)
  setwd(oldwd)
}
