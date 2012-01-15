##' Knit a document
##'
##' This function takes an input file, extracts the R code in it
##' according to a list of patterns, evaluates the code and writes the
##' output in another file. It can also tangle R source code from the
##' input document.
##'
##' For most of the time, it is not necessary to set any options
##' outside the input document; in other words, a single call like
##' \code{knit('my_input.Rnw')} is usually enough. This function will
##' try to determine many internal settings automatically. For the
##' sake of reproducibility, it is a better practice to include the
##' options inside the input document (to be self-contained), instead
##' of setting them before knitting the document.
##'
##' First the filename of the output document is determined in this
##' way: \file{foo.Rnw} generates \file{foo.tex}, and for other types
##' of files, the file extension is reserved; if the filename contains
##' \samp{_knit_}, this part will be removed in the output file, e.g.,
##' \file{foo_knit_.html} creates the output \file{foo.html}, so you
##' can use files named in this way as templates; if \samp{_knit_} is
##' not found in the filename, \file{foo.ext} will produce
##' \file{foo-out.ext}. If \code{tangle = TRUE}, \file{foo.ext}
##' generates an R script \file{foo.R}.
##'
##' Based on the file extension of the input document, a list of
##' patterns will be used to extract R code in the document. All
##' built-in pattern lists can be found in
##' \code{opts_knit$get('all.patterns')} (call it
##' \code{apat}). \samp{Rnw} files use the list \code{apat$rnw},
##' \samp{tex} uses the list \code{apat$tex}, \samp{brew} uses
##' \code{apat$brew} and HTML-like files use \code{apat$html}
##' (e.g. \samp{html} and \samp{md} files). You can manually set the
##' pattern list using the \code{\link{knit_patterns}} object, and
##' \pkg{knitr} will respect the setting.
##'
##' According to the output format
##' (\code{opts_knit$get('out.format')}), a set of output hooks will
##' be set to mark up results from R (see
##' \code{\link{render_latex}}). The output format can be LaTeX,
##' Sweave and HTML, etc. The output hooks decide how to mark up the
##' results (you can customize the hooks).
##'
##' See the package website and manuals in the references to know more
##' about \pkg{knitr}, including the full documentation of chunk
##' options and demos, etc.
##' @param input path of the input file
##' @param output path of output file (note the working directory will
##' be set to the directory of the input file, so this argument is
##' usually a filename without a directory name); if not set, this
##' function will try to guess
##' @param tangle whether to tangle the R code from the input file
##' (like \code{\link[utils]{Stangle}})
##' @return The parsed document is written into the output file.
##' @export
##' @references Package homepage: \url{http://yihui.github.com/knitr/}
##'
##' The \pkg{knitr} main manual:
##' \url{https://github.com/downloads/yihui/knitr/knitr-manual.pdf}
##'
##' The \pkg{knitr} graphics manual:
##' \url{https://github.com/downloads/yihui/knitr/knitr-graphics.pdf}
##' @examples library(knitr)
##' (f = tempfile(fileext = '.Rnw'))
##' file.copy(system.file('examples', 'knitr-minimal.Rnw', package = 'knitr'),
##'   f, overwrite = TRUE)
##' knit(f)
##'
##' knit(f, tangle = TRUE)  # extract R code only
knit = function(input, output, tangle = FALSE) {

    if (missing(output)) output = basename(auto_out_name(input, tangle))

    ext = tolower(file_ext(input))
    apat = opts_knit$get('all.patterns')
    opat = knit_patterns$get(); on.exit(knit_patterns$set(opat), add = TRUE)
    if (length(opat) == 0 || all(sapply(opat, is.null))) {
        pattern = if (ext == 'md') 'html' else ext
        if (!(pattern %in% names(apat)))
            stop("a pattern list cannot be automatically found for the file extension '",
                 ext, "' in built-in pattern lists; ",
                 'see ?knit_patterns on how to set up customized patterns')
        knit_patterns$restore()
        knit_patterns$set(apat[[pattern]])
    }

    owd = setwd(dirname(input)); on.exit(setwd(owd), add = TRUE)
    optk = opts_knit$get(); on.exit(opts_knit$set(optk), add = TRUE)
    opts_knit$set(input.dir = getwd())  # record current working dir
    ohooks = knit_hooks$get(); on.exit(knit_hooks$set(ohooks), add = TRUE)
    if (is.null(optk$out.format)) {
        fmt =
            switch(ext, rnw = 'latex', tex = 'latex', html = 'html', md = 'jekyll',
                   brew = 'brew',
                   stop('cannot automatically decide the output format'))
        ## set built-in hooks
        opts_knit$set(out.format = fmt)
    }
    switch(opts_knit$get('out.format'), latex = render_latex(), html = render_html(),
           sweave = {opts_chunk$set(highlight = FALSE); render_sweave()},
           jekyll = render_jekyll(), markdown = render_markdown(),
           gfm = render_gfm())

    on.exit(chunk_counter(reset = TRUE), add = TRUE) # restore counter
    ## for tikz graphics (cache the dictionary); turn off fancy quotes
    oopts =
        options(tikzMetricsDictionary = str_c(sub("([^.]+)\\.[[:alnum:]]+$", "\\1",
                basename(input)), '-tikzDictionary'), useFancyQuotes = FALSE,
                digits = 4L, width = 75L, warn = 1L)
    on.exit(options(oopts), add = TRUE)

    res = process_file(basename(input), tangle)
    setwd(input_dir())
    cat(res, file = output)
    dep_list$restore()  # empty dependency list
    message('output file: ', normalizePath(output))
}

process_file = function(path, tangle) {
    knit_code$restore()  # clear code list
    groups = split_file(path)
    n = length(groups); res = character(n)

    if (opts_knit$get('progress'))
        pb = txtProgressBar(0, n, char = '>', style = 3)
    for (i in 1:n) {
        if (opts_knit$get('progress')) {
            setTxtProgressBar(pb, i)
            if (!tangle) cat('\n')
            flush.console()
        }
        res[i] = (if (tangle) process_tangle else process_group)(groups[[i]])
    }
    if (opts_knit$get('progress')) close(pb)
    knit_code$restore()

    if (!tangle) res = insert_header(res)  # insert header
    str_c(c(res, ""), collapse = "\n")
}

auto_out_name = function(input, tangle = FALSE) {
    ext = file_ext(input)
    if (tangle) return(str_replace(input, str_c(ext, '$'), 'R'))
    if (tolower(ext) == 'rnw') return(str_replace(input, str_c(ext, '$'), 'tex'))
    if (tolower(ext) %in% c('brew', 'tex', 'html', 'md')) {
        if (str_detect(input, '_knit_')) {
            return(str_replace(input, '_knit_', ''))
        } else {
            return(str_replace(input, str_c('(\\.', ext, ')$'), '-out\\1'))
        }
    }
    stop('cannot determine the output filename automatically')
}


##' Wrap evaluated results for output
##'
##' @param x output from \code{\link[evaluate]{evaluate}}
##' @param options list of options used to control output
##' @noRd
##' @S3method wrap list
##' @S3method wrap character
##' @S3method wrap source
##' @S3method wrap warning
##' @S3method wrap message
##' @S3method wrap error
##' @S3method wrap recordedplot
wrap = function(x, options = list()) {
    UseMethod("wrap", x)
}

wrap.list = function(x, options = list()) {
    lapply(x, wrap, options)
}

wrap.character = function(x, options) {
    if (options$results %in% c('tex', 'asis')) return(x)
    knit_hooks$get('output')(comment_out(x, options), options)
}

wrap.source = function(x, options) {
    src = x$src
    if (options$highlight) {
        fmt = opts_knit$get('out.format')
        src = hilight_source(str_c(src, collapse = ''), fmt, options)
    } else if (options$prompt) src = sapply(src, evaluate:::line_prompt, USE.NAMES = FALSE)
    src = str_c(src, collapse = '')
    src = str_replace(src, '([^\n]+)$', '\\1\n')
    knit_hooks$get('source')(src, options)
}

wrap.warning = function(x, options) {
    knit_hooks$get('warning')(comment_out(str_c("Warning message: ", x$message, "\n"),
                                          options), options)
}

wrap.message = function(x, options) {
    msg = str_replace(x$message, "\n$", "") # because message() comes with \n by default
    knit_hooks$get('message')(comment_out(str_c(msg, "\n"), options), options)
}

wrap.error = function(x, options) {
    knit_hooks$get('error')(comment_out(str_c("Error: ", x$message, "\n"), options), options)
}

wrap.recordedplot = function(x, options) {
    if (!is.null(base.dir <- opts_knit$get('base.dir'))) {
        odir = setwd(base.dir); on.exit(setwd(odir)) # switch to abs dir, then restore
    }
    ## figure number sequence for multiple plots
    if (options$plot.num <= 1) plot.cur = 0L else {
        plot.cur = plot_counter()
    }
    options$plot.cur = plot.cur # put fig num in options
    prefix = valid_prefix(options$fig.path)
    name = str_c(prefix, options$label, ifelse(plot.cur == 0L, '', plot.cur))
    if (!file.exists(dirname(name)))
        dir.create(dirname(name), recursive = TRUE) # automatically creates dir for plots

    name.ext = save_plot(x, name, options)
    knit_hooks$get('plot')(name.ext, options)
}
