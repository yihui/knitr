##' Knit a document
##'
##' This function takes an input file, extracts the R code in it
##' according to a list of patterns, evaluates the code and writes the
##' output in another file. It can also tangle R source code from the
##' input document (\code{purl()} is a wrapper to \code{knit(...,
##' tangle = TRUE)}).
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
##' @param output path of the output file; if \code{NULL}, this
##' function will try to guess and it will be under the same directory
##' as \code{input}
##' @param tangle whether to tangle the R code from the input file
##' (like \code{\link[utils]{Stangle}})
##' @param text a character vector as an alternative way to provide
##' the input file (\code{text} is written into a temporary file as
##' \code{input}); this argument is mainly for test purposes only
##' @return The compiled document is written into the output file, and
##' the path of the output file is returned.
##' @note The name \code{knit} comes from its counterpart \samp{weave}
##' (as in Sweave), and the name \code{purl} (as \samp{tangle} in
##' Stangle) comes from a knitting method `knit one, purl one'.
##'
##' If the input document has child documents, they will also be
##' compiled recursively. See \code{\link{knit_child}}.
##'
##' The working directory when evaluating R code chunks is the
##' directory of the input document, so if the R code involves with
##' external files (like \code{read.table()}), it is better to put
##' these files under the same directory of the input document so that
##' we can use relative paths. It is recommended to change the working
##' directory to the input directory before calling \code{knit()},
##' especially when the input document contains child documents.
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
##' ## or setwd(dirname(f)); knit(basename(f))
##'
##' purl(f)  # extract R code only
knit = function(input, output = NULL, tangle = FALSE, text = NULL) {
    if (is.character(text)) {
        input = tempfile(); writeLines(text, con = input)
    }
    opts_knit$set(tangle = tangle)
    if (is.null(output)) {
        output = file.path(dirname(input), basename(auto_out_name(input, tangle)))
    }
    ext = tolower(file_ext(input))
    apat = opts_knit$get('all.patterns')
    optc = opts_chunk$get()
    on.exit({opts_chunk$restore(); opts_chunk$set(optc)}, add = TRUE)
    opat = knit_patterns$get()
    on.exit({knit_patterns$restore(); knit_patterns$set(opat)}, add = TRUE)
    if (length(opat) == 0 || all(sapply(opat, is.null))) {
        pattern = if (ext == 'md') 'html' else ext
        if (!(pattern %in% names(apat)))
            stop("a pattern list cannot be automatically found for the file extension '",
                 ext, "' in built-in pattern lists; ",
                 'see ?knit_patterns on how to set up customized patterns')
        knit_patterns$restore()
        knit_patterns$set(apat[[pattern]])
    }

    optk = opts_knit$get(); on.exit(opts_knit$set(optk), add = TRUE)
    if (is.null(optk$input.dir)) {
        opts_knit$set(input.dir = dirname(input))  # record current working dir
    }
    if (is.null(opts_knit$get('out.format'))) {
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

    progress = opts_knit$get('progress')
    message(ifelse(progress, '\n\n', ''), 'processing file: ', input)
    res = process_file(input, tangle)
    unlink('NA')  # temp fix to issue 94
    cat(res, file = output)
    dep_list$restore()  # empty dependency list
    if (is.character(output) && file.exists(output)) {
        message('output file: ', normalizePath(output), ifelse(progress, '\n', ''))
    }
    invisible(output)
}
##' @rdname knit
##' @param ... arguments passed to \code{\link{knit}}
##' @export
purl = function(...) {
    knit(..., tangle = TRUE)
}

process_file = function(path, tangle) {
    ocode = knit_code$get()
    on.exit({knit_code$restore(); knit_code$set(ocode)}, add = TRUE)
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
        txt = try((if (tangle) process_tangle else process_group)(groups[[i]]))
        if (inherits(txt, 'try-error')) break
        res[i] = txt
    }
    if (opts_knit$get('progress')) close(pb)

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

##' Knit a child document
##'
##' This function is for LaTeX only except when it is used to extract
##' R code from the document; by default it knits a child document and
##' returns the command to input the result into the main document. It
##' is designed to be used in the inline R code and serves as the
##' alternative to the \command{SweaveInput} command in Sweave.
##'
##' The LaTeX command used to input the child document (usually
##' \samp{input} or \samp{include}) is from the package option
##' \code{child.command} (\code{opts_knit$get('child.command')}).
##'
##' When we call \code{knit_child(tangle = TRUE)} to extract R code,
##' the code in the child document is extracted and saved into an R
##' script.
##' @param ... arguments passed to \code{\link{knit}}
##' @param eval logical: whether to evaluate the child document
##' @return A character string of the form
##' \samp{\command{child-doc.tex}} or \code{source("child-doc.R")},
##' depending on the argument \code{tangle} passed in.
##' @references \url{http://yihui.github.com/knitr/demo/child/}
##' @export
##' @examples ## you can write \Sexpr{knit_child('child-doc.Rnw')} in an Rnw file 'main.Rnw' to input child-doc.tex in main.tex
##'
##' ## comment out the child doc by \Sexpr{knit_child('child-doc.Rnw', FALSE)}
##'
##' ## use \include: opts_knit$set(child.command = 'include')
knit_child = function(..., eval = TRUE) {
    if (!eval) return('')
    path = knit(...)
    if (opts_knit$get('tangle')) {
        str_c('source("', path, '")')
    } else str_c('\\', opts_knit$get('child.command'), '{', path, '}')
}

##' Automatically create a report based on an R script and a template
##'
##' This is a convenience function for small-scale automatic reporting
##' based on an R script and a template.
##'
##' The first two lines of the R script can contain the title and
##' author of the report in comments of the form \samp{## title:} and
##' \samp{## author:}. The template must have a chunk named
##' \samp{auto-report}, which will be used to input all the R code
##' from the script. See the examples below.
##' @param script path to the R script
##' @param template path of the template to use (by default the Rnw
##' template in this package; there is also an HTML template in
##' \pkg{knitr})
##' @param output the output filename (passed to \code{\link{knit}});
##' by default it uses the base filename of the script
##' @return path of the output document
##' @export
##' @examples s = system.file('misc', 'stitch-test.R', package = 'knitr')
##' \dontrun{stitch(s)}
##'
##' ## HTML report
##' out = stitch(s, system.file('misc', 'knitr-minimal_knit_.html', package = 'knitr'))
##' if (interactive()) browseURL(out)
stitch = function(script, template = system.file('misc', 'knitr-template.Rnw',
                          package = 'knitr'), output = NULL) {
    lines = readLines(script, warn = FALSE)
    ## extract title and author from first two lines
    if (comment_to_var(lines[1L], '.knitr.title', '^#+ *title:')) lines = lines[-1L]
    if (comment_to_var(lines[1L], '.knitr.author', '^#+ *author:')) lines = lines[-1L]
    knit_code$set(`auto-report` = lines)
    input = basename(template)
    input = str_c(file_path_sans_ext(basename(script)), '.', file_ext(input))
    file.copy(template, input, overwrite = TRUE)
    out = knit(input, output)
    if (str_detect(out, '\\.tex$')) {
        texi2pdf(out, clean = TRUE)
        system(paste(getOption('pdfviewer'), shQuote(str_replace(out, '\\.tex$', '.pdf'))))
    }
    knit_code$restore()
    out
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
    } else if (options$prompt) src = sapply(src, line_prompt, USE.NAMES = FALSE)
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
    if (options$fig.num <= 1) fig.cur = 0L else {
        fig.cur = plot_counter()
    }
    options$fig.cur = fig.cur # put fig num in options
    prefix = valid_prefix(options$fig.path)
    name = str_c(prefix, options$label, ifelse(fig.cur == 0L, '', fig.cur))
    if (!file.exists(dirname(name)))
        dir.create(dirname(name), recursive = TRUE) # automatically creates dir for plots

    name.ext = save_plot(x, name, options)
    knit_hooks$get('plot')(name.ext, options)
}
