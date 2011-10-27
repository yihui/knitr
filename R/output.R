##' Knit a document
##'
##' This function takes an input file, extracts the R code in it
##' according to a list of patterns, evaluates the code and writes the
##' output in another file.
##' @param input path of the input file
##' @param output path of output file (note the working directory will
##' be set to the directory of the input file, so this argument is
##' usually a filename without a directory name); if not set, this
##' function will try to guess
##' @param pattern the name of the pattern in
##' \code{optk$get('all.patterns')} to be used
##' @return The parsed document is written into the output file.
##' @export
##' @examples library(knitr)
##' (f = tempfile(fileext = '.Rnw'))
##' file.copy(system.file('examples', 'sweave-ex.Rnw', package = 'knitr'), f, overwrite = TRUE)
##' knit(f)
knit = function(input, output, pattern) {

    if (missing(output)) output = basename(auto_out_name(input))

    ext = tolower(file_ext(input))
    apat = opts_knit$get('all.patterns')
    opat = knit_patterns$get(); on.exit(knit_patterns$set(opat), add = TRUE)
    if (length(opat) == 0 || all(sapply(opat, is.null))) {
        if (missing(pattern)) {
            pattern = ext; if (ext == 'md') pattern = 'html'
        }
        if (!(pattern %in% names(apat)))
            stop("a pattern list has not been set up for pattern = '", pattern, "' yet; ",
                 'see ?kpat on how to set up customized patterns')
        knit_patterns$restore()
        knit_patterns$set(apat[[pattern]])
    }

    oopts = opts_knit$get(); on.exit(opts_knit$set(oopts), add = TRUE)
    ohooks = knit_hooks$get(); on.exit(knit_hooks$set(ohooks), add = TRUE)
    if (is.null(oopts$render.to)) {
        render.to =
            switch(ext, rnw = 'tex', tex = 'tex', html = 'html', md = 'jekyll',
                   stop('cannot automatically decide the output filetype'))
        ## set built-in hooks
        opts_knit$set(render.to = render.to)
    }
    switch(opts_knit$get('render.to'), tex = hooks_latex(), html = hooks_html(),
           jekyll = hooks_jekyll())

    owd = setwd(dirname(input)); on.exit(setwd(owd), add = TRUE)
    on.exit(chunk_counter(reset = TRUE), add = TRUE) # restore counter
    ## for tikz graphics (cache the dictionary); turn off fancy quotes
    oopts = options(tikzMetricsDictionary = str_c(sub("([^.]+)\\.[[:alnum:]]+$", "\\1",
                    basename(input)), '-tikzDictionary'), useFancyQuotes = FALSE)
    on.exit(options(oopts), add = TRUE)

    res = process_file(basename(input))
    cat(res, file = output)
    message('output file: ', normalizePath(output))
}

process_file = function(path) {
    groups = split_file(path)
    n = length(groups); res = character(n)

    if (opts_knit$get('progress'))
        pb = txtProgressBar(0, n, char = '>', style = 3)
    for (i in 1:n) {
        if (opts_knit$get('progress')) {
            setTxtProgressBar(pb, i)
            cat('\n')
        }
        res[i] = process_group(groups[[i]])
    }
    if (opts_knit$get('progress')) close(pb)

    res = insert_header(res)  # insert header
    str_c(c(res, ""), collapse = "\n")
}

auto_out_name = function(input) {
    ext = file_ext(input)
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
    knit_hooks$get('output')(comment_out(x, options), options)
}

wrap.source = function(x, options) {
    ## TODO: optionally highlight code here
    src = x$src
    if (options$highlight && identical(opts_knit$get('render.to'), 'tex')) {
        src = hilight_latex(str_c(src, collapse = ''), options)
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
    if (options$single) plot.cur = '' else {
        plot.cur = plot_counter(); options$plot.cur = plot.cur # put fig num in options
    }
    prefix = options$prefix.string
    if (length(prefix) == 0 || is.na(prefix) || prefix == 'NA') prefix = ''
    name = str_c(prefix, options$label, plot.cur)
    if (!file.exists(dirname(name)))
        dir.create(dirname(name)) # automatically creates dir for plots

    knit_hooks$get('plot')(save_plot(x, name, options), options)
}


## TODO: tangle R code
output_code = function(path, output_path = NULL) {
}

extract_code = function(block) {
}
