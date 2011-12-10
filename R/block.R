## S3 method to deal with chunks and inline text respectively
process_group = function(x) {
    UseMethod('process_group', x)
}
process_group.block = function(x) call_block(x)
process_group.inline = function(x) call_inline(x)


call_block = function(block) {
    params = opts_chunk$merge(block$params)
    params = fix_sweave_opts(params)  # for compatibility with Sweave
    params = c(list(code = block$code), params)
    ## code from external R script
    if (!length(params$code)) {
        ref = params$ref
        if (!is.null(ref) && file.exists(ref)) {
            ext.code = .knitEnv$ext.code
            if (!identical(ref, .knitEnv$ext.path)) ext.code = parse_external(ref)
            params$code = ext.code[[params$label]]
        }
    }
    if (opts_knit$get('progress')) print(block)

    if ((!params$eval && !params$echo) || length(params$code) == 0 ||
        all(is_blank(params$code)))
        return('') # a trivial chunk; do nothing

    if (params$dev == 'tikz') set_header(tikz = '\\usepackage{tikz}')

    ## Check cache
    hash =
        str_c(valid_prefix(params$prefix.cache), params$label, '_',
              digest(list(params, getOption('width'))))
    params = c(params, list(hash = hash))
    if (params$cache && cache$exists(hash)) {
        if (!dependson_changed(params$dependson, params$prefix.cache)) {
            cache$load(hash)
            cache$unmark(hash)
            return(cache$output(hash))
        }
    }
    do.call('block_exec', params)
}

block_exec = function(code, ...) {
    options = list(...)

    ## tidy code if echo
    if (options$echo && options$tidy) {
        res = try(tidy.source(text = code, output = FALSE), silent = TRUE)
        if (!inherits(res, 'try-error')) {
            code = res$text.tidy
            enc = Encoding(code)
            idx = enc != 'unknown'
            ## convert non-native enc
            if (any(idx)) code[idx] = iconv(code[idx], enc[idx])
        } else warning('failed to tidy R code in chunk <', options$label, '>\n',
                       'reason: ', res)
    }
    ## no eval chunks
    if (!options$eval) {
        return(knit_hooks$get('chunk')(wrap.source(list(src = str_c(code, collapse = '\n')),
                                                   options), options))
    }

    ## eval chunks (in an empty envir if cache)
    env = if (options$cache) new.env(parent = globalenv()) else globalenv()

    ## TODO: https://github.com/yihui/knitr/issues/9
    dargs = formals(getOption('device'))
    if (!is.null(dargs$file)) {
        dev.new(file = tempfile())
    } else if (is.null(dargs)) {
        pdf(file = tempfile())  # should not use RStudio's GD
    } else dev.new()
    dv = dev.cur(); on.exit(dev.off(dv))
    dev.control(displaylist = 'enable')  # need to record plots

    res.before = run_hooks(before = TRUE, options, env) # run 'before' hooks
    res = evaluate(code, envir = env) # run code

    ## remove some components according options
    if (!options$echo)
        res = Filter(Negate(is.source), res)
    if (options$results == 'hide')
        res = Filter(Negate(is.character), res)
    if (!options$warning)
        res = Filter(Negate(is.warning), res)
    if (!options$error)
        res = Filter(Negate(is.error), res)
    if (!options$message)
        res = Filter(Negate(is.message), res)

    ## rearrange locations of figures
    figs = sapply(res, is.recordedplot)
    if (length(figs) && any(figs)) {
        if (!options$fig) {
            res = res[!figs] # remove all
        } else {
            if (options$fig.hold) res = c(res[!figs], res[figs]) # rearrange to the end
            figs = sapply(res, is.recordedplot) # only keep last plot
            if (sum(figs) > 1) {
                if (options$fig.last) res = res[-head(which(figs), sum(figs) - 1)] else {
                    ## merge low-level plotting changes
                    if (!options$fig.low) res = merge_low_plot(res, figs)
                }
            }
        }
    }
    ## number of plots in this chunk
    options$plot.num = if (length(res)) sum(sapply(res, is.recordedplot)) else 0L

    ## merge source lines if they do not have output; is there an elegant way??
    iss = if (length(res)) which(sapply(res, is.source)) else NULL
    if ((n <- length(iss)) > 1) {
        k1 = iss[1]; k2 = NULL
        for (i in 1:(n - 1)) {
            if (iss[i + 1] - iss[i] == 1) {
                res[[k1]] =
                    structure(list(src = c(res[[k1]]$src, res[[iss[i + 1]]]$src)),
                              class = 'source')  # CAUTION: now node src is a vector!!
                k2 = c(k2, iss[i + 1])
            } else k1 = iss[i + 1]
        }
        if (length(k2)) res = res[-k2] # remove lines that have been merged back
    }

    owd = setwd(input_dir()); on.exit(setwd(owd), add = TRUE)
    output = str_c(unlist(wrap(res, options)), collapse = '') # wrap all results together

    res.after = run_hooks(before = FALSE, options, env) # run 'after' hooks
    if (options$cache) copy_env(env, globalenv())

    output = str_c(c(res.before, output, res.after), collapse = '')  # insert hook results
    output = if (length(output) == 0L) '' else knit_hooks$get('chunk')(output, options)
    plot_counter(reset = TRUE)  # restore plot number

    if (options$cache) {
        hash = options$hash
        outname = str_c('.', hash)
        assign(outname, output, envir = globalenv())
        cache$purge(str_c(valid_prefix(options$prefix.cache),
                          options$label, '_*')) # try to purge old cache
        cache$save(c(ls(env, all = TRUE), outname), hash)
        cache$mark(hash)
    }

    output
}


call_inline = function(block) {

    ## change global options if detected inline options
    options = block$params  # for compatibility
    if (length(options)) opts_chunk$set(options)
    if (opts_knit$get('progress')) print(block)

    inline_exec(block)
}

inline_exec = function(block) {

    ## run inline code and substitute original texts
    code = block$code; input = block$input
    if ((n <- length(code)) == 0) return(input) # untouched if no code is found

    loc = block$location
    for (i in 1:n) {
        res = eval(parse(text = code[i]), envir = globalenv())
        d = nchar(input)
        ## replace with evaluated results
        str_sub(input, loc[i, 1], loc[i, 2]) = if (length(res)) {
            knit_hooks$get('inline')(res)
        } else ''
        if (i < n) loc[(i + 1):n, ] = loc[(i + 1):n, ] - (d - nchar(input))
        ## may need to move back and forth because replacement may be longer or shorter
    }
    input
}

process_tangle = function(x) {
    UseMethod('process_tangle', x)
}
process_tangle.block = function(x) {
    label_code(x$code, x$params$label)
}
process_tangle.inline = function(x) return('')


## add a label to a code chunk
label_code = function(code, label) {
    str_c(str_c('## @knitr ', label), '\n', str_c(code, collapse = '\n'), '\n')
}
