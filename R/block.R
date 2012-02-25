## S3 method to deal with chunks and inline text respectively
##' @S3method process_group block
##' @S3method process_group inline
process_group = function(x) {
  UseMethod('process_group', x)
}
process_group.block = function(x) call_block(x)
process_group.inline = function(x) call_inline(x)


call_block = function(block) {
  ## now try eval all options except those in eval.after and their aliases
  af = opts_knit$get('eval.after'); al = opts_knit$get('aliases')
  if (!is.null(al) && !is.null(af)) af = c(af, names(al[af %in% al]))
  for (o in setdiff(names(block$params), af))
    block$params[[o]] = eval_lang(block$params[[o]])

  params = opts_chunk$merge(block$params)
  params = fix_options(params)  # for compatibility
  opts_current$restore(); opts_current$set(params)  # save current options
  label = ref.label = params$label
  if (!is.null(params$ref.label)) ref.label = sc_split(params$ref.label)
  params$code = unlist(knit_code$get(ref.label), use.names = FALSE)
  if (opts_knit$get('progress')) print(block)
  
  if (params$eval && !is.null(params$child)) {
    cmds = lapply(sc_split(params$child), knit_child)
    return(str_c(unlist(cmds), collapse = '\n'))
  }
  
  if ((!params$eval && isFALSE(params$echo)) || length(params$code) == 0 ||
    all(is_blank(params$code)))
    return('') # a trivial chunk; do nothing
  
  if (is_tikz_dev(params)) set_header(tikz = '\\usepackage{tikz}')
  
  params$code = parse_chunk(params$code) # parse sub-chunk references
  
  ## Check cache
  content = list(params[setdiff(names(params), 'include')], getOption('width'))
  content[[3L]] = opts_knit$get('cache.extra')
  hash = str_c(valid_prefix(params$cache.path), params$label, '_', digest(content))
  params$hash = hash
  if (params$cache && cache$exists(hash)) {
    if (!params$include) return('')
    cache$load(hash)
    return(cache$output(hash))
  }
  block_exec(params)
}

block_exec = function(params) {
  code = params$code
  options = params
  
  ## tidy code if echo
  echo = options$echo
  if (!isFALSE(echo) && options$tidy) {
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
  .knitEnv$knit_env = env # make a copy of the envir
  
  ## open a graphical device to record graphics
  dargs = formals(getOption('device'))  # is NULL in RStudio's GD
  if (is.null(dargs) || !interactive()) {
    pdf(file = NULL)
  } else dev.new()
  dv = dev.cur(); on.exit(dev.off(dv))
  
  keep = options$fig.keep
  dev.control(displaylist = if (keep != 'none') 'enable' else 'inhibit')  # enable recording
  
  ## guess plot file type if it is NULL
  if (keep != 'none' && is.null(options$fig.ext)) {
    options$fig.ext = dev2ext(options$dev)
  }
  
  res.before = run_hooks(before = TRUE, options, env) # run 'before' hooks
  owd = setwd(input_dir())
  res = evaluate(code, envir = env) # run code
  setwd(owd)
  
  ## eval other options after the chunk
  for (o in opts_knit$get('eval.after')) options[[o]] = eval_lang(options[[o]], env)

  ## remove some components according options
  if (isFALSE(echo)) {
    res = Filter(Negate(is.source), res)
  } else if (is.numeric(echo)) {
    ## choose expressions to echo using a numeric vector
    iss = which(sapply(res, is.source))
    if (length(idx <- setdiff(iss, iss[echo]))) res = res[-idx]
  }
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
    if (keep == 'none') {
      res = res[!figs] # remove all
    } else {
      if (options$fig.show == 'hold') res = c(res[!figs], res[figs]) # move to the end
      res = Filter(function(x) {
        ## filter out plot objects purely for layout (raised by par(), layout())
        !is.recordedplot(x) || !all(plot_calls(x) %in% c('par', 'layout'))
      }, res)
      figs = sapply(res, is.recordedplot)
      if (sum(figs) > 1) {
        if (keep %in% c('first', 'last')) {
          res = res[-(if (keep == 'last') head else tail)(which(figs), -1L)]
        } else {
          ## merge low-level plotting changes
          if (keep == 'high') res = merge_low_plot(res, figs)
        }
      }
    }
  }
  ## number of plots in this chunk
  if (is.null(options$fig.num)) {
    options$fig.num = if (length(res)) sum(sapply(res, is.recordedplot)) else 0L
  }
  
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
    ## purge my old cache and cache of chunks dependent on me
    cache$purge(str_c(valid_prefix(options$cache.path),
                      c(options$label, dep_list$get(options$label)), '_*'))
    cache$save(c(ls(env, all.names = TRUE), outname), hash)
  }
  
  if (!options$include) '' else output
}


call_inline = function(block) {
  
  ## change global options if detected inline options
  options = block$params = lapply(block$params, eval_lang)  # try eval global options
  if (length(options)) opts_chunk$set(options)
  if (opts_knit$get('progress')) print(block)
  
  inline_exec(block)
}

inline_exec = function(block) {
  
  ## run inline code and substitute original texts
  code = block$code; input = block$input
  if ((n <- length(code)) == 0) return(input) # untouched if no code is found
  
  owd = setwd(input_dir()); on.exit(setwd(owd))
  loc = block$location
  for (i in 1:n) {
    res = try(eval(parse(text = code[i]), envir = globalenv()))
    d = nchar(input)
    ## replace with evaluated results
    str_sub(input, loc[i, 1], loc[i, 2]) = if (length(res)) {
      if (inherits(res, 'try-error')) {
        knit_hooks$get('error')(str_c('\n', res, '\n'), opts_chunk$get())
      } else knit_hooks$get('inline')(res)
    } else ''
    if (i < n) loc[(i + 1):n, ] = loc[(i + 1):n, ] - (d - nchar(input))
    ## may need to move back and forth because replacement may be longer or shorter
  }
  input
}

##' @S3method process_tangle block
##' @S3method process_tangle inline
process_tangle = function(x) {
  UseMethod('process_tangle', x)
}
process_tangle.block = function(x) {
  params = opts_chunk$merge(x$params)
  label = params$label
  code = if (!isFALSE(params$eval) && !is.null(params$child)) {
    cmds = lapply(sc_split(params$child), knit_child)
    str_c(unlist(cmds), collapse = '\n')
  } else knit_code$get(label)
  label_code(parse_chunk(code), label)
}
process_tangle.inline = function(x) {
  code = x$code
  if ((n <- length(code)) == 0 || !any(idx <- str_detect(code, "knit_child\\(.+\\)")))
    return('')
  str_c(str_c(sapply(code[idx], function(z) eval(parse(text = z))),
              collapse = '\n'), '\n')
}


## add a label to a code chunk
label_code = function(code, label) {
  str_c(str_c('## @knitr ', label), '\n', str_c(code, collapse = '\n'), '\n')
}
