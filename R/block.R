# S3 method to deal with chunks and inline text respectively
process_group = function(x) {
  UseMethod('process_group', x)
}
#' @export
process_group.block = function(x) call_block(x)
#' @export
process_group.inline = function(x) {
  x = call_inline(x)
  knit_hooks$get('text')(x)
}


call_block = function(block) {
  # now try eval all options except those in eval.after and their aliases
  af = opts_knit$get('eval.after'); al = opts_knit$get('aliases')
  if (!is.null(al) && !is.null(af)) af = c(af, names(al[af %in% al]))

  # expand parameters defined via template
  if (!is.null(block$params$opts.label)) {
    block$params = merge_list(opts_template$get(block$params$opts.label), block$params)
  }

  params = opts_chunk$merge(block$params)
  opts_current$restore(params)
  for (o in setdiff(names(params), af)) params[o] = list(eval_lang(params[[o]]))
  params = fix_options(params)  # for compatibility

  label = ref.label = params$label
  if (!is.null(params$ref.label)) ref.label = sc_split(params$ref.label)
  params[["code"]] = params[["code"]] %n% unlist(knit_code$get(ref.label), use.names = FALSE)
  if (opts_knit$get('progress')) print(block)

  if (!is.null(params$child)) {
    if (!is_blank(params$code)) warning(
      "The chunk '", params$label, "' has the 'child' option, ",
      "and this code chunk must be empty. Its code will be ignored."
    )
    if (!params$eval) return('')
    cmds = lapply(sc_split(params$child), knit_child, options = block$params)
    out = paste(unlist(cmds), collapse = '\n')
    return(out)
  }

  params$code = parse_chunk(params$code) # parse sub-chunk references

  ohooks = opts_hooks$get()
  for (opt in names(ohooks)) {
    hook = ohooks[[opt]]
    if (!is.function(hook)) {
      warning("The option hook '", opt, "' should be a function")
      next
    }
    if (!is.null(params[[opt]])) params = as.strict_list(hook(params))
    if (!is.list(params))
      stop("The option hook '", opt, "' should return a list of chunk options")
  }

  # Check cache
  if (params$cache > 0) {
    content = c(
      params[if (params$cache < 3) cache1.opts else setdiff(names(params), cache0.opts)],
      getOption('width'), if (params$cache == 2) params[cache2.opts]
    )
    if (params$engine == 'R' && isFALSE(params$cache.comments)) {
      content[['code']] = parse_only(content[['code']])
    }
    hash = paste(valid_path(params$cache.path, label), digest(content), sep = '_')
    params$hash = hash
    if (cache$exists(hash, params$cache.lazy) &&
        isFALSE(params$cache.rebuild) &&
        params$engine != 'Rcpp') {
      if (opts_knit$get('verbose')) message('  loading cache from ', hash)
      cache$load(hash, lazy = params$cache.lazy)
      cache_engine(params)
      if (!params$include) return('')
      if (params$cache == 3) return(cache$output(hash))
    }
    if (params$engine == 'R')
      cache$library(params$cache.path, save = FALSE) # load packages
  } else if (label %in% names(dep_list$get()) && !isFALSE(opts_knit$get('warn.uncached.dep')))
    warning2('code chunks must not depend on the uncached chunk "', label, '"')

  params$params.src = block$params.src
  opts_current$restore(params)  # save current options

  # set local options() for the current R chunk
  if (is.list(params$R.options)) {
    op = options(params$R.options); on.exit(options(op), add = TRUE)
  }

  block_exec(params)
}

# options that should affect cache when cache level = 1,2
cache1.opts = c('code', 'eval', 'cache', 'cache.path', 'message', 'warning', 'error')
# more options affecting cache level 2
cache2.opts = c('fig.keep', 'fig.path', 'fig.ext', 'dev', 'dpi', 'dev.args', 'fig.width', 'fig.height')
# options that should not affect cache
cache0.opts = c('include', 'out.width.px', 'out.height.px', 'cache.rebuild')

block_exec = function(options) {
  # when code is not R language
  if (options$engine != 'R') {
    res.before = run_hooks(before = TRUE, options)
    engine = get_engine(options$engine)
    output = in_dir(input_dir(), engine(options))
    if (is.list(output)) output = unlist(output)
    res.after = run_hooks(before = FALSE, options)
    output = paste(c(res.before, output, res.after), collapse = '')
    output = knit_hooks$get('chunk')(output, options)
    if (options$cache) block_cache(options, output, switch(
      options$engine,
      'stan' = options$engine.opts$x, 'sql' = options$output.var, character(0)
    ))
    return(if (options$include) output else '')
  }

  # eval chunks (in an empty envir if cache)
  env = knit_global()
  obj.before = ls(globalenv(), all.names = TRUE)  # global objects before chunk

  keep = options$fig.keep
  keep.idx = NULL
  if (is.numeric(keep)) {
    keep.idx = keep
    keep = "index"
  }
  tmp.fig = tempfile(); on.exit(unlink(tmp.fig), add = TRUE)
  # open a device to record plots
  if (chunk_device(options$fig.width[1L], options$fig.height[1L], keep != 'none',
                   options$dev, options$dev.args, options$dpi, options, tmp.fig)) {
    # preserve par() settings from the last code chunk
    if (keep.pars <- opts_knit$get('global.par'))
      par2(opts_knit$get('global.pars'))
    showtext(options$fig.showtext)  # showtext support
    dv = dev.cur()
    on.exit({
      if (keep.pars) opts_knit$set(global.pars = par(no.readonly = TRUE))
      dev.off(dv)
    }, add = TRUE)
  }

  res.before = run_hooks(before = TRUE, options, env) # run 'before' hooks

  code = options$code
  echo = options$echo  # tidy code if echo
  if (!isFALSE(echo) && options$tidy && length(code)) {
    res = try_silent(do.call(
      formatR::tidy_source, c(list(text = code, output = FALSE), options$tidy.opts)
    ))
    if (!inherits(res, 'try-error')) {
      code = res$text.tidy
    } else warning('failed to tidy R code in chunk <', options$label, '>\n',
                   'reason: ', res)
  }
  # only evaluate certain lines
  if (is.numeric(ev <- options$eval)) {
    # group source code into syntactically complete expressions
    if (!options$tidy) code = sapply(highr:::group_src(code), paste, collapse = '\n')
    iss = seq_along(code)
    code = comment_out(code, '##', setdiff(iss, iss[ev]), newline = FALSE)
  }
  # guess plot file type if it is NULL
  if (keep != 'none' && is.null(options$fig.ext))
    options$fig.ext = dev2ext(options$dev)

  cache.exists = cache$exists(options$hash, options$cache.lazy)
  evaluate = knit_hooks$get('evaluate')
  # return code with class 'source' if not eval chunks
  res = if (is_blank(code)) list() else if (isFALSE(ev)) {
    as.source(code)
  } else if (cache.exists && isFALSE(options$cache.rebuild)) {
    fix_evaluate(cache$output(options$hash, 'list'), options$cache == 1)
  } else in_dir(
    input_dir(),
    evaluate(
      code, envir = env, new_device = FALSE,
      keep_warning = !isFALSE(options$warning),
      keep_message = !isFALSE(options$message),
      keep_package_startup_message = !isFALSE(options$package.startup.message),
      stop_on_error = if (options$error && options$include) 0L else 2L,
      output_handler = knit_handlers(options$render, options)
    )
  )
  if (options$cache %in% 1:2 && (!cache.exists || isTRUE(options$cache.rebuild))) {
    # make a copy for cache=1,2; when cache=2, we do not really need plots
    res.orig = if (options$cache == 2) remove_plot(res, keep == 'high') else res
  }

  # eval other options after the chunk
  if (!isFALSE(ev))
    for (o in opts_knit$get('eval.after'))
      options[o] = list(eval_lang(options[[o]], env))

  # remove some components according options
  if (isFALSE(echo)) {
    res = Filter(Negate(evaluate::is.source), res)
  } else if (is.numeric(echo)) {
    # choose expressions to echo using a numeric vector
    res = if (isFALSE(ev)) {
      as.source(code[echo])
    } else {
      filter_evaluate(res, echo, evaluate::is.source)
    }
  }
  if (options$results == 'hide') res = Filter(Negate(is.character), res)
  if (options$results == 'hold') {
    i = vapply(res, is.character, logical(1))
    if (any(i)) res = c(res[!i], merge_character(res[i]))
  }
  res = filter_evaluate(res, options$warning, evaluate::is.warning)
  res = filter_evaluate(res, options$message, evaluate::is.message)

  # rearrange locations of figures
  figs = find_recordedplot(res)
  if (length(figs) && any(figs)) {
    if (keep == 'none') {
      res = res[!figs] # remove all
    } else {
      if (options$fig.show == 'hold') res = c(res[!figs], res[figs]) # move to the end
      figs = find_recordedplot(res)
      if (length(figs) && sum(figs) > 1) {
        if (keep %in% c('first', 'last')) {
          res = res[-(if (keep == 'last') head else tail)(which(figs), -1L)]
        } else {
          # keep only selected
          if (keep == 'index') res = res[which(figs)[keep.idx]]
          # merge low-level plotting changes
          if (keep == 'high') res = merge_low_plot(res, figs)
        }
      }
    }
  }
  # number of plots in this chunk
  if (is.null(options$fig.num))
    options$fig.num = if (length(res)) sum(sapply(res, evaluate::is.recordedplot)) else 0L

  # merge neighbor elements of the same class into one element
  for (cls in c('source', 'message', 'warning')) res = merge_class(res, cls)

  if (isTRUE(options$fig.beforecode)) res = fig_before_code(res)

  on.exit({
    plot_counter(reset = TRUE)
    shot_counter(reset = TRUE)
  }, add = TRUE)  # restore plot number

  output = unlist(wrap(res, options)) # wrap all results together
  res.after = run_hooks(before = FALSE, options, env) # run 'after' hooks

  output = paste(c(res.before, output, res.after), collapse = '')  # insert hook results
  output = knit_hooks$get('chunk')(output, options)

  if (options$cache > 0) {
    # if cache.vars has been specifically provided, only cache these vars and no
    # need to look for objects in globalenv()
    obj.new = if (is.null(options$cache.vars)) setdiff(ls(globalenv(), all.names = TRUE), obj.before)
    copy_env(globalenv(), env, obj.new)
    objs = if (isFALSE(ev) || length(code) == 0) character(0) else
      options$cache.vars %n% codetools::findLocalsList(parse_only(code))
    # make sure all objects to be saved exist in env
    objs = intersect(c(objs, obj.new), ls(env, all.names = TRUE))
    if (options$autodep) {
      # you shall manually specify global object names if find_symbols() is not reliable
      cache$objects(
        objs, options$cache.globals %n% find_symbols(code), options$label,
        options$cache.path
      )
      dep_auto()
    }
    if (options$cache < 3) {
      if (options$cache.rebuild || !cache.exists) block_cache(options, res.orig, objs)
    } else block_cache(options, output, objs)
  }

  if (options$include) output else if (is.null(s <- options$indent)) '' else s
}

block_cache = function(options, output, objects) {
  hash = options$hash
  outname = cache_output_name(hash)
  assign(outname, output, envir = knit_global())
  purge_cache(options)
  cache$library(options$cache.path, save = TRUE)
  cache$save(objects, outname, hash, lazy = options$cache.lazy)
}

purge_cache = function(options) {
  # purge my old cache and cache of chunks dependent on me
  cache$purge(paste0(valid_path(
    options$cache.path, c(options$label, dep_list$get(options$label))
  ), '_????????????????????????????????'))
}

# open a device for a chunk; depending on the option global.device, may or may
# not need to close the device on exit
chunk_device = function(
  width, height, record = TRUE, dev, dev.args, dpi, options, tmp = tempfile()
) {
  dev_new = function() {
    # actually I should adjust the recording device according to dev, but here I
    # have only considered the png and tikz devices (because the measurement
    # results can be very different especially with the latter, see #1066), and
    # also the cairo_pdf device (#1235)
    if (identical(dev, 'png')) {
      do.call(grDevices::png, c(list(
        filename = tmp, width = width, height = height, units = 'in', res = dpi
      ), get_dargs(dev.args, 'png')))
    } else if (identical(dev, 'tikz')) {
      dargs = c(list(
        file = tmp, width = width, height = height
      ), get_dargs(dev.args, 'tikz'))
      dargs$sanitize = options$sanitize; dargs$standAlone = options$external
      if (is.null(dargs$verbose)) dargs$verbose = FALSE
      do.call(tikz_dev, dargs)
    } else if (identical(dev, 'cairo_pdf')) {
      do.call(grDevices::cairo_pdf, c(list(
        filename = tmp, width = width, height = height
      ), get_dargs(dev.args, 'cairo_pdf')))
    } else if (identical(getOption('device'), pdf_null)) {
      if (!is.null(dev.args)) {
        dev.args = get_dargs(dev.args, 'pdf')
        dev.args = dev.args[intersect(names(dev.args), c('pointsize', 'bg'))]
      }
      do.call(pdf_null, c(list(width = width, height = height), dev.args))
    } else dev.new(width = width, height = height)
  }
  if (!opts_knit$get('global.device')) {
    dev_new()
    dev.control(displaylist = if (record) 'enable' else 'inhibit')  # enable recording
    # if returns TRUE, we need to close this device after code is evaluated
    return(TRUE)
  } else if (is.null(dev.list())) {
    # want to use a global device but not open yet
    dev_new()
    dev.control('enable')
  }
  FALSE
}

# filter out some results based on the numeric chunk option as indices
filter_evaluate = function(res, opt, test) {
  if (length(res) == 0 || !is.numeric(opt) || !any(idx <- sapply(res, test)))
    return(res)
  idx = which(idx)
  idx = setdiff(idx, na.omit(idx[opt]))  # indices of elements to remove
  if (length(idx) == 0) res else res[-idx]
}

# find recorded plots in the output of evaluate()
find_recordedplot = function(x) {
  vapply(x, is_plot_output, logical(1))
}

is_plot_output = function(x) {
  evaluate::is.recordedplot(x) || inherits(x, 'knit_image_paths')
}

# move plots before source code
fig_before_code = function(x) {
  s = vapply(x, evaluate::is.source, logical(1))
  if (length(s) == 0 || !any(s)) return(x)
  s = which(s)
  f = which(find_recordedplot(x))
  f = f[f >= min(s)]  # only move those plots after the first code block
  for (i in f) {
    j = max(s[s < i])
    tmp = x[i]; x[[i]] = NULL; x = append(x, tmp, j - 1)
    s = which(vapply(x, evaluate::is.source, logical(1)))
  }
  x
}

# merge neighbor elements of the same class in a list returned by evaluate()
merge_class = function(res, class = c('source', 'message', 'warning')) {

  class = match.arg(class)
  idx = if (length(res)) which(sapply(res, inherits, what = class))
  if ((n <- length(idx)) <= 1) return(res)

  k1 = idx[1]; k2 = NULL; res1 = res[[k1]]
  el = c(source = 'src', message = 'message', warning = 'message')[class]
  for (i in 1:(n - 1)) {
    idx2 = idx[i + 1]; idx1 = idx[i]
    if (idx2 - idx1 == 1) {
      res2 = res[[idx2]]
      # merge warnings/messages only if next one is identical to previous one
      if (class == 'source' || identical(res1, res2) ||
          (class == 'message' && !grepl('\n$', tail(res1[[el]], 1)))) {
        res[[k1]][[el]] = c(res[[k1]][[el]], res2[[el]])
        k2 = c(k2, idx2)
      } else {
        k1 = idx2
        res1 = res[[k1]]
      }
    } else k1 = idx2
  }
  if (length(k2)) res = res[-k2] # remove lines that have been merged back
  res

}

# merge character output for output='hold', if the subsequent character is of
# the same class(es) as the previous one (e.g. should not merge normal
# characters with asis_output())
merge_character = function(res) {
  if ((n <- length(res)) <= 1) return(res)
  k = NULL
  for (i in 1:(n - 1)) {
    cls = class(res[[i]])
    if (identical(cls, class(res[[i + 1]]))) {
      res[[i + 1]] = paste0(res[[i]], res[[i + 1]])
      class(res[[i + 1]]) = cls
      k = c(k, i)
    }
  }
  if (length(k)) res = res[-k]
  res
}

call_inline = function(block) {
  if (opts_knit$get('progress')) print(block)
  in_dir(input_dir(), inline_exec(block))
}

inline_exec = function(
  block, envir = knit_global(), hook = knit_hooks$get('inline'),
  hook_eval = knit_hooks$get('evaluate.inline')
) {

  # run inline code and substitute original texts
  code = block$code; input = block$input
  if ((n <- length(code)) == 0) return(input) # untouched if no code is found

  loc = block$location
  for (i in 1:n) {
    res = hook_eval(code[i], envir)
    if (inherits(res, 'knit_asis')) res = wrap(res, inline = TRUE)
    d = nchar(input)
    # replace with evaluated results
    stringr::str_sub(input, loc[i, 1], loc[i, 2]) = if (length(res)) {
      paste(hook(res), collapse = '')
    } else ''
    if (i < n) loc[(i + 1):n, ] = loc[(i + 1):n, ] - (d - nchar(input))
    # may need to move back and forth because replacement may be longer or shorter
  }
  input
}

process_tangle = function(x) {
  UseMethod('process_tangle', x)
}
#' @export
process_tangle.block = function(x) {
  params = opts_chunk$merge(x$params)
  for (o in c('purl', 'eval', 'child'))
    try(params[o] <- list(eval_lang(params[[o]])))
  if (isFALSE(params$purl)) return('')
  label = params$label; ev = params$eval
  if (params$engine != 'R') return(comment_out(knit_code$get(label)))
  code = if (!isFALSE(ev) && !is.null(params$child)) {
    cmds = lapply(sc_split(params$child), knit_child)
    paste(unlist(cmds), collapse = '\n')
  } else knit_code$get(label)
  # read external code if exists
  if (!isFALSE(ev) && length(code) && grepl('read_chunk\\(.+\\)', code)) {
    eval(parse_only(unlist(stringr::str_extract_all(code, 'read_chunk\\(([^)]+)\\)'))))
  }
  code = parse_chunk(code)
  if (isFALSE(ev)) code = comment_out(code, params$comment, newline = FALSE)
  if (opts_knit$get('documentation') == 0L) return(paste(code, collapse = '\n'))
  label_code(code, x$params.src)
}
#' @export
process_tangle.inline = function(x) {

  output = if (opts_knit$get('documentation') == 2L) {
    output = paste(line_prompt(x$input.src, "#' ", "#' "), collapse = '\n')
  } else ''

  code = x$code
  if (length(code) == 0L) return(output)

  if (getOption('knitr.purl.inline', FALSE)) output = c(output, code)

  idx = grepl('knit_child\\(.+\\)', code)
  if (any(idx)) {
    cout = sapply(code[idx], function(z) eval(parse_only(z)))
    output = c(output, cout, '')
  }

  paste(output, collapse = '\n')
}


# add a label [and extra chunk options] to a code chunk
label_code = function(code, label) {
  code = paste(c('', code, ''), collapse = '\n')
  paste0('## ----', stringr::str_pad(label, max(getOption('width') - 11L, 0L), 'right', '-'),
         '----', code)
}

as.source = function(code) {
  list(structure(list(src = code), class = 'source'))
}
