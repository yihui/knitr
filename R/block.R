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
  params = opts_chunk$merge(block$params)
  opts_current$restore(params)
  for (o in setdiff(names(params), af)) params[o] = list(eval_lang(params[[o]]))

  params = fix_options(params)  # for compatibility

  # expand parameters defined via template
  if (!is.null(params$opts.label))
    params = merge_list(params, opts_template$get(params$opts.label))

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

  # Check cache
  if (params$cache > 0) {
    content = c(
      params[if (params$cache < 3) cache1.opts else setdiff(names(params), cache0.opts)],
      getOption('width'), if (params$cache == 2) params[cache2.opts]
    )
    if (params$engine == 'R' && isFALSE(params$cache.comments)) {
      content[['code']] = formatR:::parse_only(content[['code']])
    }
    hash = paste(valid_path(params$cache.path, label), digest::digest(content), sep = '_')
    params$hash = hash
    if (cache$exists(hash, params$cache.lazy) && isFALSE(params$cache.rebuild)) {
      if (opts_knit$get('verbose')) message('  loading cache from ', hash)
      cache$load(hash, lazy = params$cache.lazy)
      if (!params$include) return('')
      if (params$cache == 3) return(cache$output(hash))
    }
    if (params$engine == 'R')
      cache$library(params$cache.path, save = FALSE) # load packages
  } else if (label %in% names(dep_list$get()) && !isFALSE(opts_knit$get('warn.uncached.dep')))
    warning('code chunks must not depend on the uncached chunk "', label, '"',
            call. = FALSE)

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
cache2.opts = c('fig.keep', 'fig.path', 'fix.ext', 'dev', 'dpi', 'dev.args', 'fig.width', 'fig.height')
# options that should not affect cache
cache0.opts = c('include', 'out.width.px', 'out.height.px', 'cache.rebuild')

block_exec = function(options) {
  # when code is not R language
  if (options$engine != 'R') {
    res.before = run_hooks(before = TRUE, options)
    engine = get_engine(options$engine)
    output = in_dir(opts_knit$get('root.dir') %n% input_dir(), engine(options))
    res.after = run_hooks(before = FALSE, options)
    output = paste(c(res.before, output, res.after), collapse = '')
    output = if (is_blank(output)) '' else knit_hooks$get('chunk')(output, options)
    if (options$cache) block_cache(options, output, character(0))
    return(if (options$include) output else '')
  }

  # eval chunks (in an empty envir if cache)
  env = knit_global()
  obj.before = ls(globalenv(), all.names = TRUE)  # global objects before chunk

  keep = options$fig.keep
  # open a device to record plots
  if (chunk_device(options$fig.width[1L], options$fig.height[1L], keep != 'none',
                   options$dev, options$dev.args, options$dpi)) {
    # preserve par() settings from the last code chunk
    if (keep.pars <- opts_knit$get('global.par'))
      par(opts_knit$get('global.pars'))
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
      code = native_encode(res$text.tidy)
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

  if (!is.null(err.code <- opts_knit$get('stop_on_error'))) {
    warning('the package option stop_on_error was deprecated;',
            ' use the chunk option error = ', err.code != 2L, ' instead')
    options$error = err.code != 2L
  }
  cache.exists = cache$exists(options$hash, options$cache.lazy)
  # return code with class 'source' if not eval chunks
  res = if (is_blank(code)) list() else if (isFALSE(ev)) {
    as.source(code)
  } else if (cache.exists && isFALSE(options$cache.rebuild)) {
    fix_evaluate(cache$output(options$hash, 'list'), options$cache == 1)
  } else in_dir(
    opts_knit$get('root.dir') %n% input_dir(),
    evaluate::evaluate(
      code, envir = env, new_device = FALSE,
      keep_warning = !isFALSE(options$warning),
      keep_message = !isFALSE(options$message),
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
    if (any(i)) res = c(res[!i], list(paste(unlist(res[i]), collapse = '')))
  }
  res = filter_evaluate(res, options$warning, evaluate::is.warning)
  res = filter_evaluate(res, options$message, evaluate::is.message)

  # rearrange locations of figures
  figs = vapply(res, evaluate::is.recordedplot, logical(1))
  if (length(figs) && any(figs)) {
    if (keep == 'none') {
      res = res[!figs] # remove all
    } else {
      if (options$fig.show == 'hold') res = c(res[!figs], res[figs]) # move to the end
      figs = sapply(res, evaluate::is.recordedplot)
      if (length(figs) && sum(figs) > 1) {
        if (keep %in% c('first', 'last')) {
          res = res[-(if (keep == 'last') head else tail)(which(figs), -1L)]
        } else {
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
  for (cls in c('source', 'message')) res = merge_class(res, cls)

  on.exit(plot_counter(reset = TRUE), add = TRUE)  # restore plot number
  if (options$fig.show != 'animate' && options$fig.num > 1) {
    options = recycle_plot_opts(options)
  }

  output = unlist(wrap(res, options)) # wrap all results together
  res.after = run_hooks(before = FALSE, options, env) # run 'after' hooks

  output = paste(c(res.before, output, res.after), collapse = '')  # insert hook results
  output = if (is_blank(output)) '' else knit_hooks$get('chunk')(output, options)

  if (options$cache > 0) {
    obj.new = setdiff(ls(globalenv(), all.names = TRUE), obj.before)
    copy_env(globalenv(), env, obj.new)
    objs = if (isFALSE(ev) || length(code) == 0) character(0) else
      options$cache.vars %n% codetools::findLocalsList(parse_only(code))
    # make sure all objects to be saved exist in env
    objs = intersect(c(objs, obj.new), ls(env, all.names = TRUE))
    if (options$autodep) {
      cache$objects(objs, code, options$label, options$cache.path)
      dep_auto()
    }
    if (options$cache < 3) {
      if (!cache.exists) block_cache(options, res.orig, objs)
    } else block_cache(options, output, objs)
  }

  if (options$include) output else ''
}

block_cache = function(options, output, objects) {
  hash = options$hash
  outname = sprintf('.%s', hash)
  assign(outname, output, envir = knit_global())
  purge_cache(options)
  cache$library(options$cache.path, save = TRUE)
  cache$save(objects, outname, hash, lazy = options$cache.lazy)
}

purge_cache = function(options) {
  # purge my old cache and cache of chunks dependent on me
  cache$purge(paste(valid_path(
    options$cache.path, c(options$label, dep_list$get(options$label))
  ), '_????????????????????????????????', sep = ''))
}

# open a device for a chunk; depending on the option global.device, may or may
# not need to close the device on exit
chunk_device = function(width, height, record = TRUE, dev, dev.args, dpi) {
  dev_new = function() {
    # actually I should adjust the recording device according to dev, but here
    # I have only considered the png device
    if (identical(dev, 'png')) {
      do.call(grDevices::png, c(list(
        filename = tempfile(), width = width, height = height, units = 'in', res = dpi
      ), get_dargs(dev.args, 'png')))
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

# merge neighbor elements of the same class in a list returned by evaluate()
merge_class = function(res, class) {

  idx = if (length(res)) which(sapply(res, inherits, what = class))
  if ((n <- length(idx)) <= 1) return(res)

  k1 = idx[1]; k2 = NULL
  el = switch(
    class, `source` = 'src', `message` = 'message',
    stop("`class` must be either 'source' or 'message'")
  )
  for (i in 1:(n - 1)) {
    if (idx[i + 1] - idx[i] == 1) {
      res[[k1]] = structure(
        list(c(res[[k1]][[el]], res[[idx[i + 1]]][[el]])),
        class = class, .Names = el
      )
      k2 = c(k2, idx[i + 1])
    } else k1 = idx[i + 1]
  }
  if (length(k2)) res = res[-k2] # remove lines that have been merged back
  res

}

call_inline = function(block) {
  if (opts_knit$get('progress')) print(block)
  in_dir(opts_knit$get('root.dir') %n% input_dir(), inline_exec(block))
}

inline_exec = function(
  block, eval = eval_lang(opts_chunk$get('eval')), envir = knit_global(),
  hook = knit_hooks$get('inline')
) {

  # run inline code and substitute original texts
  code = block$code; input = block$input
  if ((n <- length(code)) == 0) return(input) # untouched if no code is found

  loc = block$location
  for (i in 1:n) {
    res = if (eval) {
      v = withVisible(eval(parse_only(code[i]), envir = envir))
      if (v$visible) knit_print(v$value, inline = TRUE, options = opts_chunk$get())
    } else '??'
    if (inherits(res, c('knit_asis', 'knit_asis_list'))) res = wrap(res, inline = TRUE)
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
  if (opts_knit$get('documentation') == 2L) {
    return(paste(line_prompt(x$input.src, "#' ", "#' "), collapse = '\n'))
  }
  code = x$code
  if (length(code) == 0L || !any(idx <- grepl('knit_child\\(.+\\)', code)))
    return('')
  paste(c(sapply(code[idx], function(z) eval(parse_only(z))), ''), collapse = '\n')
}


# add a label [and extra chunk options] to a code chunk
label_code = function(code, label) {
  code = paste(c('', code, ''), collapse = '\n')
  paste('## ----', stringr::str_pad(label, max(getOption('width') - 11L, 0L), 'right', '-'),
        '----', code, sep = '')
}

as.source <- function(code) {
  list(structure(list(src = code), class = 'source'))
}

