# S3 method to deal with chunks and inline text respectively
#' @S3method process_group block
#' @S3method process_group inline
process_group = function(x) {
  UseMethod('process_group', x)
}
process_group.block = function(x) call_block(x)
process_group.inline = function(x) call_inline(x)


call_block = function(block) {
  # now try eval all options except those in eval.after and their aliases
  af = opts_knit$get('eval.after'); al = opts_knit$get('aliases')
  if (!is.null(al) && !is.null(af)) af = c(af, names(al[af %in% al]))
  params = opts_chunk$merge(block$params)
  for (o in setdiff(names(params), af)) params[o] = list(eval_lang(params[[o]]))

  params = fix_options(params)  # for compatibility

  # expand parameters defined via template
  if(!is.null(params$opts.label)) params = merge_list(params, opts_template$get(params$opts.label))

  label = ref.label = params$label
  if (!is.null(params$ref.label)) ref.label = sc_split(params$ref.label)
  params$code = unlist(knit_code$get(ref.label), use.names = FALSE)
  if (opts_knit$get('progress')) print(block)

  if (!is.null(params$child)) {
    if (!params$eval) return('')
    if (concord_mode()) {
      concord_gen()  # generate a partial concordance before knit children
      i = knit_concord$get('i'); olines = knit_concord$get('outlines')
      knit_concord$set(parent.line = current_lines(i)[1L])
    }
    cmds = lapply(sc_split(params$child), knit_child)
    out = str_c(unlist(cmds), collapse = '\n')
    if (concord_mode()) {
      knit_concord$set(out.next = sum(olines) + line_count(out) - 1L,
                       in.next = i + 1L)
    }
    return(out)
  }

  params$code = parse_chunk(params$code) # parse sub-chunk references

  # Check cache
  if (params$cache) {
    content = list(params[setdiff(names(params), 'include')], getOption('width'))
    hash = str_c(valid_path(params$cache.path, label), '_', digest(content))
    params$hash = hash
    if (cache$exists(hash)) {
      if (opts_knit$get('verbose')) message('  loading cache from ', hash)
      cache$load(hash)
      if (!params$include) return('')
      return(cache$output(hash))
    }
    cache$library(params$cache.path, save = FALSE) # load packages
  } else if (label %in% names(dep_list$get()))
    warning('code chunks must not depend on the uncached chunk "', label, '"',
            call. = FALSE)

  params$params.src = block$params.src
  opts_current$restore(params)  # save current options

  block_exec(params)
}

block_exec = function(options) {
  # when code is not R language
  if (options$engine != 'R') {
    output = knit_engines$get(options$engine)(options)
    if (options$cache) block_cache(options, output, character(0))
    return(output)
  }

  # eval chunks (in an empty envir if cache)
  env = if (options$cache) new.env(parent = knit_global()) else knit_global()
  .knitEnv$knit_env = env # make a copy of the envir
  obj.before = ls(globalenv(), all.names = TRUE)  # global objects before chunk

  keep = options$fig.keep
  # open a device to record plots
  if (chunk_device(options$fig.width, options$fig.height, keep != 'none')) {
    dv = dev.cur(); on.exit(dev.off(dv))
  }

  res.before = run_hooks(before = TRUE, options, env) # run 'before' hooks

  code = options$code
  echo = options$echo  # tidy code if echo
  if (!isFALSE(echo) && options$tidy && length(code)) {
    res = try(do.call(
      tidy.source, c(list(text = code, output = FALSE), options$tidy.opts)
    ), silent = TRUE)
    if (!inherits(res, 'try-error')) {
      code = native_encode(res$text.tidy)
    } else warning('failed to tidy R code in chunk <', options$label, '>\n',
                   'reason: ', res)
  }
  # only evaluate certain lines
  if (is.numeric(ev <- options$eval)) {
    iss = seq_along(code)
    code = comment_out(code, '##', setdiff(iss, iss[ev]), newline = FALSE)
  }
  # guess plot file type if it is NULL
  if (keep != 'none' && is.null(options$fig.ext))
    options$fig.ext = dev2ext(options$dev)

  # return code with class 'source' if not eval chunks
  res = if (is_blank(code)) list() else if (isFALSE(ev)) {
    list(structure(list(src = code), class = 'source'))
  } else in_dir(
    opts_knit$get('root.dir') %n% input_dir(),
    evaluate(code, envir = env, new_device = FALSE,
             stop_on_error = if (options$include) opts_knit$get('stop_on_error') else 2L)
  )

  # eval other options after the chunk
  for (o in opts_knit$get('eval.after')) options[[o]] = eval_lang(options[[o]], env)

  # remove some components according options
  if (isFALSE(echo)) {
    res = Filter(Negate(is.source), res)
  } else if (is.numeric(echo)) {
    # choose expressions to echo using a numeric vector
    if (isFALSE(ev)) {
      res = list(structure(list(src = code[echo]), class = 'source'))
    } else {
      iss = which(sapply(res, is.source))
      if (length(idx <- setdiff(iss, iss[echo]))) res = res[-idx]
    }
  }
  if (options$results == 'hide') res = Filter(Negate(is.character), res)
  if (!options$warning) res = Filter(Negate(is.warning), res)
  if (!options$error) res = Filter(Negate(is.error), res)
  if (!options$message) res = Filter(Negate(is.message), res)

  # rearrange locations of figures
  figs = sapply(res, is.recordedplot)
  if (length(figs) && any(figs)) {
    if (keep == 'none') {
      res = res[!figs] # remove all
    } else {
      if (options$fig.show == 'hold') res = c(res[!figs], res[figs]) # move to the end
      res = rm_blank_plot(res)
      figs = sapply(res, is.recordedplot)
      if (sum(figs) > 1) {
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
    options$fig.num = if (length(res)) sum(sapply(res, is.recordedplot)) else 0L

  # merge source lines if they do not have output; is there an elegant way??
  iss = if (length(res)) which(sapply(res, is.source)) else NULL
  if ((n <- length(iss)) > 1) {
    k1 = iss[1]; k2 = NULL
    for (i in 1:(n - 1)) {
      if (iss[i + 1] - iss[i] == 1) {
        res[[k1]] = structure(list(src = c(res[[k1]]$src, res[[iss[i + 1]]]$src)),
                              class = 'source')  # CAUTION: now node src is a vector!!
        k2 = c(k2, iss[i + 1])
      } else k1 = iss[i + 1]
    }
    if (length(k2)) res = res[-k2] # remove lines that have been merged back
  }

  on.exit(plot_counter(reset = TRUE), add = TRUE)  # restore plot number
  if (options$fig.show != 'animate' && options$fig.num > 1) {
    options = recycle_plot_opts(options)
  }
  in_dir(opts_knit$get('base.dir'), {
    output = unlist(wrap(res, options)) # wrap all results together
    res.after = run_hooks(before = FALSE, options, env) # run 'after' hooks
  })
  if (options$cache) copy_env(env, knit_global())

  output = str_c(c(res.before, output, res.after), collapse = '')  # insert hook results
  output = if (is_blank(output)) '' else knit_hooks$get('chunk')(output, options)

  if (options$cache) {
    obj.after = ls(globalenv(), all.names = TRUE)  # figure out new global objs
    copy_env(globalenv(), knit_global(), setdiff(obj.after, obj.before))
    objs = ls(env, all.names = TRUE)
    block_cache(options, output, objs)
    if (options$autodep) cache$objects(objs, code, options$label, options$cache.path)
  }

  if (options$include) output else ''
}

block_cache = function(options, output, objects) {
  hash = options$hash
  outname = str_c('.', hash)
  assign(outname, output, envir = knit_global())
  # purge my old cache and cache of chunks dependent on me
  cache$purge(str_c(valid_path(options$cache.path,
                               c(options$label, dep_list$get(options$label))), '_*'))
  cache$library(options$cache.path, save = TRUE)
  cache$save(objects, outname, hash)
}

# open a device for a chunk; depending on the option global.device, may or may
# not need to close the device on exit
chunk_device = function(width, height, record = TRUE) {
  if (!opts_knit$get('global.device')) {
    dev.new(width = width, height = height)
    dev.control(displaylist = if (record) 'enable' else 'inhibit')  # enable recording
    # if returns TRUE, we need to close this device after code is evaluated
    return(TRUE)
  } else if (is.null(dev.list())) {
    # want to use a global device but not open yet
    dev.new(width = width, height = height)
    dev.control('enable')
  }
  FALSE
}

call_inline = function(block) {
  if (opts_knit$get('progress')) print(block)
  in_dir(opts_knit$get('root.dir') %n% input_dir(), inline_exec(block))
}

inline_exec = function(block, eval = opts_chunk$get('eval'), envir = knit_global(),
                       stop_on_error = opts_knit$get('stop_on_error'),
                       hook = knit_hooks$get('inline')) {

  # run inline code and substitute original texts
  code = block$code; input = block$input
  if ((n <- length(code)) == 0) return(input) # untouched if no code is found

  loc = block$location
  for (i in 1:n) {
    res = if (eval) {
      (if (stop_on_error == 2L) identity else try)(
        {
          v = withVisible(eval(parse(text = code[i], srcfile = NULL), envir = envir))
          if (v$visible) v$value
        }
      )
    } else '??'
    d = nchar(input)
    # replace with evaluated results
    str_sub(input, loc[i, 1], loc[i, 2]) = if (length(res)) {
      if (inherits(res, 'try-error')) {
        knit_hooks$get('error')(str_c('\n', res, '\n'), opts_chunk$get())
      } else hook(res)
    } else ''
    if (i < n) loc[(i + 1):n, ] = loc[(i + 1):n, ] - (d - nchar(input))
    # may need to move back and forth because replacement may be longer or shorter
  }
  input
}

#' @S3method process_tangle block
#' @S3method process_tangle inline
process_tangle = function(x) {
  UseMethod('process_tangle', x)
}
process_tangle.block = function(x) {
  params = opts_chunk$merge(x$params)
  label = params$label; ev = params$eval
  code = if (!isFALSE(ev) && !is.null(params$child)) {
    cmds = lapply(sc_split(params$child), knit_child)
    str_c(unlist(cmds), collapse = '\n')
  } else knit_code$get(label)
  # read external code if exists
  if (!isFALSE(ev) && length(code) && str_detect(code, 'read_chunk\\(.+\\)')) {
    eval(parse(text = unlist(str_extract_all(code, 'read_chunk\\(([^)]+)\\)'))))
  }
  code = parse_chunk(code)
  if (isFALSE(ev)) code = comment_out(code, params$comment, newline = FALSE)
  label_code(code, x$params.src)
}
process_tangle.inline = function(x) {
  if (opts_knit$get('documentation') == 2L) {
    return(str_c(line_prompt(x$input.src, "#' ", "#' "), collapse = '\n'))
  }
  code = x$code
  if (length(code) == 0L || !any(idx <- str_detect(code, "knit_child\\(.+\\)")))
    return('')
  str_c(str_c(sapply(code[idx], function(z) eval(parse(text = z))),
              collapse = '\n'), '\n')
}


# add a label [and extra chunk options] to a code chunk
label_code = function(code, label) {
  code = str_c(c('', code, ''), collapse = '\n')
  if (opts_knit$get('documentation') == 0L) return(code)
  str_c('## @knitr ', label, code)
}
