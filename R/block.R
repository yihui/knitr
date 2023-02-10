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
  for (o in setdiff(names(params), af)) {
    params[o] = list(eval_lang(params[[o]]))
    # also update original options before being merged with opts_chunk
    if (o %in% names(block$params)) block$params[o] = params[o]
  }

  label = ref.label = params$label
  if (!is.null(params$ref.label)) {
    ref.label = sc_split(params$ref.label)
    # ref.label = I() implies opts.label = ref.label
    if (inherits(params$ref.label, 'AsIs') && is.null(params$opts.label))
      params$opts.label = ref.label
  }
  params[['code']] = get_code(params, label, ref.label)

  # opts.label = TRUE means inheriting chunk options from ref.label
  if (isTRUE(params$opts.label)) params$opts.label = ref.label
  # expand chunk options defined via opts_template and reference chunks
  params2 = NULL
  for (lab in params$opts.label) {
    # referenced chunk options (if any) override template options
    params3 = merge_list(opts_template$get(lab), attr(knit_code$get(lab), 'chunk_opts'))
    params2 = merge_list(params2, params3)
  }
  if (length(params2)) {
    # local options override referenced options
    params2 = merge_list(params2, block$params)
    # then override previously merged opts_chunk options
    params  = merge_list(params, params2)
    # in case any options are not evaluated
    for (o in setdiff(names(params), af)) params[o] = list(eval_lang(params[[o]]))
  }

  # save current chunk options in opts_current
  opts_current$restore(params)

  if (opts_knit$get('progress')) print(block)

  if (!is.null(params$child)) {
    if (!is_blank(params[['code']]) && getOption('knitr.child.warning', TRUE)) warning(
      "The chunk '", params$label, "' has the 'child' option, ",
      "and this code chunk must be empty. Its code will be ignored."
    )
    if (!params$eval) return('')
    cmds = lapply(sc_split(params$child), knit_child, options = block$params)
    out = one_string(unlist(cmds))
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

  params = fix_options(params)  # for compatibility

  # Check cache
  if (params$cache > 0) {
    content = c(
      params[if (params$cache < 3) cache1.opts else setdiff(names(params), cache0.opts)],
      75L, if (params$cache == 2) params[cache2.opts]
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

# if chunk option 'file' is provided, read the file(s) as the chunk body;
# otherwise if 'code' is provided, use it; if neither 'file' nor 'code' is
# provided, use the chunk body
get_code = function(params, label, ref.label) {
  code = params[['code']]; file = params[['file']]
  if (is.null(code) && is.null(file))
    return(unlist(knit_code$get(ref.label), use.names = FALSE))
  if (!is.null(file)) code = in_input_dir(xfun::read_all(file))
  set_code(label, code)
  code
}

# replace code in knit_code but preserve attributes
set_code = function(label, code) {
  res = knit_code$get(label)
  attributes(code) = attributes(res)
  knit_code$set(setNames(list(code), label))
}

# options that should affect cache when cache level = 1,2
cache1.opts = c('code', 'eval', 'cache', 'cache.path', 'cache.globals', 'message', 'warning', 'error')
# more options affecting cache level 2
cache2.opts = c('fig.keep', 'fig.path', 'fig.ext', 'dev', 'dpi', 'dev.args', 'fig.width', 'fig.height')
# options that should not affect cache
cache0.opts = c('include', 'out.width.px', 'out.height.px', 'cache.rebuild')

block_exec = function(options) {
  if (options$engine == 'R') return(eng_r(options))

  # when code is not R language
  res.before = run_hooks(before = TRUE, options)
  engine = get_engine(options$engine)
  output = in_input_dir(engine(options))
  if (is.list(output)) output = unlist(output)
  res.after = run_hooks(before = FALSE, options)
  output = paste(c(res.before, output, res.after), collapse = '')
  output = knit_hooks$get('chunk')(output, options)
  if (options$cache) {
    cache.exists = cache$exists(options$hash, options$cache.lazy)
    if (options$cache.rebuild || !cache.exists) block_cache(options, output, switch(
      options$engine,
      'stan' = options$output.var, 'sql' = options$output.var, character(0)
    ))
  }
  if (options$include) output else ''
}

#' Engine for R
#'
#' This function handles the execution of R code blocks (when the chunk option
#' \code{engine} is \code{'R'}) and generates the R output for each code block.
#'
#' This engine function has one argument \code{options}: the source code of the
#' current chunk is in \code{options$code}. It returns a processed output that
#' can consist of data frames (as tables), graphs, or character output. This
#' function is intended for advanced use to allow developers to extend R, and
#' customize the pipeline with which R code is executed and processed within
#' knitr.
#'
#' @param options A list of chunk options. Usually this is just the object
#'   \code{options} associated with the current code chunk.
#' @noRd
eng_r = function(options) {
  # eval chunks (in an empty envir if cache)
  env = knit_global()
  obj.before = ls(globalenv(), all.names = TRUE)  # global objects before chunk

  keep = options$fig.keep
  keep.idx = NULL
  if (is.logical(keep)) keep = which(keep)
  if (is.numeric(keep)) {
    keep.idx = keep
    keep = "index"
  }

  if (keep.pars <- opts_knit$get('global.par')) on.exit({
    opts_knit$set(global.pars = par(no.readonly = TRUE))
  }, add = TRUE)

  tmp.fig = tempfile(); on.exit(unlink(tmp.fig), add = TRUE)
  # open a device to record plots if not using a global device or no device is
  # open, and close this device if we don't want to use a global device
  if (!opts_knit$get('global.device') || is.null(dev.list())) {
    # reset current device if any is open (#2166)
    if (!is.null(dev.list())) {
      dv0 = dev.cur(); on.exit(dev.set(dv0), add = TRUE)
    }
    chunk_device(options, keep != 'none', tmp.fig)
    dv = dev.cur()
    if (!opts_knit$get('global.device')) on.exit(dev.off(dv), add = TRUE)
    showtext(options)  # showtext support
  }
  # preserve par() settings from the last code chunk
  if (keep.pars) par2(opts_knit$get('global.pars'))

  res.before = run_hooks(before = TRUE, options, env) # run 'before' hooks

  code = options$code
  echo = options$echo  # tidy code if echo
  if (!isFALSE(echo) && !isFALSE(options$tidy) && length(code)) {
    tidy.method = if (isTRUE(options$tidy)) 'formatR' else options$tidy
    if (is.character(tidy.method)) tidy.method = switch(
      tidy.method,
      formatR = function(code, ...) {
        if (!loadable('formatR')) stop2(
          'The formatR package is required by the chunk option tidy = TRUE but ',
          'not installed; tidy = TRUE will be ignored.'
        )
        formatR::tidy_source(text = code, output = FALSE, ...)$text.tidy
      },
      styler = function(code, ...) unclass(styler::style_text(text = code, ...))
    )
    res = try_silent(do.call(tidy.method, c(list(code), options$tidy.opts)))

    if (!inherits(res, 'try-error')) code = res else warning(
      "Failed to tidy R code in chunk '", options$label, "'. Reason:\n", res
    )
  }
  # only evaluate certain lines
  if (is.numeric(ev <- options$eval)) {
    # group source code into syntactically complete expressions
    if (isFALSE(options$tidy)) code = sapply(xfun::split_source(code), one_string)
    iss = seq_along(code)
    code = comment_out(code, '##', setdiff(iss, iss[ev]), newline = FALSE)
  }
  # guess plot file type if it is NULL
  if (keep != 'none') options$fig.ext = dev2ext(options)

  cache.exists = cache$exists(options$hash, options$cache.lazy)
  evaluate = knit_hooks$get('evaluate')
  # return code with class 'source' if not eval chunks
  res = if (is_blank(code)) list() else if (isFALSE(ev)) {
    as.source(code)
  } else if (cache.exists && isFALSE(options$cache.rebuild)) {
    fix_evaluate(cache$output(options$hash, 'list'), options$cache == 1)
  } else in_input_dir(
    evaluate(
      code, envir = env, new_device = FALSE,
      keep_warning = if (is.numeric(options$warning)) TRUE else options$warning,
      keep_message = if (is.numeric(options$message)) TRUE else options$message,
      stop_on_error = if (is.numeric(options$error)) options$error else {
        if (options$error && options$include) 0L else 2L
      },
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
  res = rearrange_figs(res, keep, keep.idx, options$fig.show)

  # number of plots in this chunk
  if (is.null(options$fig.num))
    options$fig.num = if (length(res)) sum(sapply(res, function(x) {
      if (inherits(x, 'knit_image_paths')) return(length(x))
      if (is_plot_output(x)) return(1)
      0
    })) else 0L

  # merge neighbor elements of the same class into one element
  for (cls in c('source', 'message', 'warning')) res = merge_class(res, cls)

  if (isTRUE(options$fig.beforecode)) res = fig_before_code(res)

  on.exit({
    plot_counter(reset = TRUE)
    shot_counter(reset = TRUE)
    opts_knit$delete('plot_files')
  }, add = TRUE)  # restore plot number

  sewn = sew(res, options)

  # The markdown chunk hook messes up htmlwidgets output when it collapses a
  # chunk, so wrap it in special markers that won't be matched by the chunk hook
  # regexp.

  if (isTRUE(options$collapse))
    specials = vapply(sewn, function(s) {
        !identical(class(s), 'character') ||
        grepl("^\n(.)\\1{2,}\\{=html\\}\n", s[1])
      }, FALSE)
  else
    specials = FALSE

  if (any(specials)) {
    specialSeparator = paste('KNITR SPECIAL OBJECT', format(Sys.time(), '%OS6'))
    for (i in which(specials))
      sewn[[i]] = c(specialSeparator, sewn[[i]],
                    specialSeparator)
  } else
    specialSeparator = ''

  output = unlist(sewn) # wrap all results together
  res.after = run_hooks(before = FALSE, options, env) # run 'after' hooks

  output = paste(c(res.before, output, res.after), collapse = '')  # insert hook results
  output = knit_hooks$get('chunk')(output, options)

  if (any(specials))
    output = gsub(specialSeparator, '', output, fixed = TRUE)

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
        objs, cache_globals(options$cache.globals, code), options$label,
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

cache_globals = function(option, code) {
  if (is.character(option)) option else {
    (if (isFALSE(option)) find_symbols else find_globals)(code)
  }
}

# open a graphical device for a chunk to record plots
chunk_device = function(options, record = TRUE, tmp = tempfile()) {
  width = options$fig.width[1L]
  height = options$fig.height[1L]
  dev = fallback_dev(options$dev)
  dev.args = options$dev.args
  dpi = options$dpi

  # actually I should adjust the recording device according to dev, but here I
  # have only considered devices like png and tikz (because the measurement
  # results can be very different especially with the latter, see #1066), the
  # cairo_pdf device (#1235), and svg (#1705)
  if (identical(dev, 'png')) {
    do.call(grDevices::png, c(list(
      filename = tmp, width = width, height = height, units = 'in', res = dpi
    ), get_dargs(dev.args, 'png')))
  } else if (identical(dev, 'ragg_png')) {
    do.call(ragg_png_dev, c(list(
      filename = tmp, width = width, height = height, units = 'in', res = dpi
    ), get_dargs(dev.args, 'ragg_png')))
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
  } else if (identical(dev, 'svg')) {
    do.call(grDevices::svg, c(list(
      filename = tmp, width = width, height = height
    ), get_dargs(dev.args, 'svg')))
  } else if (identical(getOption('device'), pdf_null)) {
    if (!is.null(dev.args)) {
      dev.args = get_dargs(dev.args, 'pdf')
      dev.args = dev.args[intersect(names(dev.args), c('pointsize', 'bg'))]
    }
    do.call(pdf_null, c(list(width = width, height = height), dev.args))
  } else dev.new(width = width, height = height)
  dev.control(displaylist = if (record) 'enable' else 'inhibit')
}

# fall back to a usable device (e.g., during R CMD check)
fallback_dev = function(dev) {
  if (length(dev) != 1 || !getOption('knitr.device.fallback', is_R_CMD_check()))
    return(dev)
  choices = list(
    svg = c('png', 'jpeg', 'bmp'), cairo_pdf = c('pdf'), cairo_ps = c('postscript'),
    png = c('jpeg', 'svg', 'bmp'), jpeg = c('png', 'svg', 'bmp')
  )
  # add choices provided by users
  choices = merge_list(choices, getOption('knitr.device.choices'))
  if (!dev %in% names(choices)) return(dev)  # no fallback devices available
  # first test if the specified device actually works
  if (dev_available(dev)) return(dev)
  for (d in choices[[dev]]) if (dev_available(d)) {
    warning2("The device '", dev, "' is not operational; falling back to '", d, "'.")
    return(d)
  }
  dev  # no fallback device found; you'll to run into an error soon
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
  evaluate::is.recordedplot(x) ||
    inherits(x, c('knit_image_paths', 'html_screenshot', 'knit_other_plot'))
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

rearrange_figs = function(res, keep, idx, show) {
  figs = find_recordedplot(res)
  if (!any(figs)) return(res) # no figures
  if (keep == 'none') return(res[!figs]) # remove all

  if (show == 'hold') {
    res = c(res[!figs], res[figs]) # move to the end
    figs = find_recordedplot(res)
  }
  if (sum(figs) <= 1) return(res) # return early if only 1 figure to keep
  switch(
    keep,
    first = res[-tail(which(figs), -1L)],
    last  = res[-head(which(figs), -1L)],
    high  = merge_low_plot(res, figs),  # merge low-level plotting changes
    index = {
      i = which(figs)[-idx]
      if (length(i) > 0) res[-i] else res  # keep only selected
    },
    res
  )
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
  in_input_dir(inline_exec(block))
}

inline_exec = function(
  block, envir = knit_global(), hook = knit_hooks$get('inline'),
  hook_eval = knit_hooks$get('evaluate.inline')
) {

  # run inline code and substitute original texts
  code = block$code; input = block$input
  if ((n <- length(code)) == 0) return(input) # untouched if no code is found
  code.src = block$code.src

  ans = character(n)
  for (i in 1:n) {
    tryCatch(parse_only(code[i]), error = function(e) {
      stop2('Failed to parse the inline R code: ', code.src[i], '\nReason: ', e$message)
    })
    res = hook_eval(code[i], envir)
    if (inherits(res, c('knit_asis', 'knit_asis_url'))) res = sew(res, inline = TRUE)
    tryCatch(as.character(res), error = function(e) {
      stop2("The inline value cannot be coerced to character: ", code[i])
    })
    if (length(res)) ans[i] = paste(hook(res), collapse = '')
  }
  # replace with evaluated results
  str_replace(input, block$location, ans)
}

process_tangle = function(x) {
  UseMethod('process_tangle', x)
}
#' @export
process_tangle.block = function(x) {
  params = opts_chunk$merge(x$params)
  for (o in c('purl', 'eval', 'child')) {
    if (inherits(try(params[o] <- list(eval_lang(params[[o]]))), 'try-error')) {
      params[['purl']] = FALSE  # if any of these options cannot be determined, don't purl
    }
  }
  if (isFALSE(params$purl)) return('')
  label = params$label; ev = params$eval
  if (params$engine != 'R') return(one_string(comment_out(knit_code$get(label))))
  code = if (!isFALSE(ev) && !is.null(params$child)) {
    cmds = lapply(sc_split(params$child), knit_child)
    one_string(unlist(cmds))
  } else knit_code$get(label)
  # read external code if exists
  if (!isFALSE(ev) && length(code) && any(grepl('read_chunk\\(.+\\)', code))) {
    eval(parse_only(unlist(str_extract(code, 'read_chunk\\(([^)]+)\\)'))))
  }
  code = parse_chunk(code)
  if (isFALSE(ev)) code = comment_out(code, params$comment, newline = FALSE)
  if (opts_knit$get('documentation') == 0L) return(one_string(code))
  # e.g when documentation 1 or 2 with purl()
  label_code(code, x)
}
#' @export
process_tangle.inline = function(x) {

  output = if (opts_knit$get('documentation') == 2L) {
    output = paste("#'", gsub('\n', "\n#' ", x$input, fixed = TRUE))
  } else ''

  code = x$code
  if (length(code) == 0L) return(output)

  if (getOption('knitr.purl.inline', FALSE)) output = c(output, code)

  idx = grepl('knit_child\\(.+\\)', code)
  if (any(idx)) {
    cout = sapply(code[idx], function(z) eval(parse_only(z)))
    output = c(output, cout, '')
  }

  one_string(output)
}


# add a label [and extra chunk options] to a code chunk
label_code = function(code, options) {
  code = one_string(c('', code, ''))
  comments = if (is_quarto()) one_string(options$params$yaml.code) else paste0(
    '## ----', options$params.src,
    strrep('-', max(getOption('width') - 11L - nchar(options$params.src), 0L)),
    '----'
  )
  paste0(comments, code)
}

as.source = function(code) {
  list(structure(list(src = code), class = 'source'))
}
