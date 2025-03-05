# hooks that return character values will be inserted into final output
run_hooks = function(before, options, envir = knit_global()) {
  # default and user-defined new hooks
  hooks.n = knit_hooks$get()
  hooks.a = hooks.n[setdiff(names(hooks.n), names(.default.hooks))] # a list of hooks to run
  out = NULL
  nms = intersect(names(options), names(hooks.a))
  if (!before) nms = rev(nms)
  for (i in nms) {
    # run only when option is not NULL, and hook is not NULL
    if (is.null(options[[i]]) || is.null(hook <- hooks.a[[i]])) next
    if (is.character(hook)) hook = get(hook, envir = envir, mode = 'function')
    args = list(before = before, options = options, envir = envir, name = i)
    anms = names(formals(hook))
    if (!('...' %in% anms)) args = args[anms]
    res  = do.call(hook, args, envir = envir)
    if (is.character(res)) out = c(out, res)
  }
  out
}
