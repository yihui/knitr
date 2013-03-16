## hooks that return character values will be inserted into final output
run_hooks = function(before, options, envir = knit_global()) {
  ## default and user-defined new hooks
  hooks.n = knit_hooks$get()
  hooks.a = hooks.n[setdiff(names(hooks.n), names(.default.hooks))] # a list of hooks to run
  out = NULL
  nms = intersect(names(options), names(hooks.a))
  if (!before) nms = rev(nms)
  for (i in nms) {
    if (!is.null(options[[i]])) {
      ## run only when option is not NULL
      res = hooks.a[[i]](before = before, options = options, envir = envir)
      if (is.character(res)) out = c(out, res)
    }
  }
  out
}
