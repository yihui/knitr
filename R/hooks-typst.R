#' @rdname hook_plot
#' @export
hook_plot_typst = function(x, options) {
  if (options$fig.show == 'animate') return(hook_plot_html(x, options))

  cap = .img.cap(options)

  w = options[['out.width']]
  h = options[['out.height']]
  s = options[['out.extra']]

  # build named arguments for #image()
  args = c(
    if (!is.null(w)) sprintf('width: %s', w),
    if (!is.null(h)) sprintf('height: %s', h),
    s
  )
  img_path = paste0(opts_knit$get('base.url'), .upload.url(x))
  img_args = paste(c(sprintf('"%s"', img_path), args), collapse = ', ')

  if (nzchar(cap)) {
    sprintf('\n#figure(\n  image(%s),\n  caption: [%s],\n)\n', img_args, cap)
  } else {
    sprintf('\n#image(%s)\n', img_args)
  }
}

#' @rdname output_hooks
#' @export
render_typst = function() {
  opts_chunk$set(dev = 'pdf')
  opts_knit$set(out.format = 'typst')
  knit_hooks$set(hooks_typst())
}

#' @rdname output_hooks
#' @export
hooks_typst = function() {
  hook.source = function(x, options) {
    x = one_string(c(hilight_source(x, 'markdown', options), ''))
    lang = tolower(options$lang %n% eng2lang(options$engine))
    sprintf('\n```%s\n%s```\n', lang, x)
  }
  hook.text = function(x, options) {
    sprintf('\n```\n%s```\n', x)
  }
  list(
    source = hook.source,
    output = function(x, options) {
      if (output_asis(x, options)) return(x)
      hook.text(x, options)
    },
    warning = hook.text,
    message = hook.text,
    error = hook.text,
    plot = hook_plot_typst,
    inline = function(x) .inline.hook(format_sci(x, 'typst'))
  )
}
