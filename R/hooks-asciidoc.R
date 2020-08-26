#' @rdname hook_plot
#' @export
hook_plot_asciidoc = function(x, options) {
  base = opts_knit$get('base.url') %n% ''
  cap = .img.cap(options)

  width = sprintf('width=%s', options$out.width)
  height = sprintf('height=%s', options$out.height)
  align = sprintf('align=%s', options$fig.align)
  tags = paste(c(cap, width, height, align), collapse = ',')

  sprintf('.%s\nimage::%s%s[%s]\n', cap, base, .upload.url(x), tags)
}

#' @rdname output_hooks
#' @export
render_asciidoc = function() {
  set_html_dev()
  opts_knit$set(out.format = 'asciidoc')
  knit_hooks$set(hooks_asciidoc())
}

#' @rdname output_hooks
#' @export
hooks_asciidoc = function() {
  hook.source = function(x, options) {
    x = one_string(c(hilight_source(x, 'asciidoc', options), ''))
    sprintf('\n[source,%s]\n----\n%s----\n', tolower(options$engine), x)
  }
  hook.message = function(x, options) {
    sprintf('\n[NOTE]\n====\n.Message\n%s\n====\n', substring(x, comment_length(options$comment)))
  }
  hook.warning = function(x, options) {
    sprintf('\n[WARNING]\n====\n.Warning\n%s\n====\n', gsub('^.*Warning: ', '', x))
  }
  hook.error = function(x, options) {
    sprintf('\n[CAUTION]\n====\n.Error\n%s\n====\n', gsub('^.*Error: ', '', x))
  }
  hook.output = function(x, options) sprintf('\n----\n%s----\n', x)
  list(
    source = hook.source, output = hook.output, message = hook.message,
    warning = hook.warning, error = hook.error, plot = hook_plot_asciidoc
  )
}

comment_length = function(x) {
  (if (is.null(x) || !nzchar(x) || is.na(x)) 0L else nchar(x)) + 1L
}
