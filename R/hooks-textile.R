#' @rdname hook_plot
#' @export
hook_plot_textile = function(x, options) {
  base = opts_knit$get('base.url') %n% ''
  cap = .img.cap(options)

  width = sprintf('width=%s', options$out.width)
  height = sprintf('height=%s', options$out.height)
  align = sprintf('align=%s', options$fig.align)
  tags = paste(c(width, height, align), collapse = ';')

  sprintf('!{%s}%s(%s)!\n\np(knitr_plot_caption#%s). %s', tags, .upload.url(x), cap, options$label, cap)
}

.chunk.hook.textile = function(x, options) {
  if (output_asis(x, options)) return(x)
  sprintf('###. start of chunk "%s"\n%s\n###. end of chunk "%s"',
              options$label, x, options$label)
}

#' @rdname output_hooks
#' @export
render_textile = function() {
  knit_hooks$restore()
  set_html_dev()
  opts_knit$set(out.format = 'textile')
  textile.hook = function(name) {
    force(name)
    function (x, options) {
      if (name == 'source') {
        x = paste(c(hilight_source(x, 'textile', options), ''), collapse = '\n')
      }
      sprintf('bc(knitr_%s_%s#%s).. \n%s\np(knitr_end). \n\n', tolower(options$engine), name, options$label, x)
    }
  }
  hook.output = function(x, options) {
    if (output_asis(x, options)) x else textile.hook('output')(x, options)
  }
  hook.inline = function(x) {
      sprintf(if (inherits(x, 'AsIs')) '%s' else '@(knitr inline)%s@',
              .inline.hook(format_sci(x, 'html')))
  }
  z = list()
  for (i in c('source', 'warning', 'message', 'error'))
    z[[i]] = textile.hook(i)
  knit_hooks$set(z)
  knit_hooks$set(inline = hook.inline, 
                 output = hook.output, 
                 plot = hook_plot_textile,
		 		 chunk = .chunk.hook.textile)
}
