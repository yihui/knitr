#' @rdname hook_plot
#' @export
hook_plot_textile = function(x, options) {
  cap = .img.cap(options); if (is.na(cap)) cap = ''

  tags = unlist(c(Map(
    sprintf, c('width: %s', 'height: %s'),
    options[c('out.width', 'out.height')]
  ), css_align(options$fig.align)))
  tags = if (length(tags)) sprintf('{%s}', paste(tags, collapse = ';')) else ''

  paste(sep = '',
    '!', tags, opts_knit$get('base.url'), .upload.url(x),
    if (nzchar(cap)) sprintf('(%s)', cap), '!\n\n',
    if (nzchar(cap)) sprintf('p(knitr_plot_caption#%s). %s', options$label, cap)
  )
}

.chunk.hook.textile = function(x, options) {
  if (output_asis(x, options)) return(x)
  sprintf('==<!-- start of chunk "%s" -->==\n\n%s\n==<!-- end of chunk "%s" -->==\n',
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
      if (name == 'source') x = c(hilight_source(x, 'textile', options), '')
      x = paste(x, collapse = '\n')
      sprintf('bc(knitr %s %s#%s).. %s\np(knitr_end). \n\n',
              tolower(options$engine), name, options$label, x)
    }
  }
  hook.inline = function(x) {
      sprintf(if (inherits(x, 'AsIs')) '%s' else '@(knitr inline)%s@',
              .inline.hook(format_sci(x, 'html')))
  }
  z = list()
  for (i in c('source', 'warning', 'message', 'error'))
    z[[i]] = textile.hook(i)
  knit_hooks$set(z)
  knit_hooks$set(
    inline = hook.inline, output = textile.hook('output'), plot = hook_plot_textile,
    chunk = .chunk.hook.textile
  )
}
