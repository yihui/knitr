#' @rdname hook_plot
#' @export
hook_plot_textile = function(x, options) {
  cap = .img.cap(options); if (is.na(cap)) cap = ''

  tags = unlist(c(Map(
    sprintf, c('width: %s', 'height: %s'),
    options[c('out.width', 'out.height')]
  ), css_align(options$fig.align)))
  tags = if (length(tags)) sprintf('{%s}', paste(tags, collapse = ';')) else ''

  paste0(
    '!', tags, opts_knit$get('base.url'), .upload.url(x),
    if (nzchar(cap)) sprintf('(%s)', cap), '!\n\n',
    if (nzchar(cap)) sprintf('p(knitr plot caption#%s). %s', options$label, cap),
    '\n\n'
  )
}

#' @rdname output_hooks
#' @export
render_textile = function() {
  set_html_dev()
  opts_knit$set(out.format = 'textile')
  knit_hooks$set(hooks_textile())
}

#' @rdname output_hooks
#' @export
hooks_textile = function() {
  hook = function(name) {
    force(name)
    function(x, options) {
      if (name == 'source') x = c(hilight_source(x, 'textile', options), '')
      x = one_string(x)
      if (name == 'output' && output_asis(x, options)) return(x)
      sprintf('bc(knitr %s %s#%s).. %s\np(knitr_end). \n\n',
              tolower(options$engine), name, options$label, x)
    }
  }
  list(
    source = hook('source'), output = hook('output'), warning = hook('warning'),
    message = hook('message'), error = hook('error'), plot = hook_plot_textile,
    inline = function(x) .inline.hook(format_sci(x, 'html'))
  )
}
