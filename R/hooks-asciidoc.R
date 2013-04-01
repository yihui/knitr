#' @rdname hook_plot
#' @export
hook_plot_asciidoc = function(x, options) {
  base = opts_knit$get('base.url') %n% ''
  cap = .img.cap(options)
 
  alt <- cap
  width <- sprintf("width=%s", options$out.width)
  height <- sprintf("height=%s", options$out.height)
  align <- sprintf("align=%s", options$fig.align)  
  tags <- paste(c(alt, width, height, align), collapse = ",")
  
  sprintf(
    '%s\nimage::%s[%s]',
    cap,
    .upload.url(x),
    tags
  )
}

#' @rdname output_hooks
#' @export
render_asciidoc = function() {
  knit_hooks$restore()
  opts_chunk$set(dev = 'png') # default device is png in HTML and markdown
  opts_knit$set(out.format = 'asciidoc')
  hook.source = function(x, options) {
    sprintf(
      '\n[source,%s]\n----\n%s----\n', 
      tolower(options$engine), 
      x
    )
  }
  hook.message = function(x, options) {
    sprintf('\n[NOTE]\n%s\n', x)
  }
  hook.warning = function(x, options) {
      sprintf('\n[WARNING]\n%s\n', x)
  }
  hook.error = function(x, options) {
    sprintf('\n[CAUTION]\n%s\n', x)
  }  
  hook.output = function(x, options) {
    sprintf(
      '\n[source,%s]\n----\ns %s----\n', 
      tolower(options$engine), 
      options$comment, 
      x
    )
  }
  hook.inline = function(x, options) {
    sprintf('+%s+', x)
  }
  hook.chunk = function(x, options) {
    x = gsub("-{4,}", "----", x)
    if (is.null(s <- options$indent)) return(x)
    line_prompt(x, prompt = s, continue = s)
  }

  knit_hooks$set(
    source = hook.source,
    output = hook.output,
    message = hook.message,
    warning = hook.warning,
    error = hook.error,
    plot = hook_plot_asciidoc,
    inline = hook.inline,
    chunk = hook.chunk
  )
}