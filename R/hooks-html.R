#' @rdname hook_plot
#' @export
hook_plot_html = function(x, options) {
  ## TODO: output size not implemented for HTML yet
  if(options$fig.show == 'animate') {
    .ani.plot.hook.html(x, options)
  } else {
    ## TODO: output size not implemented for HTML yet
    a = options$fig.align
    sprintf('<img src="%s" class="plot" %s/>\n', .upload.url(x),
            switch(a, default = '', left = 'style="float: left"',
                   right = 'style="float: right"',
                   center = 'style="margin: auto; display: block"'))
  }
}

## a wrapper to upload an image and return the URL
.upload.url = function(x) {
  file = paste(x, collapse = '.')
  opts_knit$get('upload.fun')(file)
}

.chunk.hook.html = function(x, options) {
  if (output_asis(x, options)) return(x)
  x = sprintf('<pre class="knitr">%s</pre>', x)
  if (options$split) {
    name = fig_path('.html', options)
    if (!file.exists(dirname(name)))
      dir.create(dirname(name))
    cat(x, file = name)
    sprintf('<iframe src="%s" class="knitr" width="100%%"></iframe>', name)
  } else x
}
.ani.plot.hook.html = function(x, options) {
  # pull out all the relevant plot options
  fig.num = options$fig.num
  fig.cur = options$fig.cur
  if(is.null(fig.cur)) fig.cur = 0
  
  # Don't print out intermediate plots if we're animating
  if(fig.cur < fig.num) return('')
  
  # set up the ffmpeg run
  ffmpeg.opts = options$aniopts
  fig.fname = str_c(sub(str_c(fig.num, '$'), '%d', x[1]), x[2])
  mov.fname = str_c(sub(paste(fig.num, '$',sep = ''), '', x[1]), ".mp4")
  if(is.na(ffmpeg.opts)) ffmpeg.opts = NULL
  
  ffmpeg.cmd = paste("ffmpeg", "-y", "-r", 1/options$interval,
                     "-i", fig.fname, mov.fname)
  system(ffmpeg.cmd, ignore.stdout = TRUE)
  
  # figure out the options for the movie itself
  mov.opts = sc_split(options$aniopts)
  opt.str = paste(sprintf('width=%s', options$out.width),
                  sprintf('height=%s', options$out.height),
                  if('controls' %in% mov.opts) 'controls="controls"',
                  if('loop' %in% mov.opts) 'loop="loop"')
  sprintf('<video %s><source src="%s" type="video/mp4" />video of chunk %s</video>',
          opt.str, mov.fname, options$label)
}

#' @rdname output_hooks
#' @export
render_html = function() {
  knit_hooks$restore()
  opts_chunk$set(dev = 'png') # default device is png in HTML and markdown
  ## use div with different classes
  html.hook = function(name) {
    force(name)
    function (x, options) sprintf('<div class="%s">%s</div>', name, x)
  }
  h = opts_knit$get('header')
  if (!nzchar(h['highlight'])) set_header(highlight = .header.hi.html)
  z = list()
  for (i in c('source', 'output', 'warning', 'message', 'error'))
    z[[i]] = html.hook(i)
  knit_hooks$set(z)
  knit_hooks$set(inline = function(x) {
    sprintf(if (inherits(x, 'AsIs')) '%s' else '<code class="knitr inline">%s</code>',
            .inline.hook(format_sci(x, 'html')))
  }, plot = hook_plot_html, chunk = .chunk.hook.html)
}
