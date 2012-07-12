#' @rdname hook_plot
#' @export
hook_plot_html = function(x, options) {
  if(options$fig.show == 'animate') {
    return(opts_knit$get('animation.fun')(x, options))
  }
  fig.cur = options$fig.cur; fig.num = options$fig.num
  ai = options$fig.show == 'asis'
  plot1 = ai || fig.cur <= 1L; plot2 = ai || fig.cur == 0L || fig.cur == fig.num
  d1 = if (plot1) sprintf('</div><div class="rimage %s">', options$fig.align) else ''
  d2 = if (plot2) '</div><div class="rcode">' else ''
  add = paste(c(sprintf('width="%s"', options$out.width),
                sprintf('height="%s"', options$out.height),
                options$out.extra), collapse = ' ')
  sprintf('%s<img src="%s" %s class="plot" />%s\n', d1, .upload.url(x), add, d2)
}

## a wrapper to upload an image and return the URL
.upload.url = function(x) {
  file = paste(x, collapse = '.')
  opts_knit$get('upload.fun')(file)
}

.chunk.hook.html = function(x, options) {
  if (output_asis(x, options)) return(x)
  x = sprintf('<div class="chunk"><div class="rcode">%s</div></div>', x)
  x = gsub('<div class="rcode">\\s*</div>', '', x) # rm empty rcode layers
  if (options$split) {
    name = fig_path('.html', options)
    if (!file.exists(dirname(name)))
      dir.create(dirname(name))
    cat(x, file = name)
    sprintf('<iframe src="%s" class="knitr" width="100%%"></iframe>', name)
  } else x
}

#' Hooks to create animations in HTML output
#'
#' \code{hook_ffmpeg_html()} uses FFmpeg to convert images to a video;
#' \code{hook_scianimator()} uses the JavaScript library SciAnimator to create
#' animations; \code{hook_r2swf()} uses the \pkg{R2SWF} package.
#'
#' These hooks are mainly for the package option \code{animation.fun}, e.g. you
#' can set \code{opts_knit$set(animation.fun = hook_scianimator)}.
#' @inheritParams hook_plot_tex
#' @rdname hook_animation
#' @export
hook_ffmpeg_html = function(x, options) {
  # pull out all the relevant plot options
  fig.num = options$fig.num
  fig.cur = options$fig.cur %n% 0L

  # Don't print out intermediate plots if we're animating
  if(fig.cur < fig.num) return('')

  # set up the ffmpeg run
  ffmpeg.opts = options$aniopts
  fig.fname = str_c(sub(str_c(fig.num, '$'), '%d', x[1]), '.', x[2])
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

opts_knit$set(animation.fun = hook_ffmpeg_html)

## use SciAnimator to create animations
#' @rdname hook_animation
#' @export
hook_scianimator = function(x, options) {
  # pull out all the relevant plot options
  fig.num = options$fig.num
  fig.cur = options$fig.cur %n% 0L

  # Don't print out intermediate plots if we're animating
  if(fig.cur < fig.num) return('')

  fig.name = str_c(sub(str_c(fig.num, '$'), '', x[1]), 1:fig.num, '.', x[2])
  base = opts_knit$get('base.url') %n% ''
  fig.paths = str_c(shQuote(str_c(base, fig.name)), collapse = ", ")

  # write the div and js code here
  id = gsub('[^[:alnum:]]', '_', options$label)
  sid = str_c('#', id)
  sprintf('
<div class="scianimator"><div id="%s" style="display: inline-block;"></div></div>
<script type="text/javascript">
  (function($) {
    $(document).ready(function() {
      $("%s").scianimator({
          "images": [%s],
          "delay": %s,
          "controls": ["first", "previous", "play", "next", "last", "loop", "speed"],
      });
      $("%s").scianimator("play");
    });
  })(jQuery);
</script>
',
         id, sid, fig.paths, options$interval * 1000, sid)
}


## use the R2SWF package to create Flash animations
#' @rdname hook_animation
#' @export
hook_r2swf = function(x, options) {
  library(R2SWF)
  # pull out all the relevant plot options
  fig.num = options$fig.num
  fig.cur = options$fig.cur %n% 0L

  # Don't print out intermediate plots if we're animating
  if(fig.cur < fig.num) return('')

  # set up the R2SWF run
  fig.name = str_c(sub(str_c(fig.num, '$'), '', x[1]), 1:fig.num, '.', x[2])
  swf.name = fig_path('.swf', options)

  w = options$out.width %n% (options$fig.width * options$dpi)
  h = options$out.height %n% (options$fig.height * options$dpi)

  swfhtml = swf2html(file2swf(files = fig.name, swf.name, interval = options$interval),
                     output = FALSE, fragment = TRUE,  width = w, height = h)
  if(options$fig.align == 'default')  return(swfhtml)
  sprintf(paste('<div align = "%s">\n', swfhtml, '\n</div>'), options$fig.align)
}

#' @rdname output_hooks
#' @export
render_html = function() {
  knit_hooks$restore()
  opts_chunk$set(dev = 'png') # default device is png in HTML and markdown
  ## use div with different classes
  html.hook = function(name) {
    force(name)
    function (x, options) {
      sprintf('<div class="%s"><pre class="knitr %s">%s</pre></div>', name, options$engine, x)
    }
  }
  h = opts_knit$get('header')
  if (!nzchar(h['highlight'])) set_header(highlight = .header.hi.html)
  z = list()
  for (i in c('source', 'warning', 'message', 'error'))
    z[[i]] = html.hook(i)
  knit_hooks$set(z)
  knit_hooks$set(inline = function(x) {
    sprintf(if (inherits(x, 'AsIs')) '%s' else '<code class="knitr inline">%s</code>',
            .inline.hook(format_sci(x, 'html')))
  }, output = function(x, options) {
    if (output_asis(x, options)) x else html.hook('output')(x, options)
  }, plot = hook_plot_html, chunk = .chunk.hook.html)
}
