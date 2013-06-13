#' @rdname hook_plot
#' @export
hook_plot_html = function(x, options) {
  # pull out all the relevant plot options
  fig.num = options$fig.num = options$fig.num %n% 1L
  fig.cur = options$fig.cur %n% 1L

  if(options$fig.show == 'animate') {
    # Don't print out intermediate plots if we're animating
    return(if(fig.cur < fig.num) '' else opts_knit$get('animation.fun')(x, options))
  }
  ai = options$fig.show == 'asis'
  plot1 = ai || fig.cur <= 1L; plot2 = ai || fig.cur == fig.num
  d1 = if (plot1) str_c(if (out_format('html')) '</div>',
                        sprintf('<div class="rimage %s">', options$fig.align))
  d2 = if (plot2) str_c('</div>', if (out_format('html')) '<div class="rcode">')
  paste(
    d1, .img.tag(
      .upload.url(x), options$out.width, options$out.height, .img.cap(options),
      paste(c(options$out.extra, 'class="plot"'), collapse = ' ')
    ), d2, '\n', sep = ''
  )
}

.img.tag = function(src, width, height, caption, extra) {
  extra = paste(c(sprintf('width="%s"', width), sprintf('height="%s"', height),
                  extra), collapse = ' ')
  paste('<img src="', opts_knit$get('base.url'), src,
        '" title="', caption, '" alt="', caption, '" ', extra, ' />', sep = '')
}

.img.cap = function(options) {
  options$fig.cap %n% sprintf('plot of chunk %s', options$label)
}

## a wrapper to upload an image and return the URL
.upload.url = function(x) {
  file = paste(x, collapse = '.')
  opts_knit$get('upload.fun')(file)
}

.chunk.hook.html = function(x, options) {
  if (output_asis(x, options)) return(x)
  x = sprintf('<div class="chunk" id="%s"><div class="rcode">%s</div></div>',
              options$label, x)
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
  fig.num = options$fig.num
  # set up the ffmpeg run
  ffmpeg.opts = options$aniopts
  fig.fname = str_c(sub(str_c(fig.num, '$'), '%d', x[1]), '.', x[2])
  mov.fname = str_c(sub(paste(fig.num, '$',sep = ''), '', x[1]), '.ogg')
  if(is.na(ffmpeg.opts)) ffmpeg.opts = NULL

  ffmpeg.cmd = paste('ffmpeg', '-y', '-r', 1/options$interval,
                     '-i', fig.fname, mov.fname)
  message('executing: ', ffmpeg.cmd)
  system(ffmpeg.cmd, ignore.stdout = TRUE)

  # figure out the options for the movie itself
  mov.opts = sc_split(options$aniopts)
  opt.str = paste(sprintf('width=%s', options$out.width),
                  sprintf('height=%s', options$out.height),
                  if('controls' %in% mov.opts) 'controls="controls"',
                  if('loop' %in% mov.opts) 'loop="loop"')
  sprintf('<video %s><source src="%s" />video of chunk %s</video>',
          opt.str, str_c(opts_knit$get('base.url'), mov.fname), options$label)
}

opts_knit$set(animation.fun = hook_ffmpeg_html)

## use SciAnimator to create animations
#' @rdname hook_animation
#' @export
hook_scianimator = function(x, options) {
  fig.num = options$fig.num
  base = opts_knit$get('base.url') %n% ''

  # write the div and js code here
  id = gsub('[^[:alnum:]]', '_', options$label)
  sprintf('
<div class="scianimator">
<div id="%s" style="display: inline-block;">
</div>
</div>
<script type="text/javascript">
  (function($) {
    $(document).ready(function() {
      var imgs = Array(%s);
      for (i=0; ; i++) {
        if (i == imgs.length) break;
        imgs[i] = "%s%s" + (i + 1) + ".%s";
      }
      $("#%s").scianimator({
          "images": imgs,
          "delay": %s,
          "controls": ["first", "previous", "play", "next", "last", "loop", "speed"],
      });
      $("#%s").scianimator("play");
    });
  })(jQuery);
</script>
',
          id, fig.num, base, sub(str_c(fig.num, '$'), '', x[1]), x[2], id,
          options$interval * 1000, id)
}


## use the R2SWF package to create Flash animations
#' @rdname hook_animation
#' @export
hook_r2swf = function(x, options) {
  library(R2SWF)

  fig.num = options$fig.num
  # set up the R2SWF run
  fig.name = str_c(sub(str_c(fig.num, '$'), '', x[1]), 1:fig.num, '.', x[2])
  swf.name = fig_path('.swf', options)

  w = options$out.width %n% (options$fig.width * options$dpi)
  h = options$out.height %n% (options$fig.height * options$dpi)

  swfhtml = swf2html(file2swf(files = fig.name, swf.name, interval = options$interval),
                     output = FALSE, fragment = TRUE,  width = w, height = h)
  if(options$fig.align == 'default') return(swfhtml)
  sprintf(paste('<div align = "%s">\n', swfhtml, '\n</div>'), options$fig.align)
}

#' @rdname output_hooks
#' @export
render_html = function() {
  knit_hooks$restore()
  opts_chunk$set(dev = 'png') # default device is png in HTML and markdown
  opts_knit$set(out.format = 'html')
  ## use div with different classes
  html.hook = function(name) {
    force(name)
    function (x, options) {
      sprintf('<div class="%s"><pre class="knitr %s">%s</pre></div>\n', name, tolower(options$engine), x)
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
