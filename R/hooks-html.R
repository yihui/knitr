#' @rdname hook_plot
#' @export
hook_plot_html = function(x, options) {
  # pull out all the relevant plot options
  fig.num = options$fig.num = options$fig.num %n% 1L
  fig.cur = options$fig.cur %n% 1L

  if (options$fig.show == 'animate') {
    # Don't print out intermediate plots if we're animating
    return(if (fig.cur < fig.num) '' else opts_knit$get('animation.fun')(x, options))
  }
  ai = options$fig.show == 'asis'
  plot1 = ai || fig.cur <= 1L; plot2 = ai || fig.cur == fig.num
  d1 = if (plot1) paste0(if (out_format('html')) '</div>',
                        sprintf('<div class="rimage %s">', options$fig.align))
  d2 = if (plot2) paste0('</div>', if (out_format('html')) '<div class="rcode">')
  paste0(
    d1, .img.tag(
      .upload.url(x), options$out.width, options$out.height, .img.cap(options),
      paste(c(options$out.extra, 'class="plot"'), collapse = ' ')
    ), d2, '\n'
  )
}

.img.attr = function(w, h, extra) {
  paste(c(sprintf('width="%s"', w), sprintf('height="%s"', h), extra), collapse = ' ')
}

.img.tag = function(src, w, h, caption, extra) {
  paste0(
    '<img src="', opts_knit$get('base.url'), src, '" title="', caption,
    '" alt="', caption, '" ', .img.attr(w, h, extra), ' />'
  )
}

.img.cap = function(options) {
  cap = options$fig.cap %n% {
    if (is.null(pandoc_to())) sprintf('plot of chunk %s', options$label) else ''
  }
  if (length(cap) == 0) cap = ''
  if (cap == '') return(cap)
  paste0(create_label(options$fig.lp, options$label), cap)
}

# a wrapper to upload an image and return the URL
.upload.url = function(x) {
  opts_knit$get('upload.fun')(x)
}

.chunk.hook.html = function(x, options) {
  if (output_asis(x, options)) return(x)
  x = sprintf('<div class="chunk" id="%s"><div class="rcode">%s</div></div>',
              options$label, x)
  x = gsub('<div class="rcode">\\s*</div>', '', x) # rm empty rcode layers
  if (options$split) {
    name = fig_path('.html', options, NULL)
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
  hook_ffmpeg(x, options, options$ffmpeg.format %n% 'webm')
}

hook_ffmpeg = function(x, options, format = 'webm') {
  x = c(sans_ext(x), file_ext(x))
  fig.num = options$fig.num
  format = sub('^[.]', '', format)
  # set up the ffmpeg run
  fig.fname = paste0(sub(paste0(fig.num, '$'), '%d', x[1]), '.', x[2])
  mov.fname = paste0(sub(paste0(fig.num, '$'), '', x[1]), '.', format)

  extra = if (format == 'webm') {
    paste('-b:v', options$ffmpeg.bitrate %n% '1M', '-crf 10')
  }
  ffmpeg.cmd = paste(
    'ffmpeg', '-y', '-r', 1 / options$interval, '-i', fig.fname, extra, mov.fname
  )

  message('executing: ', ffmpeg.cmd)
  system(ffmpeg.cmd, ignore.stdout = TRUE)

  # controls,loop --> controls loop
  opts = paste(sc_split(options$aniopts), collapse = ' ')
  opts = paste(
    sprintf('width="%s"', options$out.width),
    sprintf('height="%s"', options$out.height), opts
  )
  sprintf('<video %s><source src="%s" /><p>video of chunk %s</p></video>',
          opts, paste0(opts_knit$get('base.url'), mov.fname), options$label)
}

# use SciAnimator to create animations
#' @rdname hook_animation
#' @export
hook_scianimator = function(x, options) {
  x = c(sans_ext(x), file_ext(x))
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
      for (i = 0; ; i++) {
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
          id, fig.num, base, sub(paste0(fig.num, '$'), '', x[1]), x[2], id,
          options$interval * 1000, id)
}


# use the R2SWF package to create Flash animations
#' @rdname hook_animation
#' @export
hook_r2swf = function(x, options) {
  x = c(sans_ext(x), file_ext(x))
  fig.num = options$fig.num
  # set up the R2SWF run
  fig.name = paste0(sub(paste0(fig.num, '$'), '', x[1]), 1:fig.num, '.', x[2])
  swf.name = fig_path('.swf', options, NULL)

  w = options$out.width %n% (options$fig.width * options$dpi)
  h = options$out.height %n% (options$fig.height * options$dpi)

  swf2html = getFromNamespace('swf2html', 'R2SWF')
  file2swf = getFromNamespace('file2swf', 'R2SWF')
  swfhtml = swf2html(file2swf(files = fig.name, swf.name, interval = options$interval),
                     output = FALSE, fragment = TRUE,  width = w, height = h)
  if (options$fig.align == 'default') return(swfhtml)
  sprintf(paste('<div align = "%s">\n', swfhtml, '\n</div>'), options$fig.align)
}

#' @rdname output_hooks
#' @export
render_html = function() {
  set_html_dev()
  opts_knit$set(out.format = 'html')
  # use div with different classes
  html.hook = function(name) {
    force(name)
    function(x, options) {
      x = if (name == 'source') {
        c(hilight_source(x, 'html', options), '')
      } else escape_html(x)
      x = paste(x, collapse = '\n')
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
  }, output = html.hook('output'), plot = hook_plot_html, chunk = .chunk.hook.html)
}
