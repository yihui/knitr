## default plot hook: x is c(filename, extension)
.plot.hook.tex = function(x, options) {
    if (!options$include) return('')
    rw = options$resize.width; rh = options$resize.height
    resize1 = resize2 = ''
    if (!is.null(rw) || !is.null(rh)) {
        resize1 =
            sprintf('\\resizebox{%s}{%s}{', ifelse(is.null(rw), '!', rw),
                    ifelse(is.null(rh), '!', rh))
        resize2 = '} '
    }

    tikz = options$dev == 'tikz' && !options$external

    a = options$align; plot.cur = options$plot.cur; plot.num = options$plot.num
    align1 = switch(a, left = '\n\n', center = '\n\n\\centering{}', right = '\n\n\\hfill{}', '')
    align2 = switch(a, left = '\\hfill{}\n\n', center = '\n\n', right = '\n\n', '')
    ## multiple plots: begin at 1, end at plot.num
    if (options$fig.hold && plot.cur > 1L)
        align1 = ''
    if (options$fig.hold && plot.cur > 0L && plot.cur < plot.num)
        align2 = ''

    size =
        paste(sprintf('width=%s', options$out.width),
              sprintf('height=%s', options$out.height), sep = '', collapse = ',')

    paste(align1, resize1,

          if (tikz) {
              sprintf('\\input{%s.tikz}', x[1])
          } else if (options$animate) {
              ## \animategraphics{} should be inserted only *once*!
              if (plot.cur == 1L) {
                  aniopts = options$aniopts
                  aniopts = if (is.na(aniopts)) NULL else gsub(';', ',', aniopts)
                  size = paste(size, sprintf(',%s', aniopts), sep = '')
                  if (nzchar(size)) size = sprintf('[%s]', size)
                  sprintf('\\animategraphics%s{%s}{%s}{%s}{%s}', size, 1/options$interval,
                          sub('1$', '', x[1]), 1L, plot.num)
              }
          } else {
              if (nzchar(size)) size = sprintf('[%s]', size)
              sprintf('\\includegraphics%s{%s} ', size, x[1])
          },

          resize2, align2, sep = '')
}
.plot.hook.html = function(x, options) {
    ## TODO: output size not implemented for HTML yet
    a = options$align
    sprintf('<img src="%s" class="plot" %s/>\n',
            paste(x, collapse = '.'),
            switch(a,
                   default = '',
                   left = 'style="float: left"',
                   right = 'style="float: right"',
                   center = 'style="margin: auto; display: block"'))
}
.plot.hook.markdown = function(x, options) {
    base = opts_knit$get('base.url')
    if (is.null(base)) base = ''
    sprintf('![plot of chunk %s](%s%s.%s)', options$label,
            base, x[1], x[2])
}
.chunk.hook.tex = function(x, options) {
    if (is_blank(x) || options$results %in% c('tex', 'asis')) return(x)
    x = str_c(framed_color(options$background), '\\begin{kframe}\n', x, '\n\\end{kframe}')
    x = str_c('\\begin{knitrout}\n', x, '\n\\end{knitrout}')
    if (options$split) {
        name = str_c(options$prefix.string, options$label, '.tex')
        if (!file.exists(dirname(name)))
            dir.create(dirname(name))
        cat(x, file = name)
        if (options$include) sprintf('\\input{%s}', name) else ''
    } else x
}
.chunk.hook.html = function(x, options) {
    if (is_blank(x)) return(x)
    sprintf('<pre class="knitr">%s</pre>', x)
}

## format a single inline object
.inline.hook = function(x) {
    if (is.numeric(x)) x = formatC(x)
    paste(as.character(x), collapse = ', ')
}

## single param hook: a function of one argument
.param.hook = function(before, options, envir) {
    if (before) {
        'do something before the code chunk'
    } else {
        'do something after the code chunk'
    }
}

.out.hook = function(x, options) x

##' Hooks for R code chunks, inline R code and output
##'
##' A hook is a function of a pre-defined form (arguments) that takes
##' values of arguments and returns desired output. The object
##' \code{knit_hooks} is used to access or set hooks in this package.
##' @export
##' @references Usage: \url{http://yihui.github.com/knitr/objects}
##'
##' Components in \code{knit_hooks}: \url{http://yihui.github.com/knitr/hooks}
##' @examples knit_hooks$get('source'); knit_hooks$get('inline')
knit_hooks =
    new_defaults(c(
                   ## hooks for code output
                   list(source = .out.hook, output = .out.hook,
                        warning = .out.hook, message = .out.hook, error = .out.hook,
                        plot = .plot.hook.tex,
                        inline = .inline.hook, chunk = function(x, options) x)

                   ## and hooks for params
                   ))

## hooks that return character values will be inserted into final output
run_hooks = function(before, options, envir) {
    ## default and user-defined new hooks
    hooks.d = knit_hooks$get(default = TRUE); hooks.n = knit_hooks$get()
    hooks.a = hooks.n[setdiff(names(hooks.n), names(hooks.d))] # a list of hooks to run
    out = NULL
    for (i in names(hooks.a)) {
        if (isTRUE(options[[i]])) {
            ## run only when option is TRUE
            res = hooks.a[[i]](before = before, options = options, envir = envir)
            if (is.character(res)) out = c(out, res)
        }
    }
    out
}

##' Themes for output
##'
##' The theme functions are used to set output hooks. Currently there
##' are built-in themes for LaTeX, HTML, Markdown, GFM (GitHub
##' Flavored Markdown) and Jekyll (a blogging system on GitHub). The
##' original Sweave style is supported via \code{theme_sweave()}.
##' @rdname themes
##' @return \code{NULL}; corresponding hooks are set
##' @export
##' @references See output hooks in \url{http://yihui.github.com/knitr/hooks}
theme_latex = function() {
    res = try(system("kpsewhich framed.sty", intern = TRUE), silent = TRUE)
    if (inherits(res, 'try-error') || !length(res)) {
        warning("unable to find LaTeX package 'framed'; will copy from the knitr package")
        file.copy(system.file('misc', 'framed.sty', package = 'knitr'), '.')
    }
    set_header(framed = .header.framed, highlight = .header.hi.tex)
    knit_hooks$restore()
    verb.hook = function(x, options) str_c('\\begin{verbatim}\n', x, '\\end{verbatim}\n')
    knit_hooks$set(source = function(x, options) {
        if (options$highlight) {
            str_c('\\begin{flushleft}\n', x, '\\end{flushleft}\n')
        } else verb.hook(x, options)}, output = verb.hook,
                   warning = verb.hook, message = verb.hook, error = verb.hook,
                   inline = function(x) {
                       sprintf('\\verb|%s|', .inline.hook(x))
                   }, plot = .plot.hook.tex,
                   chunk = .chunk.hook.tex)
}
##' @rdname themes
##' @export
theme_sweave = function() {
    knit_hooks$restore()
    ## wrap source code in the Sinput environment, output in Soutput
    hook.i = function(x, options) str_c('\\begin{Sinput}\n', x, '\\end{Sinput}\n')
    hook.o = function(x, options) str_c('\\begin{Soutput}\n', x, '\\end{Soutput}\n')
    hook.c = function(x, options) str_c('\\begin{Schunk}\n', x, '\\end{Schunk}\n')
    knit_hooks$set(source = hook.i, output = hook.o, warning = hook.o,
                   message = hook.o, error = hook.o, inline = identity,
                   plot = function(x, options) sprintf('\\includegraphics{%s}', x[1]),
                   chunk = hook.c)
}
##' @rdname themes
##' @export
theme_html = function() {
    knit_hooks$restore()
    ## use div with different classes
    html.hook = function(name) {
        force(name)
        function (x, options) sprintf('<div class="%s">%s</div>', name, x)
    }
    set_header(highlight = .header.hi.html)
    z = list()
    for (i in c('source', 'output', 'warning', 'message', 'error'))
        z[[i]] = html.hook(i)
    knit_hooks$set(z)
    knit_hooks$set(inline = function(x)
                   sprintf('<code class="knitr inline">%s</code>', .inline.hook(x)),
                   plot = .plot.hook.html, chunk = .chunk.hook.html)
}
##' @rdname themes
##' @export
theme_markdown = function() {
    knit_hooks$restore()
    ## four spaces lead to <pre></pre>
    hook.t = function(x, options) evaluate:::line_prompt(x, '    ', '    ')
    knit_hooks$set(source = hook.t, output = hook.t, warning = hook.t,
                   error = hook.t, message = hook.t,
                   inline = function(x) sprintf('`%s`', .inline.hook(x)),
                   plot = .plot.hook.markdown)
}
##' @rdname themes
##' @export
theme_gfm = function() {
    ## gfm and jekyll are derived from markdown
    theme_markdown()
    hook.r = function(x, options) str_c('```r\n', x, '```\n')
    hook.t = function(x, options) str_c('```\n', x, '```\n')
    knit_hooks$set(source = hook.r, output = hook.t, warning = hook.t,
                   error = hook.t, message = hook.t)
}
##' @rdname themes
##' @export
theme_jekyll = function() {
    theme_markdown()
    hook.r = function(x, options) str_c('{% highlight r %}\n', x, '{% endhighlight %}\n')
    hook.t = function(x, options) str_c('{% highlight text %}\n', x, '{% endhighlight %}\n')
    knit_hooks$set(source = hook.r, output = hook.t, warning = hook.t,
                   error = hook.t, message = hook.t)
}
## may add textile, ReST and many other markup languages

##' A hook function to save rgl plots
##'
##' This function can be set as a hook in \pkg{knitr} to save plots
##' produced by the \pkg{rgl} package. According to the chunk option
##' \samp{dev} (graphical device), plots can be save to different
##' formats (\samp{postscript}: \samp{eps}; \samp{pdf}: \samp{pdf};
##' other devices correspond to the default PNG format). The plot
##' window will be adjusted according to chunk options \samp{width}
##' and \samp{height}. Filenames are derived from chunk labels and the
##' prefix string.
##' @param before,options,envir see references
##' @references \url{http://yihui.github.com/knitr/hooks#chunk_hooks}
##' @seealso \code{\link[rgl]{rgl.snapshot}},
##' \code{\link[rgl]{rgl.postscript}}
##' @export
hook_rgl = function(before, options, envir) {
    ## after a chunk has been evaluated
    if (before || !require('rgl') || rgl.cur() == 0) return()  # no active device
    name = paste(valid_prefix(options$prefix.string), options$label, sep = '')
    par3d(windowRect = 100 + options$dpi * c(0, 0, options$width, options$height))
    Sys.sleep(.05) # need time to respond to window size change
    ## support 3 formats: eps, pdf and png (default)
    switch(options$dev,
           postscript = rgl.postscript(paste(name, '.eps', sep = ''), fmt = 'eps'),
           pdf = rgl.postscript(paste(name, '.pdf', sep = ''), fmt = 'pdf'),
           rgl.snapshot(paste(name, '.png', sep = ''), fmt = 'png'))
    paste(ifelse(options$align == 'center', '\\centering{}', ''), '\\includegraphics[',
          sprintf('width=%s', options$out.width), ']{', name, '}\n', sep = '')
}
