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

    a = options$align; plot.cur = options$plot.cur
    align1 = switch(a, left = '\n\n', center = '\n\n\\hfill{}', right = '\n\n\\hfill{}', '')
    align2 = switch(a, left = '\\hfill{}\n\n', center = '\\hfill{}\n\n', right = '\n\n', '')
    ## multiple plots: begin at 1, end at plot.num
    if (options$fig.hold && plot.cur > 1L)
        align1 = ''
    if (options$fig.hold && plot.cur > 0L && plot.cur < options$plot.num)
        align2 = ''

    paste(align1, resize1,

          if (tikz) sprintf('\\input{%s.tikz}', x[1]) else {
              size =
                  paste(sprintf('width=%s', options$out.width),
                        sprintf('height=%s', options$out.height), sep = '', collapse = ',')
              if (nzchar(size)) size = sprintf('[%s]', size)
              sprintf('\\includegraphics%s{%s} ', size, x[1])
          },

          resize2, align2, sep = '')
}
.plot.hook.html = function(x, options) {
    ## TODO: output size not implemented for HTML yet
    a = options$align
    sprintf('<img src="%s" class="knit plot" %s/>\n',
            paste(x, collapse = '.'),
            switch(a,
                   default = '',
                   left = 'style="float: left"',
                   right = 'style="float: right"',
                   center = 'style="margin: auto; display: block"'))
}
.chunk.hook.tex = function(x, options) {
    if (is_blank(x)) return(x)
    if (str_detect(opts_knit$get('header')['framed'], fixed('\\usepackage{framed}')))
        x = str_c(framed_color(options$background), '\\begin{shaded}\n', x, '\n\\end{shaded}')
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
    sprintf('<pre>%s</pre>', x)
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
##' \code{hooks} is used to access or set hooks in this package.
##' @export
##' @references Usage: \url{http://yihui.github.com/knitr/objects}
##'
##' Components in \code{kpat}: \url{http://yihui.github.com/knitr/hooks}
##' @examples hooks$get('source'); hooks$get('inline')
hooks =
    new_defaults(c(
                   ## hooks for code output
                   list(source = .out.hook, output = .out.hook,
                        warning = .out.hook, message = .out.hook, error = .out.hook,
                        plot = .plot.hook.tex,
                        inline = .inline.hook, chunk = function(x, options) x)

                   ## and hooks for params
                   ))

knit_hooks = hooks  # I use this longer name internally

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

hooks_html = function() {
    knit_hooks$restore()
    ## use div with different classes
    html.hook = function(name) {
        function (x, options) sprintf('<div class="knit %s">%s</div>\n', name, x)
    }
    z = list()
    for (i in c('source', 'output', 'warning', 'message', 'error'))
        z[[i]] = html.hook(i)
    knit_hooks$set(z)
    knit_hooks$set(inline = function(x)
                   sprintf('<code class="knit inline">%s</code>', .inline.hook(x)),
                   plot = .plot.hook.html, chunk = .chunk.hook.html)
}

hooks_latex = function() {
    knit_hooks$restore()
    ## wrap source code in the Sinput environment, output in Soutput
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

hooks_jekyll = function() {
    knit_hooks$restore()
    hook.r = function(x, options) str_c('{% highlight r %}\n', x, '{% endhighlight %}\n')
    hook.t = function(x, options) str_c('{% highlight text %}\n', x, '{% endhighlight %}\n')
    knit_hooks$set(source = hook.r, output = hook.t, warning = hook.t,
                   error = hook.t, message = hook.t,
                   inline = function(x) sprintf('`%s`', x),
                   plot = function(x, options) {
                       base = opts_knit$get('base.url')
                       if (is.null(base)) base = ''
                       sprintf('![plot of chunk %s](%s%s.%s)', options$label,
                               base, x[1], x[2])
                   })
}

hooks_markdown = function() {
    knit_hooks$restore()
    hook.r = function(x, options) str_c('```r\n', x, '```\n')
    hook.t = function(x, options) str_c('```\n', x, '```\n')
    knit_hooks$set(source = hook.r, output = hook.t, warning = hook.t,
                   error = hook.t, message = hook.t,
                   inline = function(x) sprintf('`%s`', x),
                   plot = function(x, options) {
                       base = opts_knit$get('base.url')
                       if (is.null(base)) base = ''
                       sprintf('![plot of chunk %s](%s%s.%s)', options$label,
                               base, x[1], x[2])
                   })
}
