---
layout: default
title: Hooks
subtitle: Customizable functions to run before/after a code chunk and tweak the output of knitr
---

- [Chunk hooks](#chunk_hooks)
- [Output hooks](#output_hooks)

The object `knit_hooks` in the **knitr** package is used to set hooks; the basic usage is `knit_hooks$set(param = FUN)` (see [objects](objects) for details) where `param` is the name of a chunk option (can be arbitrary), and `FUN` is a function. There are two types of hooks: chunk hooks and output hooks. Hook functions may have different forms, depending what they are designed to do.

## Chunk hooks <a id="chunk_hooks"></a>

Chunk hooks are functions to be called before or after a code chunk when the chunk option is not `NULL` (it basically means as long as you set a option, the hook will be run), and they should be defined with three arguments:

{% highlight r %}
foo_hook = function(before, options, envir) {
    if (before) {
        ## code to be run before a chunk
    } else {
        ## code to be run after a chunk
    }
}
{% endhighlight %}

When **knitr** is processing the document, `foo_hook(before = TRUE)` will be called before a code chunk is executed (unless the chunk is cached or set not to be evaluated), and `foo_hook(before = FALSE)` is called after a chunk; the argument `options` is a list of [options](options) in the current chunk (e.g. `options$label` is the label of the current chunk), and `envir` is the environment in which the code chunk is evaluated. The latter two arguments can be optionally used in a chunk hook. For example, if we set a hook for the `small.mar` option as:

{% highlight r %}
knit_hooks$set(small.mar = function(before, options, envir) {
    if (before) par(mar = c(4, 4, .1, .1))  # smaller margin on top and right
})
{% endhighlight %}

Then this function will be called for a chunk like this:

{% highlight r %}
% small.mar does not have to be TRUE, it can be any non-null value
<<myplot, small.mar=TRUE>>=
hist(rnorm(100), main = '')  # no main title
@
{% endhighlight %}

In **knitr**, hooks can also be used to insert texts into the output. To do this, the hook function must return a character result. This feature can greatly extend the power of hooks. Take the **rgl** package for example: if we want to insert 3D snapshots produced in **rgl** into our LaTeX document, we may consider this hook function (see the more sophisticated `hook_rgl()` in this package):

{% highlight r %}
knit_hooks$set(rgl = function(before, options, envir) {
  if (!before) {
    ## after a chunk has been evaluated
    if (rgl.cur() == 0) return()  # no active device
    name = paste(options$fig.path, options$label, sep = '')
    rgl.snapshot(paste(name, '.png', sep = ''), fmt = 'png')
    return(paste('\\includegraphics{', name, '}\n', sep = ''))
  }
})
{% endhighlight %}

And the code chunk may look like this:

{% highlight r %}
<<fancy-rgl, rgl=TRUE>>=
library(rgl)  # example taken from ?plot3d
open3d()
x = sort(rnorm(1000)); y = rnorm(1000); z = rnorm(1000) + atan2(x,y)
plot3d(x, y, z, col = rainbow(1000))
@
{% endhighlight %}

In the LaTeX output, we will see `\includegraphics{fancy-rgl}`.

To sum up,

1. the hook can be set in `knit_hooks` by `knit_hooks$set(foo = FUN)`;
2. the chunk option `foo` should take a non-`NULL` value in this chunk for the hook function to run;
3. a hook can be run before and/or after a chunk;
4. character results returned by hooks will be written into the output without modifications;

See [045-chunk-hook.md](https://github.com/yihui/knitr-examples/blob/master/045-chunk-hook.md) ([source](https://github.com/yihui/knitr-examples/blob/master/045-chunk-hook.Rmd)) for further examples.

## Output hooks <a id="output_hooks"></a>

Output hooks are used to customize and polish the *raw* output from chunks. There are 8 output hooks in all to deal with different types of output: 

- `source`: the source code
- `output`: ordinary R output (i.e., what would have been printed in an R terminal) except warnings, messages and errors
- `warning`: warnings from `warning()`
- `message`: messages from `message()`
- `error`: errors from `stop()` (applies to errors in both code chunks and inline R code)
- `plot`: graphics output
- `inline`: output of inline R code
- `chunk`: all the output of a chunk (i.e., those produced by the previous hooks)
- `document`: the output of the whole document (is `base::identity` by default)

All these hooks should be of the form `function(x, options)` (except the `inline` and `document` hooks which only have one argument `x`), where `x` is the character string of the output, and `options` is a list of current chunk options. Unlike chunk hooks which are empty by default, output hooks all come with default values. 

This package tried hard to set reasonable default output hooks for different parts of output and to accommodate different output formats such as LaTeX, HTML and even Jekyll. A series of functions of the form `render_xxx()` are provided to set built-in output hooks for different output formats, e.g. `render_latex()` and `render_html()`, etc. Details for these formats:

### LaTeX: render_latex()

If the output file type is LaTeX, default hooks will put most output in the `verbatim` environment, and numeric `inline` output will be formatted in scientific notation (see [output demo](/knitr/demo/output/) for details); `plot` and `chunk` hooks are more complicated:

- the default `plot` hook takes many factors into account to give a reasonable output, for example, if the graphics device is `tikz`, the command `\input{}` will be used, otherwise it uses the normal `\includegraphics{}`; depending on the `out.width` and `out.height` options, the hook will reset the size of the plot (e.g. `\includegraphics[width=.8\textwidth]{file}`); if there are multiple plots per chunk, we can set the option `fig.show='hold'` with an appropriate width so more than one plot can be arranged in a row (e.g., `.45\textwidth` means 2 plots per row); note this is not true for tikz graphics because they are inserted by `\input{}`, however, the chunk option `resize.width` and `resize.height` can be used to arrange multiple tikz plots in a row (via `\resizebox{resize.width}{resize.height}{file.tikz}`; if one option is `NULL`, it will be replaced by `!`; see LaTeX package `graphicx` for details); this hook function gives the user full power of using graphics in automatic report generation -- not only multiple plots per chunk and setting sizes of plots become possible, but also we can even put base graphics and grid graphics (e.g. **ggplot2**) or multiple grid plots side by side (think how hard it is, if not possible, for one to put two such plots in one window in R); there are four values for the `fig.align` option to decide how to align plots (`default`, `left`, `right`, `center`), e.g., it is easy to center the plots (set chunk option `fig.align='center'`)
- the default `chunk` hook is mainly used to decorate chunks; if the LaTeX package `framed` is available in the user's TeX software package (TeXLive or MikTeX or other packages), the chunk hook will put the whole output in the `kframe` environment with customizable background colors (default is very light gray), which makes the chunks cognitively better (they stands out from other normal texts yet do not have a too strong visual impact); in the end, all the output is wrapped in a `knitrout` environment, and the user can redefine this environment in LaTeX

### Sweave: render_sweave()

Put source code in the `Sinput` environment, output in the `Soutput` environment and the whole chunk in the `Schunk` environment. The style file `Sweave.sty` is required to use this theme, or at least these three environments have to be defined.

### Listings: render_listings()

Similar to Sweave, and [`Sweavel.sty`](https://github.com/yihui/knitr/blob/master/inst/misc/Sweavel.sty) is used instead.

### HTML: render_html()

To write output into an HTML file, the hooks will be automatically adjusted. Basically the output from chunks is put in `div` layers with classes, e.g. source code is in `<div class="knitr source"></div>`; the whole chunk output is in `<pre></pre>`; inline output is in `<code class="knitr inline"></code>`.

### Markdown: render_markdown()

The source code and output will be indented by 4 spaces. For GitHub Flavored Markdown, the source code is put in between ```` ```r ```` and ```` ``` ````; output is between ```` ``` ```` and ```` ``` ````.

### Jekyll: render_jekyll()

I need to build this site so I also set up some hooks especially for Jekyll, and they are actually quite simple: R source code is put in a highlight environment with the language set to `r`, and the rest of output belongs to the highlight environment with the `text` language (nearly no highlighting at all). Currently plots are written out according to the syntax of Markdown.

### reStructuredText: render_rst()

Code is put after `::` and indented by 4 spaces, or in the `sourcecode` directive.

