---
layout: default
title: Options
subtitle: Chunk options and package options
---

- [Chunk Options](#chunk_options)
- [Package Options](#package_options)

The **knitr** package shares most options with Sweave, but some were dropped/changed and some new options were added. The default values are in the parentheses below. Note that the chunk label for each chunk is assumed to be unique, i.e., no two chunks share the same label. This is especially important for cache and plot filenames. Chunks without labels will be assigned labels like `unnamed-chunk-i` where `i` is the chunk number.

## Chunk Options <a id="chunk_options"></a>

Take Rnw files as an example: usually we write chunk options like this

{% highlight r %}
<<mychunk, cache=TRUE, fig.show=hold, dev=png, background=.97;.97;1>>=
@
{% endhighlight %}

And `\SweaveOpts{}` can change the default global options in a document (e.g. `\SweaveOpts{comment=#, fig.width=6, fig.height=6}`). A few special notes on the options:

1. Avoid spaces ` ` and periods `.` in chunk labels and directory names; if your output is a TeX document, these characters can cause troubles (in general it is recommended to use alphabetic characters with words separated by `-` and avoid other characters), e.g. `setup-options` is a good label, whereas `setup.options` and `chunk 1` are bad; `fig.path=figures/mcmc-` is a good prefix for figure output if this project is about MCMC, and `fig.path=markov chain/monte carlo` is bad;
2. For options that take _character_ values, you should not quote them as you do in R (e.g. should write `fig.path=abc` instead of `fig.path="abc"`), and should avoid the comma `,` in the character string since it is the separator of chunk options; for logical options, `TRUE` and `FALSE` are OK, and `true`/`false` will not work as you might have expected;

All available options in **knitr** are:

### Code Evaluation

- `eval`: (`TRUE`) whether to evaluate the code chunk

### Text Results

- `echo`: (`TRUE`) whether to include R source code in the output file
- `results`: (`markup`) takes three possible values `markup` (mark up the results using the output hook, e.g. put results in a special LaTeX environment), `asis` (output as-is, i.e., write raw results from R into the output document), or `hide` (hide results); note this option only applies to normal R output (not warnings, messages or errors), and `markup` and `asis` are equivalent to `verbatim` and `tex` in Sweave respectively (you can still use the latter two, but they can be misleading, e.g., `verbatim` does not really mean verbatim in R, and `tex` seems to be restricted to LaTeX)
- `warning`: (`TRUE`) whether to show warnings (produced by `warning()`) in the output like we run R code in a terminal
- `error`: (`TRUE`) whether to show errors (from `stop()`) (by default, the evaluation will not stop even in case of errors!!)
- `message`: (`TRUE`) whether to show messages emitted by `message()`
- `split`: (`FALSE`) whether to split the output from R into separate files and include them into LaTeX by `\input{}` (for LaTeX only)
- `include`: (`TRUE`) whether to include the chunk output in the final output document when `split=TRUE` and graphics output (if `include=FALSE`, you may want to manually insert output somewhere; this option is for LaTeX only)

### Code Decoration

- `tidy`: (`TRUE`) whether R code should be tidied up using the function `tidy.source()` in the **formatR** package; if it failed to tidy up, original R code will not be changed; `tidy=TRUE` is like `keep.source=FALSE` in Sweave, but it also tries not to discard R comments
- `prompt`: (`FALSE`) whether to add the prompt characters in the R code (see `prompt` and `continue` in `?base::options`; note that adding prompts can make it difficult for readers to copy R code from the output, so `prompt = FALSE` may be a better choice
- `comment`: (`##`) the prefix to be put before source code output; default is to comment out the output by `##`, which is good for readers to copy R source code since output is masked in comments (set `comment=NA` to disable this feature)
- `highlight`: (`TRUE`) whether to highlight the source code
- `size`: (`normalsize`) font size for highlighting some special characters such as the prompts `>` (see `?highlight` in the **highlight** package for details); to change the font size for the whole chunk, you can redefine the `knitrout` environment in the LaTeX preamble (see the chunk hook in the [hooks](/knitr/hooks) page and [an example](/knitr/demo/beamer))
- `background`: (`.97;.97;.97`) background color of chunks in LaTeX output (passed to the LaTeX package **framed**); the color model is `rgb`; the three numbers, all between 0 and 1, should be separated by `;` (very important because `,` is reserved as the separator of chunk options); alternatively, you can use built-in colors in R like `red` or `springgreen3` (see `colors()` for a full list), or a hex string like `#FFFF00`, or an integer (all these colors will be converted to the RGB model; see `?col2rgb` for details)

### Cache

- `cache`: (`FALSE`) whether to cache a code chunk; when evaluating code chunks, the cached chunks are skipped, but the objects created in these chunks are (lazy) loaded from previously saved databases (`.rdb` and `.rdx`) files, and these files are saved when a chunk is evaluated for the first time, or when cached files are not found (e.g. you may have removed them by hand); note the filename consists of the chunk label with an MD5 digest of the R code in the chunk (the MD5 string is a summary of the chunk text, and any changes in the chunk will produce a different MD5 digest); unlike the **cacheSweave** package which uses **stashR**, this package directly uses internal functions in base R for cache, and another difference is that results of the code will *still* be included in the output even in case of cache (whereas **cacheSweave** has no output when a chunk is cached), because **knitr** also caches the printed output of a code chunk as a character string
- `cache.path`: (`cache/`) a prefix to be used for the names of cache files (by default they are saved to a directory named `cache` relative to the current working directory; you can also use an absolute dir here, e.g. `/home/foo/bar-` or `D:\\abc\\mycache`, but it is not recommended since such absolute directories may not exist in other people's systems, therefore it is recommended to use relative directories)
- `dependson`: (`NULL`) which other chunks does this chunk depend on? the chunk labels should be separated by `;`, e.g. `chunk1;chunk2`; this option applies to cached chunks only -- sometimes the objects in a cached chunk may depend on other cached chunks, so when other chunks are changed, this chunk must be updated accordingly

### Plots

- `fig.path`: (default an empty string) prefix to be used for figure filenames (`fig.path` and chunk labels are concatenated to make filenames); it may contain a directory like `figure/prefix-` (will be created if it does not exist); the default empty string means files will be created under the current working directory
- `fig.keep`: (`high`) how plots in chunks should be kept; it takes five possible values (see the end of this section for an example)
  - `high`: only keep high-level plots (merge low-level changes into high-level plots);
  - `none`: discard all plots;
  - `all`: keep all plots (low-level plot changes may produce new plots)
  - `first`: only keep the first plot
  - `last`: only keep the last plot
- `fig.show`: (`asis`) how to show/arrange the plots; three possible values are
  - `asis`: show plots exactly in places where they were generated (as if the code were run in an R terminal);
  - `hold`: hold all plots and output them in the very end of a code chunk;
  - `animate`: wrap all plots into an animation if there are mutiple plots in a chunk;
- `dev`: (`pdf`) a character string of the function name which will be used as a graphical device to record plots; for the convenience of usage, this package has included all the graphics devices in base R as well as those in **Cairo**, **cairoDevice** and **tikzDevice**, e.g. if we set `dev = CairoPDF`, the function with the same name in the **Cairo** package will be used for graphics output; if none of the 20 built-in devices is appropriate, we can still provide yet another name as long as it is a legal function name which can record plots (it must be of the form `function(filename, width, height)`, however); note the units for images are *always* inches (even for bitmap devices, in which DPI is used to convert between pixels and inches); currently available devices are `bmp`, `postscript`, `pdf`, `png`, `svg`, `jpeg`, `pictex`, `tiff`, `win.metafile`, `cairo_pdf`, `cairo_ps`, `CairoJPEG`, `CairoPNG`, `CairoPS`, `CairoPDF`, `CairoSVG`, `CairoTIFF`, `Cairo_pdf`, `Cairo_png`, `Cairo_ps`, `Cairo_svg`, `tikz`
- `fig.ext`: (`NULL`) file extension of the figure output (if `NULL`, it will be derived from the graphical device; see `knitr:::dev2ext` for details)
- `dpi`: (default 72) the DPI (dots per inch) for bitmap devices (`dpi * inches = pixels`)
- `fig.width`, `fig.height`: (both are `7`) width and height of the plot, to be used in the graphics device (in inches)
- `out.width`, `out.height`: (`NULL`) width and height of the plot in the final output file (can be different with its real `fig.width` and `fig.height`, i.e. plots can be scaled in the output document)
- `resize.width`, `resize.height`: (`NULL`) the width and height to be used in `\resizebox{}{}` in LaTeX; these two options are not needed unless you want to resize tikz graphics because there is no natural way to do it; however, according to **tikzDevice** authors, tikz graphics is not meant to be resized to maintain consistency in style with other texts in LaTeX; if only one of them is `NULL`, `!` will be used (read the documentation of **graphicx** if you do not understand this)
- `fig.align`: (`default`) alignment of figures in the output document (possible values are `left`, `right` and `center`; default is not to make any alignment adjustments)
- `external`: (`FALSE`) whether to externalize tikz graphics (pre-compile tikz graphics to PDF); it is only used for the `tikz()` device in the **tikzDevice** package (i.e., when `dev=tikz`)
- `sanitize`: whether to sanitize tikz graphics (escape special LaTeX characters); see documentation in the **tikzDevice** package

Note any number of plots can be recorded in a single code chunk, and this package does not need to know how many plots are in a chunk in advance -- it can figure out automatically, and name these images as `prefix-string-label-i` where `i` is incremental from 1; if a code chunk does not actually produce any plots, **knitr** will not record anything either (the graphics device is open *only when plots are really produced*); in other words, it does not matter if `fig.keep=high` but no plots were produced.

Low-level plotting commands include `lines()` and `points()`, etc. To better understand `fig.keep`, consider the following chunk:

{% highlight r %}
<<test-plot>>=
plot(1)         # high-level plot
abline(0, 1)    # low-level change
plot(rnorm(10)) # high-level plot
## many low-level changes in a loop (a single R expression)
for(i in 1:10) {
    abline(v = i, lty = 2)
}
@
{% endhighlight %}

Normally this produces 2 plots in the output (i.e. when `fig.keep=high`); for `fig.keep=none`, no plots will be saved; for `fig.keep=all`, 4 plots are saved; for `fig.keep=first`, the plot produced by `plot(1)` is saved, and for `fig.keep=last`, the last plot with 10 vertical lines is saved.

### Animation

- `interval`: (`1`) number of seconds to pause between animation frames
- `aniopts`: (`controls;loop`) extra options for animations (should be separated by `;` in chunk options, and `;` will be replaced by `,` internally); see the [documentation of the animate package](http://www.ctan.org/tex-archive/macros/latex/contrib/animate)

### Chunk Reference

- `ref.label`: (`NULL`) labels of the chunks from which R code is borrowed; multiple labels should be separated by `;` (see the demo for [chunk reference](/knitr/demo/reference/))

## Package Options <a id="package_options"></a>

The package options can be changed using the object [`opts_knit`](objects); for example,

{% highlight r %}
opts_knit$set(progress = TRUE, verbose = TRUE)
{% endhighlight %}

All package options are:

- `progress`: (`TRUE`) whether to display a progress bar when running **knitr**
- `verbose`: (`FALSE`) whether to show verbose information (e.g., R code in each chunk) or just show chunk labels and options
- `out.format`: (`NULL`) possible values are `latex`, `sweave`, `html`, `markdown`, `gfm` and `jekyll`; it will be automatically determined based on the input file, and this option will affect which set of hooks to use (see `?render_latex` for example)
- `all.patterns`: a list of built-in patterns
- `header`: the text to be inserted into the output document before the document begins (e.g. after `\documentclass{article}` in LaTeX, or `<head>` in HTML); this is useful for defining commands and styles in the LaTeX preamble or HTML header; the beginning of document is found using the pattern defined in `knit_patternss$get('document.begin')`
- `base.dir`: (`NULL`) an absolute directory under which the plots are generated
- `base.url`: (`NULL`) the base url for HTML pages
