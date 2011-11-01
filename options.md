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
<<mychunk, cache=TRUE, fig.hold=TRUE, dev=png, background=.97;.97;1>>=
@
{% endhighlight %}

And `\SweaveOpts{comment=#, width=6, height=6}` can change the default global options in a document. All available options in **knitr** are:

- `eval`: (`TRUE`) whether to evaluate the code chunk
- `echo`: (`TRUE`) whether to include R source code in the output file
- `results`: (`format`) takes three possible values `format` (format the results using the output hook, e.g. put results in a special LaTeX environment), `asis` (output as-is, i.e., write raw results from R into the output document), or `hide` (hide results); note this option only applies to normal R output (not warnings, messages or errors), and `format` and `asis` are equivalent to `verbatim` and `tex` in Sweave respectively (you can still use the latter two, but they can be misleading, e.g., `verbatim` does not really mean verbatim in R, and `tex` seems to be restricted to LaTeX)
- `tidy`: (`TRUE`) whether R code should be tidied up using the function `tidy.source()` in the **formatR** package; if it failed to tidy up, original R code will not be changed; `tidy = TRUE` is like `keep.source = FALSE` in Sweave, but it also tries not to discard R comments
- `cache`: (`FALSE`) whether to cache a code chunk; when evaluating code chunks, the cached chunks are skipped, but the objects created in these chunks are (lazy) loaded from previously saved database (`.rdb` and `.rdx`) files, and these files are saved when a chunk is evaluated for the first time, or cached files are lost for some reason; note the filename consists of the chunk label with an MD5 digest of the R code in the chunk (to make it as unique as possible); unlike the **cacheSweave** package which uses **stashR**, this package directly uses internal functions in base R for cache, and another difference is that results of the code will *still* be included in the output even in case of cache (whereas **cacheSweave** has no output when a chunk is cached), because **knitr** also caches the printed output of a code chunk as a character string
- `purge`: (`FALSE`) whether to explicitly delete the cache for a chunk (note that if a chunk is changed, new cache results will be saved and old cache files will be removed automatically; **pgfSweave** will simply accumulate cache files, however)
- `ref`: (`NULL`) a filename of a reference R script in which a code chunk can be extracted using the chunk label as the identifier (in R code, use `## @knitr chunk-label` to denote the code below will go to the chunk); this make it possible to separate the main file and the R script, because sometimes it is easier to maintain a separate R script than to copy and paste R code directly into the main file (e.g. in LyX with an Sweave module, it is often difficult to run R code without compiling the whole document, which can be very time-consuming when we only want to run a small portion of the code)
- `prefix.string`: (default an empty string) prefix to be used for figure filenames; may contain a directory like `figure/prefix-` (will be created if it does not exist)
- `prompt`: (`FALSE`) whether to add the prompt characters in the R code (see `prompt` and `continue` in `?base::options`; note that adding prompts can make it difficult for readers to copy R code from the output, so `prompt = FALSE` may be a better choice
- `comment`: (`##`) the prefix to be put before source code output; default is to comment out the output by `##`, which is good for readers to copy R source code since output is masked in comments (set `comment=` to disable this feature)
- `fig`: (`FALSE`) whether to record plots in chunks; note any number of plots can be recorded in a single code chunk, and this package does not need to know how many plots are in a chunk in advance -- it can figure out automatically, and name these images as `prefix-string-label-i` where `i` is incremental from 1; if a code chunk does not actually produce any plots, **knitr** will not record anything either (the graphics device is open *only when plots are really produced*); in other words, it does not matter if `fig=TRUE` but no plots were produced
- `fig.low`: (`FALSE`) whether to let low-level plotting functions produce new plots instead of adding to the current plot; if `TRUE`, calling a low-level plotting function like `lines()` or `points()` will generate a new plot
- `fig.hold`: (`FALSE`) whether to hold all plots and output them in the very end of a code chunk; if `FALSE`, plots will be generated inline just as if the code were run in an R terminal
- `fig.last`: (`FALSE`) whether to keep the last plot only (and discard all previous plots in a chunk)
- `dev`: (`pdf`) a character string of the function name which will be used as a graphical device to record plots; for the convenience of usage, this package has included all the graphics devices in base R as well as those in **Cairo**, **cairoDevice** and **tikzDevice**, e.g. if we set `dev = CairoPDF`, the function with the same name in the **Cairo** package will be used for graphics output; if none of the 20 built-in devices is appropriate, we can still provide yet another name as long as it is a legal function name which can record plots (it must be of the form `function(filename, width, height)`, however); note the units for images are *always* inches (even for bitmap devices, in which DPI is used to convert between pixels and inches); currently available devices are `bmp`, `postscript`, `pdf`, `png`, `svg`, `jpeg`, `pictex`, `tiff`, `win.metafile`, `cairo_pdf`, `cairo_ps`, `CairoJPEG`, `CairoPNG`, `CairoPS`, `CairoPDF`, `CairoSVG`, `CairoTIFF`, `Cairo_pdf`, `Cairo_png`, `Cairo_ps`, `Cairo_svg`, `tikz`
- `fig.ext`: (`NULL`) file extension of the figure output (if `NULL`, it will be derived from the graphical device; see `knitr:::dev2ext` for details)
- `dpi`: (default 72) the DPI (dots per inch) for bitmap devices (`dpi * inches = pixels`
- `width`, `height`: (both are `7`) width and height of the plot, to be used in the graphics device (in inches)
- `out.width`, `out.height`: (`NULL`) width and height of the plot in the final output file (can be different with its real `width` and `height`, i.e. plots can be scaled in the output document)
- `resize.width`, `resize.height`: (`NULL`) the width and height to be used in `\resizebox{}{}` in LaTeX; these two options are not needed unless you want to resize tikz graphics because there is no natural way to do it; however, according to **tikzDevice** authors, tikz graphics is not meant to be resized to maintain consistency in style with other texts in LaTeX; if only one of them is `NULL`, `!` will be used (read the documentation of **graphicx** if you do not understand this)
- `align`: (`default`) alignment of figures in the output document (possible values are `left`, `right` and `center`; default is not to make any alignment adjustments)
- `external`: (`FALSE`) whether to externalize tikz graphics (pre-compile tikz graphics to PDF); it is only used for the `tikz()` device in the **tikzDevice** package (i.e., when `dev = tikz`)
- `sanitize`: whether to sanitize tikz graphics (escape special LaTeX characters); see documentation in the **tikzDevice** package
- `warning`: (`TRUE`) whether to show warnings (produced by `warning()`) in the output like we run R code in a terminal
- `error`: (`TRUE`) whether to show errors (from `stop()`) (by default, the evaluation will not stop even in case of errors!!)
- `message`: (`TRUE`) whether to show messages emitted by `message()`
- `background`: (`.97;.97;.97`) background color of chunks in LaTeX output (passed to the LaTeX package **framed**); the color model is `rgb`; the three numbers, all between 0 and 1, should be separated by `;` (very important because `,` is reserved as the separator of chunk options)

## Package Options <a id="package_options"></a>

- `progress`: (`TRUE`) whether to display a progress bar when running **knitr**
- `verbose`: (`FALSE`) whether to show verbose information (e.g., R code in each chunk) or just show chunk labels and options
- `render.to`: (`NULL`) possible values are `tex`, `html` and `jekyll`; it will be automatically determined based on the input file, and this option will affect which set of hooks to use
- `all.patterns`: a list of built-in patterns
- `header`: the text to be inserted into the output document before the document begins (e.g. after `\documentclass{article}` in LaTeX, or `<head>` in HTML); this is useful for defining commands and styles in the LaTeX preamble or HTML header; the beginning of document is found using the pattern defined in `kpats$get('document.begin')`
- `cache.dir`: (`cache`) the directory to store cache results
- `base.dir`: (`NULL`) an absolute directory under which the plots are generated
- `base.url`: (`NULL`) the base url for HTML pages
