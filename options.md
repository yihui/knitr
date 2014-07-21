---
layout: default
title: Options
subtitle: Chunk options and package options
---

- [Chunk Options](#chunk_options)
- [Package Options](#package_options)

The **knitr** package shares most options with Sweave, but some were
dropped/changed and some new options were added. The default values are in
the parentheses below. Note that the chunk label for each chunk is assumed
to be unique, i.e., no two chunks share the same label. This is especially
important for cache and plot filenames. Chunks without labels will be
assigned labels like `unnamed-chunk-i` where `i` is the chunk number.

## Chunk Options <a id="chunk_options"></a>

Take Rnw files as an example: usually we write chunk options in the form
`tag=value` like this:

{% highlight r %}
<<mychunk, cache=TRUE, eval=FALSE, dpi=100>>=
@
{% endhighlight %}

And `opts_chunk$set()` can change the default global options in a document
(e.g. put this in a code chunk: `opts_chunk$set(comment=NA, fig.width=6,
fig.height=6)`), and `\SweaveOpts{}` will no longer be supported (it is good
if you do not know what this means). A few special notes on the options:

1. Chunk options must be written in one line; no line breaks are allowed
  inside chunk options;
1. Avoid spaces ` ` and periods `.` in chunk labels and directory names; if
  your output is a TeX document, these characters can cause troubles (in
  general it is recommended to use alphabetic characters with words
  separated by `-` or `_` and avoid other characters), e.g. `setup-options`
  is a good label, whereas `setup.options` and `chunk 1` are bad;
  `fig.path='figures/mcmc-'` is a good prefix for figure output if this
  project is about MCMC, and `fig.path='markov chain/monte carlo'` is bad;
  non-alphanumeric characters except `-` and `_` in figure filenames will be
  replaced with `_` automatically;
1. All option values must be _valid R expressions_ just like how we write
  function arguments;
  - for example, options that take _character_ values must be quoted as you
    do in R (e.g. should write `fig.path="abc"` instead of `fig.path=abc`,
    and `out.width='\\textwidth'` instead of `out.width=\textwidth`)
  - in theory, the chunk label should be quoted as well but the sake of
    convenience it will be automatically quoted if you did not quote it
    (e.g. `<<2a>>=` will become `<<'2a'>>=`)
  - for logical options, `TRUE` and `FALSE` are OK, and `true`/`false` will
    not work as you might have expected because `true` is not `TRUE`
  - you can write arbitrarily complicated expressions as you want as long as
    they are legitimate R code
  - Sweave users please read the [transition page](/knitr/demo/sweave/)
    carefully because the syntax is different

All built-in options in **knitr** are:

### Code Evaluation

- `eval`: (`TRUE`; logical) whether to evaluate the code chunk; it can also
  be a numeric vector to select which R expression(s) to evaluate, e.g.
  `eval=c(1, 3, 4)` or `eval=-(4:5)`

### Text Results

- `echo`: (`TRUE`; logical or numeric) whether to include R source code in
  the output file; besides `TRUE`/`FALSE` which completely turns on/off the
  source code, we can also use a numeric vector to select which R
  expression(s) to echo in a chunk, e.g. `echo=2:3` means only echo the 2nd
  and 3rd expressions, and `echo=-4` means to exclude the 4th expression
- `results`: (`'markup'`; character) takes these possible values
  - `markup`: mark up the results using the output hook, e.g. put results in
    a special LaTeX environment
  - `asis`: output as-is, i.e., write raw results from R into the output
    document
  - `hold`: hold all the output pieces and push them to the end of a chunk
  - `hide` hide results; this option only applies to normal R output (not
    warnings, messages or errors)
  - note `markup` and `asis` are equivalent to `verbatim` and `tex` in Sweave
    respectively (you can still use the latter two, but they can be
    misleading, e.g., `verbatim` does not really mean verbatim in R, and
    `tex` seems to be restricted to LaTeX)
- `collapse`: (`FALSE`; logical; applies to Markdown output only) whether
  to, if possible, collapse all the source and output blocks from one code
  chunk into a single block (by default, they are written to separate
  `<pre></pre>` blocks)
- `warning`: (`TRUE`; logical) whether to preserve warnings (produced by
  `warning()`) in the output like we run R code in a terminal (if `FALSE`,
  all warnings will be printed in the console instead of the output
  document); it can also take numeric values as indices to select a subset
  of warnings to include in the output
- `error`: (`TRUE`; logical) whether to preserve errors (from `stop()`); by
  default, the evaluation will not stop even in case of errors!! if we want
  R to stop on errors, we need to set this option to `FALSE`
- `message`: (`TRUE`; logical) whether to preserve messages emitted by
  `message()` (similar to `warning`)
- `split`: (`FALSE`; logical) whether to split the output from R into separate
  files and include them into LaTeX by `\input{}` or HTML by
  `<iframe></iframe>`
- `include`: (`TRUE`; logical) whether to include the chunk output in the final
  output document; if `include=FALSE`, nothing will be written into the
  output document, but the code is still evaluated and plot files are
  generated if there are any plots in the chunk, so you can manually insert
  figures; note this is the only chunk option that is not cached, i.e.,
  changing it will not invalidate the cache
- `strip.white`: (`TRUE`; logical) whether to remove the white lines in the
  beginning or end of a source chunk in the output

### Code Decoration

- `tidy`: (`FALSE`; logical) whether R code should be tidied up using the function `tidy.source()` in the **formatR** package; if it failed to tidy up, original R code will not be changed; `tidy=TRUE` is like `keep.source=FALSE` in Sweave, but it also tries not to discard R comments (N.B. this option does not work in certain cases; see <https://github.com/yihui/formatR/wiki> for more information)
  - `tidy.opts`: (`NULL`; list) a list of options to be passed to `tidy.source()`, e.g. `tidy.opts=list(keep.blank.line=FALSE, width.cutoff=60)`; these **formatR** options can also be set globally via `options()`
- `prompt`: (`FALSE`; logical) whether to add the prompt characters in the R code (see `prompt` and `continue` in `?base::options`; note that adding prompts can make it difficult for readers to copy R code from the output, so `prompt=FALSE` may be a better choice
- `comment`: (`'##'`; character) the prefix to be put before source code output; default is to comment out the output by `##`, which is good for readers to copy R source code since output is masked in comments (set `comment=NA` to disable this feature)
- `highlight`: (`TRUE`; logical) whether to highlight the source code (it is `FALSE` by default if the output is Sweave or listings)
- `size`: (`'normalsize'`; character) font size for the default LaTeX output (see `?highlight` in the **highlight** package for a list of possible values)
- `background`: (`'#F7F7F7'`; character or numeric) background color of chunks in LaTeX output (passed to the LaTeX package **framed**); the color model is `rgb`; it can be either a numeric vector of length 3, with each element between 0 and 1 to denote red, green and blue, or any built-in color in R like `red` or `springgreen3` (see `colors()` for a full list), or a hex string like `#FFFF00`, or an integer (all these colors will be converted to the RGB model; see `?col2rgb` for details)

There is a hidden option `indent` which stores the possible leading white spaces of the chunk, e.g. for the chunk below, `indent` is a character string of two spaces:

{% highlight r %}
  ```{r}
  rnorm(10)
  ```
{% endhighlight %}

Currently this option is only used to indent markdown output, because
leading white spaces have special meanings in markdown.

### Cache

- `cache`: (`FALSE`; logical) whether to cache a code chunk; when evaluating
  code chunks, the cached chunks are skipped, but the objects created in
  these chunks are (lazy-) loaded from previously saved databases (`.rdb`
  and `.rdx`) files, and these files are saved when a chunk is evaluated for
  the first time, or when cached files are not found (e.g. you may have
  removed them by hand); note the filename consists of the chunk label with
  an MD5 digest of the R code in the chunk (the MD5 string is a summary of
  the chunk text, and any changes in the chunk will produce a different MD5
  digest); unlike the **cacheSweave** package which uses **stashR**, this
  package directly uses internal functions in base R for cache, and another
  difference is that results of the code will *still* be included in the
  output even when cache is used (whereas **cacheSweave** has no output when
  a chunk is cached), because **knitr** also caches the printed output of a
  code chunk as a character string
  - for advanced users, `cache` can be more granular and takes numeric
    values `0, 1, 2, 3`; see the [cache example](/knitr/demo/cache) page
    for "More granular cache"
- `cache.path`: (`'cache/'`; character) a prefix to be used for the names of
  cache files (by default they are saved to a directory named `cache`
  relative to the current working directory; you can also use an absolute
  dir here, e.g. `/home/foo/bar-` or `D:\\abc\\mycache`, but it is not
  recommended since such absolute directories may not exist in other
  people's systems, therefore it is recommended to use relative directories)
- `cache.vars`: (`NULL`) a character vector of variable names to be saved in
  the cache database; by default all variables created in the current chunks
  are identified and saved, but we can manually set the variables to be saved
- `cache.lazy`: (`TRUE`) whether to `lazyLoad()` or directly `load()` objects;
  for very large objects, lazyloading may not work, so `cache.lazy=FALSE`
  may be desirable (see [#572](https://github.com/yihui/knitr/issues/572))
- `cache.comments`: (`NULL`) if `FALSE`, changing comments in R code chunks will
  not invalidate the cache database
- `dependson`: (`NULL`; character or numeric) a character vector of chunk labels
  to specify which other chunks this chunk depends on; this option applies
  to cached chunks only -- sometimes the objects in a cached chunk may
  depend on other cached chunks, so when other chunks are changed, this
  chunk must be updated accordingly
  - if `dependson` is a numeric vector, it means the indices of chunk labels,
    e.g. `dependson=1` means this chunk depends on the first chunk in the
    document, and `dependson=c(-1, -2)` means it depends on the previous two
    chunks (negative indices stand for numbers of chunks before this chunk,
    and note they are always relative to the current chunk)
  - please note this option does not work when set as a global chunk option
    via `opts_chunk$set()`; it must be set as a local chunk option
- `autodep`: (`FALSE`; logical) whether to figure out the dependencies among
  chunks automatically by analyzing the global variables in the code (may
  not be reliable) so that `dependson` does not need to be set explicitly

### Plots

- `fig.path`: (`'figure/'`; character) prefix to be used for figure filenames (`fig.path` and chunk labels are concatenated to make filenames); it may contain a directory like `figure/prefix-` (will be created if it does not exist); this path is relative to the current working directory
- `fig.keep`: (`'high'`; character) how plots in chunks should be kept; it takes five possible values (see the end of this section for an example)
  - `high`: only keep high-level plots (merge low-level changes into high-level plots);
  - `none`: discard all plots;
  - `all`: keep all plots (low-level plot changes may produce new plots)
  - `first`: only keep the first plot
  - `last`: only keep the last plot
- `fig.show`: (`'asis'`; character) how to show/arrange the plots; four possible values are
  - `asis`: show plots exactly in places where they were generated (as if the code were run in an R terminal);
  - `hold`: hold all plots and output them in the very end of a code chunk;
  - `animate`: wrap all plots into an animation if there are mutiple plots in a chunk;
  - `hide`: generate plot files but hide them in the output document
- `dev`: (`'pdf'` for LaTeX output and `'png'` for HTML/markdown; character) the function name which will be used as a graphical device to record plots; for the convenience of usage, this package has included all the graphics devices in base R as well as those in **Cairo**, **cairoDevice** and **tikzDevice**, e.g. if we set `dev='CairoPDF'`, the function with the same name in the **Cairo** package will be used for graphics output; if none of the 20 built-in devices is appropriate, we can still provide yet another name as long as it is a legal function name which can record plots (it must be of the form `function(filename, width, height)`); note the units for images are *always* inches (even for bitmap devices, in which DPI is used to convert between pixels and inches); currently available devices are `bmp`, `postscript`, `pdf`, `png`, `svg`, `jpeg`, `pictex`, `tiff`, `win.metafile`, `cairo_pdf`, `cairo_ps`, `CairoJPEG`, `CairoPNG`, `CairoPS`, `CairoPDF`, `CairoSVG`, `CairoTIFF`, `Cairo_pdf`, `Cairo_png`, `Cairo_ps`, `Cairo_svg`, `tikz` and a series of `quartz` devices including `quartz_pdf`, `quartz_png`, `quartz_jpeg`, `quartz_tiff`, `quartz_gif`, `quartz_psd`, `quartz_bmp` which are just wrappers to the function `quartz()` with different file types
  - the options `dev`, `fig.ext`, `fig.width`, `fig.height` and `dpi` can be vectors (shorter ones will be recycled), e.g. `<<foo, dev=c('pdf', 'png')>>=` creates two files for the same plot: `foo.pdf` and `foo.png`
- `dev.args`: (`NULL`) more arguments to be passed to the device, e.g. `dev.args=list(bg='yellow', pointsize=10)`; note this depends on the specific device (see the device documentation); when `dev` has multiple elements, `dev.args` can be a list of lists of arguments with each list of arguments to be passed to each single device, e.g. `<<dev=c('pdf', 'tiff'), dev.args=list(pdf = list(colormodel = 'cmyk', useDingats = TRUE), tiff = list(compression = 'lzw'))>>=`
- `fig.ext`: (`NULL`; character) file extension of the figure output (if `NULL`, it will be derived from the graphical device; see `knitr:::auto_exts` for details)
- `dpi`: (`72`; numeric) the DPI (dots per inch) for bitmap devices (`dpi * inches = pixels`)
- `fig.width`, `fig.height`: (both are `7`; numeric) width and height of the plot, to be used in the graphics device (in inches) and have to be numeric
- `out.width`, `out.height`: (`NULL`; character) width and height of the plot in the final output file (can be different with its real `fig.width` and `fig.height`, i.e. plots can be scaled in the output document); depending on the output format, these two options can take flexible values, e.g. for LaTeX output, they can be `.8\\linewidth`, `3in` or `8cm` and for HTML, they may be `300px` (do not have to be in inches like `fig.width` and `fig.height`; backslashes must be escaped as `\\`); for LaTeX output, the default value for `out.width` will be changed to `\\maxwidth` which is defined [here](/knitr/demo/framed/)
- `out.extra`: (`NULL`; character) extra options for figures, e.g. `out.extra='angle=90'` in LaTeX output will rotate the figure by 90 degrees; it can be an arbitrary string, e.g. you can write multiple figure options in this option; it also applies to HTML images (extra options will be written into the `<img />` tag, e.g. `out.extra='style="display:block;"'`)
- `fig.retina`: (`1`; numeric) this option only applies to HTML output; for [Retina displays](http://en.wikipedia.org/wiki/Retina_Display), setting this option to a ratio (usually 2) will change the chunk option `dpi` to `dpi * fig.retina`, and `out.width` to `fig.width * dpi / fig.retina` internally; for example, the physical size of an image is doubled and its display size is halved when `fig.retina = 2`
- `resize.width`, `resize.height`: (`NULL`; character) the width and height to be used in `\resizebox{}{}` in LaTeX; these two options are not needed unless you want to resize tikz graphics because there is no natural way to do it; however, according to **tikzDevice** authors, tikz graphics is not meant to be resized to maintain consistency in style with other texts in LaTeX; if only one of them is `NULL`, `!` will be used (read the documentation of **graphicx** if you do not understand this)
- `fig.align`: (`'default'`; character) alignment of figures in the output
  document (possible values are `left`, `right` and `center`; default is not
  to make any alignment adjustments); note that for Markdown output, forcing
  figure alignments will lead to the HTML tag `<img />` instead of the
  original Markdown syntax `![]()`, because Markdown does not have native
  support for figure alignments (see
  [#611](https://github.com/yihui/knitr/issues/611))
- `fig.env`: (`'figure'`) the LaTeX environment for figures, e.g. set `fig.env='marginfigure'` to get `\begin{marginfigure}`
- `fig.cap`: (`NULL`; character) figure caption to be used in a figure environment in LaTeX (in `\caption{}`); if `NULL` or `NA`, it will be ignored, otherwise a figure environment will be used for the plots in the chunk (output in `\begin{figure}` and `\end{figure}`)
- `fig.scap`: (`NULL`; character) short caption; if `NULL`, all the words before `.` or `;` or `:` will be used as a short caption; if `NA`, it will be ignored
- `fig.lp`: (`'fig:'`; character) label prefix for the figure label to be used in `\label{}`; the actual label is made by concatenating this prefix and the chunk label, e.g. the figure label for `<<foo-plot>>=` will be `fig:foo-plot` by default
- `fig.pos`: (`''`; character) a character string for the figure position arrangement to be used in `\begin{figure}[fig.pos]`
- `fig.subcap`: (`NULL`) captions for subfigures; when there are multiple plots in a chunk, and neither `fig.subcap` nor `fig.cap` is NULL, `\subfloat{}` will be used for individual plots (you need to add `\usepackage{subfig}` in the preamble); see [067-graphics-options.Rnw](https://github.com/yihui/knitr-examples/blob/master/067-graphics-options.Rnw) for an example
- `fig.process`: (`NULL`) a function to post-process a figure file; it should take a filename, and return a character string as the new source of the figure to be inserted in the output
- `fig.showtext`: (`NULL`) if `TRUE`, call `showtext::showtext.begin()` before drawing plots; see the documentation of the [**showtext**](http://cran.rstudio.com/package=showtext) package for details
- `external`: (`TRUE`; logical) whether to externalize tikz graphics (pre-compile tikz graphics to PDF); it is only used for the `tikz()` device in the **tikzDevice** package (i.e., when `dev='tikz'`) and it can save time for LaTeX compilation
- `sanitize`: (`FALSE`; character) whether to sanitize tikz graphics (escape special LaTeX characters); see documentation in the **tikzDevice** package

Note any number of plots can be recorded in a single code chunk, and this package does not need to know how many plots are in a chunk in advance -- it can figure out automatically, and name these images as `fig.path-label-i` where `i` is incremental from 1; if a code chunk does not actually produce any plots, **knitr** will not record anything either (the graphics device is open *only when plots are really produced*); in other words, it does not matter if `fig.keep='high'` but no plots were produced.

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

Normally this produces 2 plots in the output (i.e. when `fig.keep='high'`); for `fig.keep='none'`, no plots will be saved; for `fig.keep='all'`, 4 plots are saved; for `fig.keep='first'`, the plot produced by `plot(1)` is saved, and for `fig.keep='last'`, the last plot with 10 vertical lines is saved.

There are two hidden options which are not designed to be set by the users: `fig.cur` (the current figure number or index when there are multiple figures) and `fig.num` (the total number of figures in a chunk). The purpose of these two options is to help **knitr** deal with the filenames of multiple figures as well as animations. In some cases, we can make use of them to write animations into the output using plot files which are saved manually (see the [graphics manual](https://bitbucket.org/stat/knitr/downloads/knitr-graphics.pdf) for examples).

### Animation

- `interval`: (`1`; numeric) number of seconds to pause between animation frames
- `aniopts`: (`'controls,loop'`) extra options for animations; see the documentation of the [animate package](http://www.ctan.org/tex-archive/macros/latex/contrib/animate)

### Code Chunk

- `code`: (`NULL`; character) if provided, it will override the code in the
  current chunk; this allows us to programmatically insert code into the
  current chunk; e.g. a chunk option `code = capture.output(dump('fivenum',
  ''))` will use the source code of the function `fivenum` to replace the
  current chunk
- `ref.label`: (`NULL`; character) a character vector of labels of the
  chunks from which the code of the current chunk is inherited (see the demo
  for [chunk reference](/knitr/demo/reference/))

### Child Documents

- `child`: (`NULL`; character) a character vector of filenames of child documents to be run and input into the main document

### Language Engines

- `engine`: (`'R'`; character) the language name of the code chunk; currently other possible values are `'python'` and `'awk'`/`'gawk'`; the object `knit_engines` in this package can be used to set up engines for other languages

### Option templates

- `opts.label`: (`NULL`; character) the label of options set in `opts_template` (see `?opts_template`); this option can save some typing efforts for sets of frequently used chunk options

### Extracting source code

- `purl`: (`TRUE`; logical) when running `purl()` to extract source code from a source document, whether to include or exclude a certain code chunk

### Other Chunk Options

- `R.options`: (`NULL`) local R options for a code chunk; these options are set temporarily via `options()` before the code chunk, and restored after the chunk

## Package Options <a id="package_options"></a>

The package options can be changed using the object [`opts_knit`](objects); for example,

{% highlight r %}
opts_knit$set(progress = TRUE, verbose = TRUE)
{% endhighlight %}

See `?opts_knit` for the alternative approach to setting package options using the R base function `options()`.

All package options are:

- `animation.fun`: (`hook_ffmpeg_html`) a hook function to create animations in HTML output; the default hook uses FFmpeg to convert images to an MP4 video
- `aliases`: (`NULL`) a named character vector to specify the aliases of chunk options, e.g. `c(h = 'fig.height', w = 'fig.width')` tells **knitr** that the chunk option `h` really means `fig.height`, and `w` is an alias for `fig.width`; this option can be used to save some typing efforts for long option names
- `base.dir`: (`NULL`) an absolute directory under which the plots are generated
- `base.url`: (`NULL`) the base url for HTML pages
- `child.path`: (`''`) the search path for child documents; by default child documents are searched for relative to the directory of the parent document
- `concordance`: (`FALSE`) whether to write a concordance file to map the output line numbers to the input line numbers; this enables one to navigate from the output to the input and can be helpful especially when TeX error occurs (this feature is mainly for RStudio)
- `eval.after`: (`NULL`) a character vector of option names; these options will be evaluated _after_ a chunk is evaluated, and all other options will be evaluated before a chunk (e.g. for chunk option `fig.cap=paste('p-value is', t.test(x)$p.value)`, it will be evaluated after the chunk according to the value of `x` if `eval.after='fig.cap'`)
- `global.par`: (`FALSE`) if `TRUE`, the `par()` settings from the last code chunk will be preserved and applied to the next code chunk (of course, this only applies to base graphics); by default, **knitr** opens a new graphics device to record plots and close it after evaluating the code, so `par()` settings will be discarded
- `header`: the text to be inserted into the output document before the document begins (e.g. after `\documentclass{article}` in LaTeX, or `<head>` in HTML); this is useful for defining commands and styles in the LaTeX preamble or HTML header; the beginning of document is found using the pattern defined in `knit_patternss$get('document.begin')`
- `latex.options.color`, `latex.options.graphicx` (`NULL`): options for the LaTeX packages **color** and **graphicx**, respectively
- `out.format`: (`NULL`) possible values are `latex`, `sweave`, `html`, `markdown` and `jekyll`; it will be automatically determined based on the input file, and this option will affect which set of hooks to use (see `?render_latex` for example); note this option has to be set _before_ `knit()` runs (i.e. it does not work if you set it in the document), or alternatively, you can use the `render_*` series inside the document to set up the hooks
- `progress`: (`TRUE`) whether to display a progress bar when running **knitr**; note it also depends on the R option `KNITR_PROGRESS` (it this variable is set to `FALSE` via `options(KNITR_PROGRESS = FALSE)`, the `progress` option will be set to `FALSE` when **knitr** is loaded)
- `root.dir`: (`NULL`) the root directory when evaluating code chunks; if `NULL`, the directory of the input document will be used
- `self.contained`: (`TRUE`) whether the output document should be self-contained (TeX styles written in the tex document, and CSS styles in HTML document)
- `unnamed.chunk.label`: (`unnamed-chunk`) the label prefix for unnamed chunks
- `upload.fun`: (`identity`) a function that takes a filename as its input, processes it and returns a character string when the output format is HTML or Markdown; typically it is a function to upload a image and return the link to the image, e.g. `opts_knit$set(upload.fun = imgur_upload)` can upload a file to <http://imgur.com> (see `?imgur_upload`)
- `verbose`: (`FALSE`) whether to show verbose information (e.g., R code in each chunk and message logs) or just show chunk labels and options
- `width`: (`75`) it is used to set the R session option `width` that may affect the width of source code and text output

