# CHANGES IN knitr VERSION 0.7

## NEW FEATURES

- added a new chunk option `out.extra` to write extra graphics output options, e.g. `<<out.extra='angle=90'>>=` to rotate the figure by 90 degrees; see http://yihui.name/knitr/options (#301) (thanks, @knokknok)

- when `opts_knit$get('verbose')` is TRUE, logs (messages, warnings and errors) along with the corresponding R code will be printed after `knit()` is done; this might help users figure out possible problems in R code quickly (#276)

- `.Random.seed` is cached again for the sake of reproducibility; see http://yihui.name/knitr/demo/cache/ for how to maintain reproducibility when the computation involves with random number generation (#274) (thanks, Renaud)

- the package option `opts_knit$get('cache.extra')` can be an unevaluated R expression now, e.g. `opts_knit$set(cache.extra = quote(.Random.seed))`; see the cache page above for a concrete example

- added a new package option `'root.dir'` (default `NULL`) which can be used to set the root directory to evaluate code chunks in a document; by default, the root directory is the directory of the input document, and this option enables users to set other directories as the working directory for code chunks (#277) (thanks, Ken Williams)

- `spin()` will add `\documentclass{article}` for Rnw output if no document class is specified in the R script so that the LaTeX output will be a complete document (#295) (thanks, Christiaan Klijn)

- added Ruby support in the `engine` option; see the example https://github.com/yihui/knitr/blob/master/inst/examples/knitr-lang.Rmd (#294) (thanks, Ramnath Vaidyanathan)

- also added Haskell support in the option `engine='haskell'` through calling `ghc` (#336) (thanks, Michel Kuhlmann)

- added support to Andre Simon's `highlight` through the option `engine='highlight'`; see https://gist.github.com/3114112 for an example of highlighting Matlab code in LaTeX (thanks, Dirk Eddelbuettel and Renaud Gaujoux)

- the output hooks for LaTeX, HTML, Markdown and reST will respect the `engine` option now, so these hooks can be readily used for output when the language is not R, e.g. `render_markdown(strict = TRUE)` also works for Python output (#251) (thanks, Chris Fonnesbeck)

- the chunk options `eval`, `echo` and `results` are also respected when the language is not R, e.g. for a Python code chunk with `eval=FALSE`, the code will not be evaluated, or for a Ruby chunk with `results='hide'`, the output will be hidden (#293) (thanks, Ramnath Vaidyanathan)

- chunk options `out.width`, `out.height` and `out.extra` also work for plots in HTML and Markdown output now, e.g. `out.width='200px'` or `out.extra='style="display:block;"'` (#297) (thanks, Taiyun Wei and Alan Severini)

- the hook function to create animations in HTML output is exported now as `hook_ffmpeg_html()`

- added a package option `opts_knit$get('animation.fun')` which defaults to `hook_ffmpeg_html`; this option is used to create animations in HTML output so that we do not have to use FFmpeg

- added two functions `hook_r2swf()` and `hook_scianimator()` which can be set as the package option `opts_knit$get('animation.fun')` and create animations via the **R2SWF** package or the **SciAnimator** library (see `animation::saveHTML`) (thanks, Taiyun Wei)

- a new function `image_uri()` to create data URIs for image files; we can set `opts_knit$set(upload.fun = image_uri)` so that images are embedded in the HTML output as data URIs (hence the HTML page does not depend on external images) (#298, #324) (thanks, Wush Wu)

- added a new object `opts_template` which can be used to set a group of chunk options and they can be referenced later with the new chunk option `opts.label`; see `?opts_template` for examples; this makes it easy to reuse groups of frequently used chunk options (#316, #320) (thanks, Cassio Pereira)

- a new function `dep_prev()` to build chunk cache dependencies so that all later chunks will depend on previous chunks; if any of a previous chunk is updated, the cache of all chunks after it will be updated as well (#285) (thanks, @muelleki)

- a new chunk hook function `hook_optipng()` to optimize PNG images using `optipng` (#272) (thanks, Winston Chang)

- added a new output hook named `document` in `knit_hooks` (see `knit_hooks$get('document')`); this hook function is used to process the output of the whole document; it can be useful when we want to post-process the whote output before writing it to the output file

- a new function `rst2pdf()` which uses the program `rst2pdf` to convert reST documents to PDF; it is also supported by `knit2pdf()` when `compiler='rst2pdf'` (#300) (thanks, Alex Zvoleff)

## BUG FIXES

- fixed #286: messages (including warnings and errors) are guaranteed to be ended by `\n`, so even when chunk option `comment=NA`, messages will also be rendered correctly (thanks, Carl Boettiger)

- fixed #273: when knitting a file under another directory with cache turned on (e.g. `knit('foo/bar.Rnw')`), `lazyLoad()` is unable to load the cache files under a relative path because the working directory has been changed to the directory of the input file during evaluation

- fixed #292: layout() may cause the capture of unwanted plots (thanks, Austen Wallace Head)

- fixed #302: when there are multiple instances of `\documentclass` in the source document, **knitr** will be unable to insert the LaTeX preamble

- fixed #308: if `options('OutDec')` was set to a character other than `.`, the LaTeX code can be malformed (#308) (thanks, Cassio Pereira)

- `opts_chunk$set()` in a child document was only working in that child document, but was expected to change chunk options globally; now it works everywhere, and will affect all chunks after this setting, no matter where `opts_chunk$set()` is (thanks, Guy Lebanon) (http://bit.ly/MexHXd)

- fixed #332: calling `purl()` inside a source document when `knit()` the same document could cause clashes; now it is safe to put `purl()` inside a source document and `knit()` it

## MAJOR CHANGES

- if the chunk label contains non-alphanumeric characters (except `-` and `_`), these characters will be replaced by `_` in the figure filenames (if there are any) and a warning will be issued; this is to guarantee the figure filenames are valid to LaTeX (#321) (thanks, (Roman Lustrik)

- the [**highlight**](http://cran.r-project.org/package=highlight) package is not enabled by default; use `opts_knit$set(use.highlight = TRUE)` to enable it

- the default LaTeX output will put messages, warnings and errors in special LaTeX environments: errors are red, warnings are magenta, and messages are italic; in previous versions they were in the `verbatim` environment (#264) (thanks, @muelleki)

- unnamed chunks are named sequentially in a single call of `knit()` according to the order of their appearance no matter where they are, e.g. if there are two unnamed chunks in two child documents respectively, they will be named as `unnamed-chunk-1` and `unnamed-chunk-2`; in previous versions, both will be named as `unnamed-chunk-1` which can cause clashes of cache and figure files

- the function `build_dep()` was renamed to `dep_auto()` which better reflects what this function really does; it is still available in this package but may be removed in future versions

- the package **tikzDevice** was removed from the `Suggests` field, but this will not affect users who have already installed **tikzDevice**; for those who have not, this package has been archived on CRAN (hopefully only temporarily), so you have to install from the source

## MINOR CHANGES

- the LaTeX environment `kframe` was updated so that it can be used in other environments such as `center` or `tabular` (#283) (thanks, @muelleki)

- the OpenCPU demo is more robust to double quotes in the text (#271); see http://public.opencpu.org/userapps/opencpu/knitr/

- for Sweave output, the results will not be put inside the Schunk environment when `results='asis'`

- `stitch()` templates use more sensible figure paths by default: the path for figures is `'figure/input-script-name'` now, i.e. it will be different for different input R scripts to avoid possible clashes

- `stitch()` templates no longer use default title and author names; if the user did not set them in the R script (as meta comments `# title:` and `# author:`), there will not be titles or authors in the output

- **knitr** will no longer use scientific notations for integers in inline R code output; sci notation only applies to double-precision numbers (#296) (thanks, @knokknok)

- `options()` set in the main document will apply to its child documents (e.g. `options('digits')`) (#306) (thanks, Cassio Pereira)

- the `...` argument in `knit2html()` is passed to `markdownToHTML()` in the **markdown** package instead of `knit()`; this allows us to pass more arguments to control the rendering of HTML output, e.g. `knit2html(input, fragment.only = TRUE)` (#333) (thanks, @Bart6114)

## DOCUMENTATION

- the example using other languages was updated to show how some chunk options (`eval`, `echo` and `results`) also work for foreign languages: https://github.com/yihui/knitr/blob/master/inst/examples/knitr-lang.Rmd

# CHANGES IN knitr VERSION 0.6.3

## MAJOR CHANGES

- this is a urgent patch version for CRAN: the dependencies on **highlight** and **parser** were removed because these two package were orphaned on CRAN; now **knitr** uses a naive syntax highlighter for LateX and HTML output if **highlight** is not available, which has a similar appearance with **highlight**; when the **parser** package is not available, users should be careful with the chunk option `tidy=TRUE`: `replace.assign` may not work as expected; see the NEWS of **formatR** for details: https://github.com/yihui/formatR/blob/master/NEWS; you are welcome to improve the naive highlighter: https://github.com/yihui/knitr/tree/master/R/highlight.R

## NEW FEATURES

- `spin()` gained a new argument `report` for Rnw/Rtex/Rmd documents to be compiled to PDF/HTML respectively (#287) (thanks, kohske takahashi)

## BUG FIXES

- `stitch()` and `spin()` should use `parent.frame()` to evaluate code by default; in 0.6, the default parent frame for the inner `knit()` was a wrong environment to use (thanks, Michael Nelson)

- use `unnamed-chunk-i` as the label when chunk `label == ''` (#280) (thanks, Josh Paulson)

- fixed #279 and #281, both of which are about concordance

- `'\\maxwidth'` does not apply to LaTeX/PDF animations, so the default value of `out.width` for animations in LaTeX is still `NULL`; you will have to set `out.width` manually for a reasonable width (#282) (thanks, Ramnath Vaidyanathan)

## MISC

- **knitr** and RStudio had an invited talk at useR! 2012; slides at http://yihui.name/slides/2012-knitr-RStudio.html (source at https://github.com/yihui/knitr-talks)

# CHANGES IN knitr VERSION 0.6

## NEW FEATURES

- for LaTeX output, the chunk option `out.width` defaults to `'\\maxwidth'` now; see http://yihui.name/knitr/demo/framed/ for its definition; this makes sure the figures do not overflow the page margins (#221)

- the chunk option `size` now defines the font size of the whole chunk instead of only some special characters in it, so that the old trick of redefining the `knitrout` environment for different font sizes is no longer necessary; see updates in the beamer examples: http://yihui.name/knitr/demo/beamer/ (thanks, Baptiste Auguie)

- added a new chunk option `dev.args` which can be used to pass more arguments to the graphical device (#254) (thanks, knokknok)

- warnings, messages and errors will be wrapped according to `options('width')`; this will make long messages stay within the page margin when the width option is appropriately small (#259) (thanks, @muelleki)

- added a new function `spin()` to turn a specially formatted R script to a report; see http://yihui.name/knitr/demo/stitch/ (#223) (thanks, Rich FitzJohn)

- `knit()` gained a new argument `envir` to specify the environment in which to evaluate code chunks (by default in the parent frame); this will allow users to run code in a specified environment so that the global environment will not be cluttered by objects in chunks, which can hopefully make the documents more reproducible (#228)

- added a new component `inline.comment` in `knit_patterns` to strip off tokens of inline R code from inline comments, e.g. a comment line like `% x is \Sexpr{x}` will become `% x is x` (#110); this will only happen to LaTeX documents because HTML does not have inline comments (it only has block comments `<!-- -->`)

- concordance works for child documents as well now (try RStudio), although it is not very precise (#225); note when concordance is enabled, the results from child documents will be written into the parent output instead of individual tex files; also note you have to set `opts_knit$set(self.contained = FALSE)` for concordance to work better

- added an OpenCPU app so that we can run knitr in the cloud now; see `system.file('opencpu', 'apps', 'index.html', package = 'knitr')` or http://public.opencpu.org/apps/knitr (thanks, Jeroen Ooms)

- all messages, errors and warnings during evaluation are recorded in an internal object `knit_log` (use `knitr:::knit_log$get()` to get all messages) and they are printed if the package option `verbose` is `TRUE` (i.e. `opts_knit$get('verbose')`) (#224)

- child documents are also supported in other document formats besides LaTeX, e.g. Markdown and HTML, etc; please use the chunk option `child` to specify the child documents; see `system.file('examples', 'child', 'knitr-main.Rmd', package = 'knitr')` for an example in markdown (#268) (thanks, @martinaryee)

## BUG FIXES

- the templates for `stitch()` used `results=hide` which should really be `results='hide'` (#219) (thanks, @r2d3)

- format numbers with the reST syntax instead of HTML (#218) (thanks, Jeffrey Arnold)

- `hook_pdfcrop()` should work better under Windows now (#209) (thanks @DCCKC and @r2d3)

- tikz compilation fails on Windows network drives

- FFmpeg does not really work for HTML/Markdown output because the dot in figure filenames was omitted (thanks, Ming Kuo)

- child documents can fail when they are in different sub directories (thanks, Christoph J)

- `set_parent()` failed to work in the last version due to a bug when inserting the parent preamble into the child document (#240)

- better preserve plot sizes in interactive sessions (#258)

## MAJOR CHANGES

- `.Random.seed` is not cached any more because of weird problems due to lazy loading (#248 and #253); users should use `set.seed()` to make sure reproducibility of random simulations; the chunk output is cached in a `.RData` database instead of a lazy load database to avoid problems in #253

- the default graphics device is set to the null PDF device before evaluating code chunks, i.e. `pdf(file = NULL)`, so that neither `Rplots.pdf` nor plot windows will be opened during knitting
  
## MINOR CHANGES

- **knitr** will show a message when a chunk is empty; this helps users who do not actually want a chunk to be empty realize the problem like #229; in the past, knitr just silently returns an empty string from such chunks

- **knitr** will show a message when the cache is loaded and the option `opts_knit$get('verbose')` is `TRUE` (#249) (thanks, Carl Boettiger)

- the filename extensions `Snw` and `Stex` are also supported (`foo.Snw`/`foo.Stex` produces `foo.tex`)

- the HTML output hooks are changed according to the suggestion of Barry Rowlingson (#250) and the default CSS is also slightly modified

- `knit()` will no longer try to remove the file `NA` generated by `pdf(file = NULL)` before R 2.14.2 (which was a bug and fixed later); you should update R if you see this file

## DOCUMENTATION

- added a minimal brew example under `system.file('examples', package = 'knitr')`
  
# CHANGES IN knitr VERSION 0.5

## NEW FEATURES

- white spaces are allowed before `<<>>` when using chunk references, and this approach of references is supported in tex and html documents as well

- added a new pattern list named `md` so that R code blocks can be written more naturally in extended markdown (e.g. GFM and pandoc): use ```` ```{r label, opts}```` to begin a chunk and ```` ``` ```` (three or more backticks) to begin normal text, and write inline R code in `` `r code.here` ``; it is the default syntax for markdown input, or you can call `pat_md()` before `knit()` so **knitr** can make use of this pattern list to process the input document

- RStudio has added full support to **knitr**: we can knit HTML and Markdown documents easily now, and markdown output will be automatically converted to an HTML preview page just like TeX to PDF

- if the pattern list is not set in advance, **knitr** will try to detect the syntax automatically by matching all built-in pattern lists against the input document, e.g. if the input contains `<<>>=`, the Rnw syntax will be used, and if ```` ```{r} ```` is detected, the markdown syntax will be used; this feature enables us to use different sets of syntax freely, e.g. we can use Sweave syntax in markdown files and knitr will be able to recognize it (#189)

- new filename extensions with a prefix `R` or `r` are introduced: `*.Rhtml`, `*.Rhtm`, `*.Rtex`, `*.Rmd` and `*.Rmarkdown` will be recognized as source documents like `*.Rnw`, and the output filename will be automatically determined by removing the `R` prefix, e.g. `foo.Rmd` produces `foo.md` by default; the old clumsy naming convention `foo_knit_.md` is still preserved but not recommended any more (#187)

- new function `knit2html()` to knit an Rmd file (R with markdown) and convert to HTML in one step using the **markdown** package

- new functions `pat_rst()` and `render_rst()` to support reStructuredText; use `.. {r label, options}` and `.. ..` to write R code chunks; see https://github.com/yihui/knitr/tree/master/inst/examples/knitr-minimal.Rrst (thanks, Jeffrey Arnold and Ramnath Vaidyanathan)

- new package option `self.contained` which decides whether to write style definitions (highlighting) in external files or put them in the output document; the highlighting definitions in LaTeX output is often too long, so `opts_knit$set(self.contained = FALSE)` can help in this case (#176) (thanks, Ramnath Vaidyanathan)

- new package option `filter.chunk.end` which decides if the `chunk.end` pattern really means `chunk.end`; see http://yihui.name/knitr/options (thanks, Joe Cheng)

- syntax highlighting themes are also available to HTML output now; the usage is the same as in LaTeX (#179) (thanks, Ramnath Vaidyanathan)

- the chunk option `fig.cap` is also used in markdown output now

- the random seed `.Random.seed` is also cached for the sake of reprodubibility in random simulations

- the function call `read_chunk()` will be evaluated when tangling R code via `purl()` (#175) (thanks, Carl Boettiger)

- the default LaTeX output will use the **upquote** package if it exists so that single quotes are straight in the output (thanks, Mathematical Coffee http://bit.ly/IKjluw)

- the chunk option `engine` is back but it is used to run code from other languages instead of just ignoring the code in chunks which have `engine != 'R'`; currently this option is still in rough edges and only supports python and awk; other languages may be added in future versions, but users can also do it by themselves by `knit_engines$set(language = function(options) {...})`; see an example at `system.file('examples', 'knitr-lang.Rmd')` (#201)

- new function `write_bib()` to write Bibtex citations for R packages; see the main manual for a sample usage (#13)

- `hook_pdfcrop()` also supports cropping other image formats like PNG or JPEG through ImageMagick (`convert -trim`) (#203) (thanks, @r2d3)

## MAJOR CHANGES

- **knitr** will completely stop when duplicated labels are found and the corresponding chunks are non-empty; in previous version, only a warning is given and R code in later chunks will override previous chunks (#185) (thanks, Randall Pruim)

- the default graphical device for HTML and markdown output is `png()` now to avoid the possible unexpected consequence that PDF plots are included in an HTML page which will not work normally in browsers

- markdown output will use the extended markdown hooks by default now: `render_markdown(strict = FALSE)`; in the past the default was `render_jekyll()`; the major difference is that the code blocks are put in ```` ```r and ``` ````; if you want the strict markdown syntax, you can all `render_markdown(strict = TRUE)` which will indent code blocks by 4 spaces

- `render_gfm()` has been removed because the name can be misleading; the main purpose of this function was to put code blocks in ```` ``` ````, and now we can replace it by `render_markdown(FALSE)`; other markdown flavors also support such fenced code blocks (e.g. pandoc) -- it is not limited to Github only

- the default value for the `fig.path` option is `'figure/'` now so that plots will be put under this directory by default; the default was `'./'` in the past which makes the current directory messy when there are many plots

- **knitr** will fully stop when an error is encountered in `knit()`; in the past only a message was issued in this case in an interactive R session

- the package option `all.patterns` has been dropped; please use the objects `all_patterns` or `knit_patterns` directly if you want to tweak the syntax

## BUG FIXES

- the compilation of tikz graphics can hang up when there are TeX errors in the tikz file; now we use `\nonstopmode` to avoid hiccup (#188)

- multiple devices per chunk was buggy (#181)

- S4 objects will be printed by `show()` instead of `print()`; this is a bug of the **evaluate** package, which has been fixed (please update it to be > 0.4.2)

## MISC

- it is recommended to use `opts_chunk$set()` to set global chunk options now instead of `\SweaveOpts{}`; all documentation has been updated (#216)

- number of downloads (https://github.com/yihui/knitr/downloads) of **knitr** documentation before I removed and updated them on GitHub: c(main = ?, graphics = 275+)

# CHANGES IN knitr VERSION 0.4

## NEW FEATURES

- Sweave concordance was finally implemented: when `opts_knit$get('concordance')` is `TRUE`, **knitr** will write a file named `'input-concordance.tex'` which contains the mapping between input Rnw and output tex line numbers; this feature is mainly for (but not limited to) RStudio to provide better error navigations: you can jump from the TeX error message to the Rnw source directly to know where the error comes from (the line number of the source of the error may not be accurate but should be in the ballpark) (#133) (thanks, JJ Allaire and Joe Cheng)

- if output hooks have been set before calling `knit()`, they will be respected, i.e. **knitr** will no longer override them by default hooks; you need to make sure *all* output hooks are set appropriately, e.g. you can start by `render_latex()` and change some individual hooks later (#165) (thanks, Andrew Redd)

- newly created objects in the global environment will also be cached if cache is turned on (`cache=TRUE`); in previous versions **knitr** is unaware of objects created in `globalenv()`, e.g. `setGeneric()` creates S4 generic functions in `globalenv()` and **knitr** was unable to capture them (#138) (thanks, syoh)

- chunk options `dev`, `fig.ext` and `dpi` can be vectors now; this allows one to save a plot to multiple formats, e.g. `<<foo, dev=c('pdf', 'png')>>=` creates two files for the same plot: `foo.pdf` and `foo.png` (#168) (thanks, MYaseen208)

- an experimental feature for animations created by FFmpeg in HTML/markdown output when `fig.show='animate'` (#166) (thanks, gabysbrain)
  
- the chunk option `fig.cap` supports multiple figure captions in LaTeX now, e.g. if a chunk produces two plots, we can use `fig.cap = c('first caption', 'second caption')` to assign two different captions to them respectively when `fig.show = 'asis'` (#155) (thanks, Jonathan Kennel)

- new package option `opts_knit$get('upload.fun')` which is a function that takes a plot file to upload to a certain host and returns the link to the image; by default it is `imgur_upload()`, and you can use your own function to upload images to other hosts like Flickr (#159) (thanks, Carl Boettiger)

- all packages loaded in the current session are also cached, so as long as a package has been loaded previously, it will be available to all following chunks (#160)

- new chunk option `autodep` and function `build_dep()` to build cache dependencies among cached chunks automatically by analyzing object names in all cached chunks; this is a loose alternative to the `dependson` option (see main manual and `?build_dep` for details) (#72) (thanks, Seth Falcon)

- input and output in `knit()` are no longer restricted to files; they can be `stdin()`/`stdout()` or other types of connections (#162; see https://github.com/yihui/knitr/issues/162) (thanks, gabysbrain)

- as-is output (`results='asis'`) and plots are no longer put in the framed environments because of incompatibilities (#163) (thanks, DCCKC, Murray Logan and Jennym Hutchison)

## BUG FIXES

- for plots in LaTeX output, centering should be done with `{\centering }` instead of `\centering{}` (#156) (thanks, Ramnath Vaidyanathan)

- the recorded plot is a more precise representation of the expected plot now, because the recording device also takes the plot size into consideration (#157) (thanks, Christiaan Klijn and Frank Harrell)

- `format_sci()` now correctly formats 0; this function is used for inline R code to format numbers in scientific notation (#161) (thanks, Kihoro J. M.)

- fixed a bug for the case in which the chunk option only contains the label like `<<label=foobar>>=`; knitr 0.3 was unable to parse the label correctly (`<<foobar>>=` is OK) (thanks, Muhammad Yaseen)

## MINOR CHANGES

- `imgur_upload()` returns the link to the image directly, with the XML list as its attribute (in v0.3 the list was returned)

- more verbose messages in case of chunk errors: both line numbers of the source and chunk info will be printed

## DOCUMENTATION

- website updated as usual: http://yihui.name/knitr

- added an example for subfloat environment: https://github.com/downloads/yihui/knitr/knitr-subfloats.pdf

- most manuals (main or graphics manuals) have been updated

## MISC

- number of downloads (https://github.com/yihui/knitr/downloads) of knitr documentation before I removed and updated them on GitHub: c(main = 400, graphics = 177)

# CHANGES IN knitr VERSION 0.3

## NEW FEATURES

- a fundamental and important new feature for writing chunk options: they can be written as valid R code now, just like we write function arguments (e.g. `echo=c(1, 3, 5)`, `fig.cap="my figure caption"`); all options will be parsed and evaluated as R code by default; see http://yihui.name/knitr/options for details (#142) (thanks, Baptiste Auguie)

- chunk references using `<<label>>` is supported now (#86); thanks to Kevin R. Coombe and Terry Therneau for the discussion

- new function `run_chunk()` to run the code in a specified chunk, which is an alternative to the chunk reference in Sweave; see http://yihui.name/knitr/demo/reference/

- a executable script `knit` under `system.files('bin', package = 'knitr')` which makes it easier to call knitr via command line under *nix (call `knit input [output] [--pdf]`)

- the inline hooks respect `getOption('digits')` and `getOption('scipen')` now (see `?options`); numbers returned from inline R code will be formatted according to these two options (see a demo at http://yihui.name/knitr/demo/output/)

- if you still use old Sweave syntax for chunk options, it is possible to write literal commas in chunk options now -- they have to be escaped by `\`, e.g. `caption=hello\, world`; this will be parsed to `'hello, world'` as a character string; of course this looks ugly and has limited power, so please please consider the new syntax!

- `knit2pdf()` gained another argument `compiler` which can be used to specify the program to compile the tex document to PDF, such as xelatex (#131) (thanks, Ramnath Vaidyanathan and Dennis Murphy)

- a new function `imgur_upload()` to upload images to imgur.com; it can be used in HTML or Markdown hooks so the output is a self-contained document which does not need additional image files; `opts_knit$get('upload.fun'`) can use this function (#66) (thanks, Ramnath Vaidyanathan)

- a child document can be compiled individually with the LaTeX preamble borrowed automatically from a parent document using a new function `set_parent()`; see the help page for details (#136) (thanks, Helder Correia)

- to avoid `$$` around numbers in the inline output, we can use `I()` to protect the numeric inline output, e.g. `$x = \Sexpr{I(10^7)}$` gives `$x = 10^7$` whereas `\Sexpr{10^7}` gives `$10^7$` (thanks, Kevin Middleton)

- the listings package is formally supported now (see `?render_listings`); the default style is borrowed from `Sweavel.sty` written by Frank Harrell (#101) (thanks, Frank)

- new package option `cache.extra` which allows more objects to affect cache; see http://yihui.name/knitr/demo/cache/ (#134)

- new package option `child.path` to specify the search path of child documents relative to the parent document (#141)

- new package option `aliases` to set aliases for chunk options; see http://yihui.name/knitr/options (#144)

- new chunk options `fig.cap`, `fig.scap` and `fig.lp` to write captions, short captions, label prefix for the figure environment in LaTeX (#145) (thanks, Frank Harrell)

- new package option `eval.after` to set a character vector of chunk options which should be evaluated _after_ a chunk is executed (thanks, Frank Harrell)

- a series of convenience functions `pat_rnw()`, `pat_tex()`, `pat_brew()` and `pat_html()` to set built-in patterns (syntax) to read input

## MINOR CHANGES

- package option `eval.opts` has been dropped: all options of classes `symbol` or `language` will be evaluated, so there is no need to specify which options to evaluate manually; remember, the chunk options are similar to function arguments, so you can use any valid R code there
  
- the default value for the `output` argument in `knit()` is NULL now, so we can also provide output filenames to `stitch()` and `knit2pdf()` (#119)

- standard LaTeX messages are suppressed when a tikz plot is compiled to PDF so that we can see the **knitr** process more clearly

- `%\SweaveInput{}` will be ignored now (#150)

- `results=asis` will no longer affect the `chunk` hook (in the past, the chunk output was not wrapped in the `kframe` environment when `results=asis`); it only affects the `output` hook now

- the package website allows comments now

## MAJOR CHANGES

- the starting pattern of normal texts in an Rnw document is `^@\\s*%*` instead of `^@\\s*$` now, meaning you can write `@ % a comment` to end a code chunk (this is consistent with Sweave)

- the default value of the argument `output` of `knit()` will be a filename under the current working directory; in previous versions, the output file will be under the same directory as the input file; this change makes it possible to completely separate the input files and output files into different places, and hopefully will give users better experience in managing a whole collection of files (including child documents): put all source files in one place and output files in another place

- the package homepage is http://yihui.name/knitr now (the previous URL yihui.github.com/knitr will be automatically redirected to the new address)

## BUG FIXES

- the object `opts_current` does not give the evaluated version of the current chunk options because it was created before the options are evaluated; this has been fixed and `opts_current$get()` will give the expected values of options (thanks, Frank Harrell)

## MISC

- number of downloads (https://github.com/yihui/knitr/downloads) of knitr documentation before I removed and updated them on GitHub: c(main = 1300, graphics = 549, themes = 130, beamer = 565, listings = 240, minimal = 160)

# CHANGES IN knitr VERSION 0.2

## NEW FEATURES

- added support for including child documents in a main document (like `\SweaveInput{}` but with different implementations); see http://yihui.name/knitr/demo/child/ (#92)

- for inline R code, character results are returned as-is now (without `\texttt{}`)

- new function `purl()` as a wrapper to `knit(..., tangle = TRUE)` which extracts R code from the input document (thanks to Dieter Menne's wife who suggested the function name)

- the error hook applies to inline R code when an error occurs in the inline R code, in which case knitr will not stop by default; instead, it writes the error message into the output (#85)

- chunk option `split` also works for HTML output now using `<iframe></iframe>` (#82)

- `knit()` gained an argument `text` as an alternative to `input` (#88)

- new chunk option `child` to include child documents into the main document (#92)

- chunk option `external` defaults to `TRUE` now (was `FALSE` by default in the last version)

- added a new demo to show how to build package vignettes with knitr: http://yihui.name/knitr/demo/vignette/

- added support to the `quartz()` device under Mac (#103); now the `dev` option has more choices (see http://yihui.name/knitr/options)

- chunk option `echo` can take a numeric vector to select which R expressions to echo into the output now (#108); see http://yihui.name/knitr/options

- a new function `stitch()` which is a convenience function to insert an R script into a template and compile (to quickly create a report
  based on an R script)

- for a chunk hook to run, the corresponding chunk option no longer has to be `TRUE`; it can be any non-null values; this enables us to make use of the option value directly instead of only knowing it is `TRUE` (see http://yihui.name/knitr/demo/cache/ for an example)

- `knit()` will no longer writes figure or cache files in the same directory as the input document; instead, these files are written in the current working directory (see ?knit)

- a new function `knit_env()` that makes the environment of the current chunk accessible to the user

## BUG FIXES

- the code used to merge global chunk options and local options was buggy for cache; it has been fixed now, so cache is more stable (#105), but users may see previously cached chunks being re-evaluated with this version, which should be regarded as a normal phenomenon, and on the second run, the cached chunks will not be evaluated again

- fixed a buglet when using both options `out.width` and `out.height` in Rnw (#113)

  
# CHANGES IN knitr VERSION 0.1

## NEW FEATURES
		
- first version of knitr: it covers most features in Sweave, **cacheSweave** and **pgfSweave**; see package homepage for documentation and examples: http://yihui.name/knitr/

## MISC

- **knitr** won an Honorable Mention prize (before it was formally released to CRAN) in the Applications of R in Business Contest hosted by Revolution Analytics: http://bit.ly/wP1Dii http://bit.ly/wDRCPV
  
- in this NEWS file, #n means the issue number on GitHub, e.g. #142 is https://github.com/yihui/knitr/issues/142

