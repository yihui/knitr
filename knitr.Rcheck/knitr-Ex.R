pkgname <- "knitr"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('knitr')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("Sweave2knitr")
### * Sweave2knitr

flush(stderr()); flush(stdout())

### Name: Sweave2knitr
### Title: Convert Sweave to knitr documents
### Aliases: Sweave2knitr

### ** Examples

Sweave2knitr(text='<<echo=TRUE>>=') # this is valid
Sweave2knitr(text="<<png=true>>=") # dev='png'
Sweave2knitr(text="<<eps=TRUE, pdf=FALSE, results=tex, width=5, prefix.string=foo>>=")
Sweave2knitr(text="<<,png=false,fig=TRUE>>=")
Sweave2knitr(text="\\SweaveOpts{echo=false}")
Sweave2knitr(text="\\SweaveInput{hello.Rnw}")
# Sweave example in utils
testfile = system.file("Sweave", "Sweave-test-1.Rnw", package = "utils")
Sweave2knitr(testfile, output = 'Sweave-test-knitr.Rnw')
if (interactive()) knit('Sweave-test-knitr.Rnw') # or knit2pdf() directly
unlink('Sweave-test-knitr.Rnw')



cleanEx()
nameEx("all_labels")
### * all_labels

flush(stderr()); flush(stdout())

### Name: all_labels
### Title: Get all chunk labels in a document
### Aliases: all_labels all_rcpp_labels

### ** Examples

# the examples below are meaningless unless you put them in a knitr document
all_labels()
all_labels(engine == 'Rcpp')
all_labels(echo == FALSE && results != 'hide')
# or separate the two conditions
all_labels(echo == FALSE, results != 'hide')



cleanEx()
nameEx("all_patterns")
### * all_patterns

flush(stderr()); flush(stdout())

### Name: all_patterns
### Title: All built-in patterns
### Aliases: all_patterns
### Keywords: datasets

### ** Examples

all_patterns$rnw; all_patterns$html

str(all_patterns)



cleanEx()
nameEx("asis_output")
### * asis_output

flush(stderr()); flush(stdout())

### Name: asis_output
### Title: Mark an R object with a special class
### Aliases: asis_output

### ** Examples

 # see ?knit_print



cleanEx()
nameEx("chunk_hook")
### * chunk_hook

flush(stderr()); flush(stdout())

### Name: hook_pdfcrop
### Title: Built-in chunk hooks to extend knitr
### Aliases: hook_pdfcrop hook_optipng hook_pngquant hook_mogrify
###   hook_plot_custom hook_purl

### ** Examples

if (require('rgl') && exists('hook_rgl')) knit_hooks$set(rgl = hook_rgl)
# then in code chunks, use the option rgl=TRUE



cleanEx()
nameEx("convert_chunk_header")
### * convert_chunk_header

flush(stderr()); flush(stdout())

### Name: convert_chunk_header
### Title: Convert the in-header chunk option syntax to the in-body syntax
### Aliases: convert_chunk_header

### ** Examples

knitr_example = function(...) system.file('examples', ..., package = 'knitr')
# Convert a document for multiline type
convert_chunk_header(knitr_example('knitr-minimal.Rmd'))
# Convert a document for wrap type
convert_chunk_header(knitr_example('knitr-minimal.Rmd'), type = "wrap")
# Reduce default wrapping width
convert_chunk_header(knitr_example('knitr-minimal.Rmd'), type = "wrap", width = 0.6 * getOption('width'))
## Not run: 
##D # Explicitly name the output
##D convert_chunk_header('test.Rmd', output = 'test2.Rmd')
##D # Overwrite the input
##D convert_chunk_header('test.Rmd', output = identity)
##D # Use a custom function to name the output
##D convert_chunk_header('test.Rmd', output = \(f) sprintf('%s-new.%s', xfun::sans_ext(f), xfun::file_ext(f)))
## End(Not run)



cleanEx()
nameEx("download_image")
### * download_image

flush(stderr()); flush(stdout())

### Name: download_image
### Title: Download an image from the web and include it in a document
### Aliases: download_image

### ** Examples

## Don't show: 
if (interactive()) withAutoprint({ # examplesIf
## End(Don't show)
knitr::download_image('https://www.r-project.org/Rlogo.png')
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("engine_output")
### * engine_output

flush(stderr()); flush(stdout())

### Name: engine_output
### Title: An output wrapper for language engine output
### Aliases: engine_output

### ** Examples

library(knitr)
engine_output(opts_chunk$merge(list(engine = 'Rscript')), code = '1 + 1', out = '[1] 2')
engine_output(opts_chunk$merge(list(echo = FALSE, engine = 'Rscript')), code = '1 + 1', out = '[1] 2')

# expert use only
engine_output(opts_chunk$merge(list(engine = 'python')), out = list(structure(list(src = '1 + 1'), class = 'source'), '2'))



cleanEx()
nameEx("fig_chunk")
### * fig_chunk

flush(stderr()); flush(stdout())

### Name: fig_chunk
### Title: Obtain the figure filenames for a chunk
### Aliases: fig_chunk

### ** Examples

library(knitr)
fig_chunk('foo', 'png')
fig_chunk('foo', 'pdf')
fig_chunk('foo', 'svg', 2)  # the second plot of the chunk foo
fig_chunk('foo', 'png', 1:5)  # if the chunk foo produced 5 plots



cleanEx()
nameEx("fig_path")
### * fig_path

flush(stderr()); flush(stdout())

### Name: fig_path
### Title: Path for figure files
### Aliases: fig_path

### ** Examples

fig_path('.pdf', options = list(fig.path='figure/abc-', label='first-plot'))
fig_path('.png', list(fig.path='foo-', label='bar'), 1:10)



cleanEx()
nameEx("hook_document")
### * hook_document

flush(stderr()); flush(stdout())

### Name: hook_movecode
### Title: Some potentially useful document hooks
### Aliases: hook_movecode

### ** Examples

## Not run: knit_hooks$set(document = hook_movecode)
# see example 103 at https://github.com/yihui/knitr-examples



cleanEx()
nameEx("hook_plot")
### * hook_plot

flush(stderr()); flush(stdout())

### Name: hook_plot_html
### Title: Default plot hooks for different output formats
### Aliases: hook_plot_html hook_plot_asciidoc hook_plot_tex hook_plot_md
###   hook_plot_rst hook_plot_textile hook_plot_typst

### ** Examples

# this is what happens for a chunk like this

# <<foo-bar-plot, dev='pdf', fig.align='right'>>=
hook_plot_tex('foo-bar-plot.pdf', opts_chunk$merge(list(fig.align='right')))

# <<bar, dev='tikz'>>=
hook_plot_tex('bar.tikz', opts_chunk$merge(list(dev='tikz')))

# <<foo, dev='pdf', fig.show='animate', interval=.1>>=

# 5 plots are generated in this chunk
hook_plot_tex('foo5.pdf', opts_chunk$merge(list(fig.show='animate',interval=.1,fig.cur=5, fig.num=5)))



cleanEx()
nameEx("image_uri")
### * image_uri

flush(stderr()); flush(stdout())

### Name: image_uri
### Title: Encode an image file to a data URI
### Aliases: image_uri

### ** Examples

uri = image_uri(file.path(R.home('doc'), 'html', 'logo.jpg'))
if (interactive()) {cat(sprintf('<img src="%s" />', uri), file = 'logo.html')
browseURL('logo.html') # you can check its HTML source
}



cleanEx()
nameEx("inline_expr")
### * inline_expr

flush(stderr()); flush(stdout())

### Name: inline_expr
### Title: Wrap code using the inline R expression syntax
### Aliases: inline_expr

### ** Examples

library(knitr)
inline_expr('1+1', 'rnw'); inline_expr('1+1', 'html'); inline_expr('1+1', 'md')



cleanEx()
nameEx("is_low_change")
### * is_low_change

flush(stderr()); flush(stdout())

### Name: is_low_change
### Title: Compare two recorded plots
### Aliases: is_low_change

### ** Examples

pdf(NULL)
dev.control('enable')  # enable plot recording
plot(1:10)
p1 = recordPlot()
abline(0, 1)  # add a line (a low-level change)
p2 = recordPlot()
plot(rnorm(100))
p3 = recordPlot()  # draw a completely new plot
dev.off()
knitr::is_low_change(p1, p2)  # true
knitr::is_low_change(p1, p3)  # false



cleanEx()
nameEx("kable")
### * kable

flush(stderr()); flush(stdout())

### Name: kable
### Title: Create tables in LaTeX, HTML, Markdown and reStructuredText
### Aliases: kable kables

### ** Examples

d1 = head(iris); d2 = head(mtcars)
# pipe tables by default
kable(d1)
kable(d2[, 1:5])
# no inner padding
kable(d2, format = 'pipe', padding = 0)
# more padding
kable(d2, format = 'pipe', padding = 2)
kable(d1, format = 'latex')
kable(d1, format = 'html')
kable(d1, format = 'latex', caption = 'Title of the table')
kable(d1, format = 'html', caption = 'Title of the table')
# use the booktabs package
kable(mtcars, format = 'latex', booktabs = TRUE)
# use the longtable package
kable(matrix(1000, ncol=5), format = 'latex', digits = 2, longtable = TRUE)
# change LaTeX default table environment
kable(d1, format = "latex", caption = "My table", table.envir='table*')
# add some table attributes
kable(d1, format = 'html', table.attr = 'id="mytable"')
# reST output
kable(d2, format = 'rst')
# no row names
kable(d2, format = 'rst', row.names = FALSE)
# Pandoc simple tables
kable(d2, format = 'simple', caption = 'Title of the table')
# format numbers using , as decimal point, and ' as thousands separator
x = as.data.frame(matrix(rnorm(60, 1e6, 1e4), 10))
kable(x, format.args = list(decimal.mark = ',', big.mark = "'"))
# save the value
x = kable(d2, format = 'html')
cat(x, sep = '\n')
# can also set options(knitr.table.format = 'html') so that the output is HTML

# multiple tables via either kable(list(x1, x2)) or kables(list(kable(x1), kable(x2)))
kable(list(d1, d2), caption = 'A tale of two tables')
kables(list(kable(d1, align = 'l'), kable(d2)), caption = 'A tale of two tables')



cleanEx()
nameEx("knit")
### * knit

flush(stderr()); flush(stdout())

### Name: knit
### Title: Knit a document
### Aliases: knit purl

### ** Examples

library(knitr)
(f = system.file('examples', 'knitr-minimal.Rnw', package = 'knitr'))
knit(f)  # compile to tex

purl(f)  # tangle R code
purl(f, documentation = 0)  # extract R code only
purl(f, documentation = 2)  # also include documentation

unlink(c('knitr-minimal.tex', 'knitr-minimal.R', 'figure'), recursive = TRUE)



cleanEx()
nameEx("knit2html")
### * knit2html

flush(stderr()); flush(stdout())

### Name: knit2html
### Title: Convert markdown to HTML using knit() and mark_html()
### Aliases: knit2html

### ** Examples

# a minimal example
writeLines(c("# hello markdown", '```{r hello-random, echo=TRUE}', 'rnorm(5)', '```'), 'test.Rmd')
knit2html('test.Rmd')
if (interactive()) browseURL('test.html')

unlink(c('test.Rmd', 'test.html', 'test.md'))



cleanEx()
nameEx("knit2pdf")
### * knit2pdf

flush(stderr()); flush(stdout())

### Name: knit2pdf
### Title: Convert Rnw or Rrst files to PDF
### Aliases: knit2pdf

### ** Examples

#' compile with xelatex
## knit2pdf(..., compiler = 'xelatex')

#' compile a reST file with rst2pdf
## knit2pdf(..., compiler = 'rst2pdf')

#' compile an Rtyp file with typst
## knit2pdf(..., compiler = 'typst')



cleanEx()
nameEx("knit2wp")
### * knit2wp

flush(stderr()); flush(stdout())

### Name: knit2wp
### Title: Knit an R Markdown document and post it to WordPress
### Aliases: knit2wp

### ** Examples

# see the reference



cleanEx()
nameEx("knit_child")
### * knit_child

flush(stderr()); flush(stdout())

### Name: knit_child
### Title: Knit a child document
### Aliases: knit_child

### ** Examples

# you can write \Sexpr{knit_child('child-doc.Rnw')} in an Rnw file 'main.Rnw'
# to input results from child-doc.Rnw in main.tex

# comment out the child doc by \Sexpr{knit_child('child-doc.Rnw', eval = FALSE)}



cleanEx()
nameEx("knit_engines")
### * knit_engines

flush(stderr()); flush(stdout())

### Name: knit_engines
### Title: Engines of other languages
### Aliases: knit_engines
### Keywords: datasets

### ** Examples

knit_engines$get('python'); knit_engines$get('awk')
names(knit_engines$get())



cleanEx()
nameEx("knit_exit")
### * knit_exit

flush(stderr()); flush(stdout())

### Name: knit_exit
### Title: Exit knitting early
### Aliases: knit_exit

### ** Examples

# see https://github.com/yihui/knitr-examples/blob/master/096-knit-exit.Rmd



cleanEx()
nameEx("knit_expand")
### * knit_expand

flush(stderr()); flush(stdout())

### Name: knit_expand
### Title: A simple macro preprocessor for templating purposes
### Aliases: knit_expand

### ** Examples

# see the knit_expand vignette
if (interactive()) browseVignettes(package='knitr')



cleanEx()
nameEx("knit_filter")
### * knit_filter

flush(stderr()); flush(stdout())

### Name: knit_filter
### Title: Spell check filter for source documents
### Aliases: knit_filter

### ** Examples

library(knitr)
knitr_example = function(...) system.file('examples', ..., package = 'knitr')



cleanEx()
nameEx("knit_hooks")
### * knit_hooks

flush(stderr()); flush(stdout())

### Name: knit_hooks
### Title: Hooks for R code chunks, inline R code and output
### Aliases: knit_hooks
### Keywords: datasets

### ** Examples

knit_hooks$get('source'); knit_hooks$get('inline')



cleanEx()
nameEx("knit_patterns")
### * knit_patterns

flush(stderr()); flush(stdout())

### Name: knit_patterns
### Title: Patterns to match and extract R code in a document
### Aliases: knit_patterns
### Keywords: datasets

### ** Examples

library(knitr)
opat = knit_patterns$get() # old pattern list (to restore later)

apats = all_patterns  # a list of all built-in patterns
str(apats)
knit_patterns$set(apats[['rnw']]) # set pattern list from apats

knit_patterns$get(c('chunk.begin', 'chunk.end', 'inline.code'))

# a customized pattern list; has to empty the original patterns first!
knit_patterns$restore()
# we may want to use this in an HTML document
knit_patterns$set(list(chunk.begin = '<!--helloR\\s+(.*)', chunk.end = '^byeR-->'))
str(knit_patterns$get())

knit_patterns$set(opat)  # put the old patterns back



cleanEx()
nameEx("knit_print")
### * knit_print

flush(stderr()); flush(stdout())

### Name: knit_print
### Title: A custom printing function
### Aliases: knit_print normal_print

### ** Examples

library(knitr)
# write tables for data frames
knit_print.data.frame = function(x, ...) {
  res = paste(c('', '', kable(x, output = FALSE)), collapse = '\n')
  asis_output(res)
}
# register the method
registerS3method("knit_print", "data.frame", knit_print.data.frame)
# after you define and register the above method, data frames will be printed
# as tables in knitr, which is different with the default print() behavior



cleanEx()
nameEx("knit_rd")
### * knit_rd

flush(stderr()); flush(stdout())

### Name: knit_rd
### Title: Knit package documentation
### Aliases: knit_rd knit_rd_all

### ** Examples

library(knitr)
## Not run: 
##D 
##D knit_rd('maps')
##D knit_rd('rpart')
##D setwd(system.file('html', package = 'ggplot2'))
##D knit_rd('ggplot2') # time-consuming!
##D 
##D knit_rd_all()  # this may take really long time if you have many packages installed
## End(Not run)



cleanEx()
nameEx("knit_theme")
### * knit_theme

flush(stderr()); flush(stdout())

### Name: knit_theme
### Title: Syntax highlighting themes
### Aliases: knit_theme
### Keywords: datasets

### ** Examples

opts_knit$set(out.format='latex'); knit_theme$set('edit-vim')

knit_theme$get()  # names of all available themes

thm = knit_theme$get('acid')  # parse the theme to a list
knit_theme$set(thm)

opts_knit$set(out.format=NULL) # restore option



cleanEx()
nameEx("knit_watch")
### * knit_watch

flush(stderr()); flush(stdout())

### Name: knit_watch
### Title: Watch an input file continuously and knit it when it is updated
### Aliases: knit_watch

### ** Examples

# knit_watch('foo.Rnw', knit2pdf)

# knit_watch('foo.Rmd', rmarkdown::render)



cleanEx()
nameEx("opts_chunk")
### * opts_chunk

flush(stderr()); flush(stdout())

### Name: opts_chunk
### Title: Default and current chunk options
### Aliases: opts_chunk opts_current
### Keywords: datasets

### ** Examples

opts_chunk$get('prompt'); opts_chunk$get('fig.keep')



cleanEx()
nameEx("opts_hooks")
### * opts_hooks

flush(stderr()); flush(stdout())

### Name: opts_hooks
### Title: Hooks for code chunk options
### Aliases: opts_hooks
### Keywords: datasets

### ** Examples

# make sure the figure width is no smaller than fig.height
opts_hooks$set(fig.width = function(options) {
  if (options$fig.width < options$fig.height) {
    options$fig.width = options$fig.height
  }
  options
})
# remove all hooks
opts_hooks$restore()



cleanEx()
nameEx("opts_knit")
### * opts_knit

flush(stderr()); flush(stdout())

### Name: opts_knit
### Title: Options for the knitr package
### Aliases: opts_knit
### Keywords: datasets

### ** Examples

opts_knit$get('verbose'); opts_knit$set(verbose = TRUE)  # change it
if (interactive()) {
# for unnamed chunks, use 'fig' as the figure prefix
opts_knit$set(unnamed.chunk.label='fig')
knit('001-minimal.Rmd') # from https://github.com/yihui/knitr-examples
}



cleanEx()
nameEx("opts_template")
### * opts_template

flush(stderr()); flush(stdout())

### Name: opts_template
### Title: Template for creating reusable chunk options
### Aliases: opts_template
### Keywords: datasets

### ** Examples

opts_template$set(myfigures = list(fig.height = 4, fig.width = 4))
# later you can reuse these chunk options by 'opts.label', e.g.

# <<foo, opts.label='myfigures'>>=

# the above is equivalent to <<foo, fig.height=4, fig.width=4>>=



cleanEx()
nameEx("output_hooks")
### * output_hooks

flush(stderr()); flush(stdout())

### Name: render_html
### Title: Set or get output hooks for different output formats
### Aliases: render_html hooks_html render_asciidoc hooks_asciidoc
###   render_latex hooks_latex render_sweave hooks_sweave render_listings
###   hooks_listings render_markdown hooks_markdown render_jekyll
###   hooks_jekyll render_rst hooks_rst render_textile hooks_textile
###   render_typst hooks_typst

### ** Examples

# below is pretty much what knitr::render_markdown() does:
knitr::knit_hooks$set(knitr::hooks_markdown())

# you can retrieve a subset of the hooks and set them, e.g.,
knitr::knit_hooks$set(knitr::hooks_markdown()["source"])

knitr::knit_hooks$restore()



cleanEx()
nameEx("output_type")
### * output_type

flush(stderr()); flush(stdout())

### Name: is_latex_output
### Title: Check the current input and output type
### Aliases: is_latex_output is_html_output pandoc_to pandoc_from

### ** Examples

# check for output formats type
knitr::is_latex_output()
knitr::is_html_output()
knitr::is_html_output(excludes = c('markdown', 'epub'))
# Get current formats
knitr::pandoc_from()
knitr::pandoc_to()
# Test if current output format is 'docx'
knitr::pandoc_to('docx')



cleanEx()
nameEx("pandoc")
### * pandoc

flush(stderr()); flush(stdout())

### Name: pandoc
### Title: A Pandoc wrapper to convert documents to other formats
### Aliases: pandoc

### ** Examples

system('pandoc -h') # see possible output formats



cleanEx()
nameEx("pat_fun")
### * pat_fun

flush(stderr()); flush(stdout())

### Name: pat_rnw
### Title: Set regular expressions to read input documents
### Aliases: pat_rnw pat_brew pat_tex pat_html pat_md pat_rst pat_asciidoc
###   pat_textile pat_typst

### ** Examples

# see how knit_patterns is modified
knit_patterns$get(); pat_rnw(); knit_patterns$get()

knit_patterns$restore()  # empty the list



cleanEx()
nameEx("rand_seed")
### * rand_seed

flush(stderr()); flush(stdout())

### Name: rand_seed
### Title: An unevaluated expression to return .Random.seed if exists
### Aliases: rand_seed
### Keywords: datasets

### ** Examples

eval(rand_seed)
rnorm(1) # .Random.seed is created (or modified)
eval(rand_seed)



cleanEx()
nameEx("raw_block")
### * raw_block

flush(stderr()); flush(stdout())

### Name: raw_block
### Title: Mark character strings as raw blocks in R Markdown
### Aliases: raw_block raw_latex raw_html

### ** Examples

knitr::raw_latex('\\emph{some text}')



cleanEx()
nameEx("raw_output")
### * raw_output

flush(stderr()); flush(stdout())

### Name: extract_raw_output
### Title: Mark character strings as raw output that should not be
###   converted
### Aliases: extract_raw_output restore_raw_output raw_output

### ** Examples

library(knitr)
out = c('*hello*', raw_output('<special>content</special> *protect* me!'), '*world*')
pre = extract_raw_output(out)
str(pre)
pre$value = gsub('[*]([^*]+)[*]', '<em>\\1</em>', pre$value)  # think this as Pandoc conversion
pre$value
# raw output was protected from the conversion (e.g. *protect* was not converted)
restore_raw_output(pre$value, pre$chunks)



cleanEx()
nameEx("read_chunk")
### * read_chunk

flush(stderr()); flush(stdout())

### Name: read_chunk
### Title: Read chunks from an external script
### Aliases: read_chunk read_demo

### ** Examples

## put this in foo.R and read_chunk('foo.R')

## ---- my-label ----
1+1
lm(y~x, data=data.frame(x=1:10,y=rnorm(10)))

## later you can use <<my-label>>= to reference this chunk

## the 2nd approach
code = c("#@a", '1+1', "#@b", "#@a", 'rnorm(10)', "#@b")
read_chunk(lines = code, labels = 'foo') # put all code into one chunk named foo
read_chunk(lines = code, labels = 'foo', from = 2, to = 2) # line 2 into chunk foo
read_chunk(lines = code, labels = c('foo', 'bar'), from = c(1, 4), to = c(3, 6))
# automatically figure out 'to'
read_chunk(lines = code, labels = c('foo', 'bar'), from = c(1, 4))
read_chunk(lines = code, labels = c('foo', 'bar'), from = "^#@a", to = "^#@b")
read_chunk(lines = code, labels = c('foo', 'bar'), from = "^#@a", to = "^#@b", from.offset = 1, to.offset = -1)

## later you can use, e.g., <<foo>>=
knitr::knit_code$get() # use this to check chunks in the current session
knitr::knit_code$restore() # clean up the session



cleanEx()
nameEx("rocco")
### * rocco

flush(stderr()); flush(stdout())

### Name: rocco
### Title: Knit R Markdown using the classic Docco style
### Aliases: rocco

### ** Examples

rocco_view=function(input) {
owd = setwd(tempdir()); on.exit(setwd(owd))
if (!file.exists(input)) return()
o=rocco(input, quiet=TRUE)
if (interactive()) browseURL(o)}
# knit these two vignettes using the docco style
rocco_view(system.file('doc', 'docco-classic.Rmd', package = 'knitr'))
rocco_view(system.file('doc', 'knit_expand.Rmd', package = 'knitr'))



cleanEx()
nameEx("set_alias")
### * set_alias

flush(stderr()); flush(stdout())

### Name: set_alias
### Title: Set aliases for chunk options
### Aliases: set_alias

### ** Examples

set_alias(w = 'fig.width', h = 'fig.height')
# then we can use options w and h in chunk headers instead of fig.width and fig.height



cleanEx()
nameEx("set_header")
### * set_header

flush(stderr()); flush(stdout())

### Name: set_header
### Title: Set the header information
### Aliases: set_header

### ** Examples

set_header(tikz = '\\usepackage{tikz}')
opts_knit$get('header')



cleanEx()
nameEx("set_parent")
### * set_parent

flush(stderr()); flush(stdout())

### Name: set_parent
### Title: Specify the parent document of child documents
### Aliases: set_parent

### ** Examples

## can use, e.g. \Sexpr{set_parent('parent_doc.Rnw')} or

# <<setup-child, include=FALSE>>=

# set_parent('parent_doc.Rnw')

# @



cleanEx()
nameEx("stitch")
### * stitch

flush(stderr()); flush(stdout())

### Name: stitch
### Title: Automatically create a report based on an R script and a
###   template
### Aliases: stitch stitch_rhtml stitch_rmd

### ** Examples

s = system.file('misc', 'stitch-test.R', package = 'knitr')
if (interactive()) stitch(s)  # compile to PDF

# HTML report
stitch(s, system.file('misc', 'knitr-template.Rhtml', package = 'knitr'))

# or convert markdown to HTML
stitch(s, system.file('misc', 'knitr-template.Rmd', package = 'knitr'))

unlink(c('stitch-test.html', 'stitch-test.md', 'figure'), recursive = TRUE)



cleanEx()
nameEx("vignette_engines")
### * vignette_engines

flush(stderr()); flush(stdout())

### Name: vignette_engines
### Title: Package vignette engines
### Aliases: vignette_engines

### ** Examples

library(knitr)
vig_list = tools::vignetteEngine(package = 'knitr')
str(vig_list)
vig_list[['knitr::knitr']][c('weave', 'tangle')]
vig_list[['knitr::knitr_notangle']][c('weave', 'tangle')]
vig_list[['knitr::docco_classic']][c('weave', 'tangle')]



cleanEx()
nameEx("wrap_rmd")
### * wrap_rmd

flush(stderr()); flush(stdout())

### Name: wrap_rmd
### Title: Wrap long lines in Rmd files
### Aliases: wrap_rmd

### ** Examples

wrap_rmd(text = c('```', '1+1', '```', '- a list item', '> a quote', '',
paste(rep('this is a normal paragraph', 5), collapse = ' ')))



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
