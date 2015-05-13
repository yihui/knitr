# knitr

[![Build Status](https://travis-ci.org/yihui/knitr.svg)](https://travis-ci.org/yihui/knitr)
[![Coverage Status](https://img.shields.io/coveralls/yihui/knitr.svg)](https://coveralls.io/r/yihui/knitr?branch=master)
[![Downloads from the RStudio CRAN mirror](http://cranlogs.r-pkg.org/badges/knitr)](http://cran.rstudio.com/package=knitr)

The R package **knitr** is a general-purpose literate programming engine,
with lightweight API's designed to give users full control of the output
without heavy coding work. It combines many features into one package with
slight tweaks motivated from my everyday use of Sweave. See the package
[homepage](http://yihui.name/knitr) for details and examples. See
[FAQ's](https://github.com/yihui/knitr/blob/master/FAQ.md) for a list of
frequently asked questions (including where to ask questions).

## Installation

You can install the stable version on
[CRAN](http://cran.rstudio.com/package=knitr):

```r
install.packages('knitr', dependencies = TRUE)
```

You can also install the development version from
[XRAN](http://yihui.name/xran/), which provides daily build of **knitr**:

```r
# update all existing packages first
update.packages(ask = FALSE, repos = 'http://cran.rstudio.org')
install.packages('knitr', repos = c('http://yihui.name/xran', 'http://cran.rstudio.org'),
                 type = 'source')
```

Or use **devtools** to install the development version from Github:

```r
devtools::install_github('yihui/knitr', build_vignettes = TRUE)
```

## Motivation

While Sweave and related add-on packages like
[**cacheSweave**](http://cran.rstudio.com/package=cacheSweave) and
[**pgfSweave**](http://cran.rstudio.com/package=pgfSweave) are fairly good
engines for literate programming in R, but I often feel my hands are tied,
for example:

- I stared at the source code of Sweave and wished for hundreds of times
  that *if only I could easily insert* `[width=.8\textwidth]` *between*
  `\includegraphics` *and* `{my-plot.pdf}` (the official way in Sweave is
  `\setkeys{Gin}` but it is setting a global width, which is unrealistic
  since we often have to set widths individually; yes, you can use
  `\setkeys{Gin}` for many times, but why not just provide an option for
  each chunk?)
- I wished for many times that *if only I could use graphics devices other
  than PDF and postscript*; now the dream has come true in the official R,
  but what I was hoping for was an option as simple as `dev = 'png'` or `dev
  = 'CairoJPEG'`
- I wished multiple plots in a code chunk could be recorded instead of only
  the last one
- I wished there was a way to round the numbers in `\Sexpr{}` other than
  writing expressions like `\Sexpr{round(x, 3)}` for *each single* `\Sexpr{}`
- I wished I did not have to `print()` plots from
  [**ggplot2**](http://cran.rstudio.com/package=ggplot2) and a simple
  `qplot(x, y)` would just give me a plot in Sweave
- I wished users would never need instructions on `Sweave.sty` or run into
  troubles due to the fact that LaTeX cannot find `Sweave.sty`
- I wished **cacheSweave** could print the results of a code chunk even if
  it was cached
- I wished [**brew**](http://cran.rstudio.com/package=brew) could support
  graphics
- I wished [**R2HTML**](http://cran.rstudio.com/package=R2HTML) could
  support R code syntax highlighting
- ...

The package **knitr** was designed to give the user access to every part of
the process of dealing with a literate programming document, so there is no
need to hack at any core components if you want more freedom. I have gone
through the source code of **pgfSweave** and **cacheSweave** for a couple of
times, and I often feel uncomfortable with the large amount of code copied
from official R, especially when R has a new version released (I will begin
to worry if the add-on packages are still up-to-date with the official
Sweave).

## Usage

```r
library(knitr)
?knit
knit(input)
```

[<img src="http://i.imgur.com/R6DSHDE.jpg" align="right" alt="The book Dynamic Documents with R and knitr" />](http://amzn.com/1482203537)

If options are not explicitly specified, **knitr** will try to guess
reasonable default settings. A few manuals are available such as the [main
manual](http://yihui.name/knitr/demo/manual/), and the
[graphics
manual](http://yihui.name/knitr/demo/graphics/). For a
more organized reference, see the [knitr book](http://amzn.com/1482203537).

## License

This package is free and open source software, licensed under GPL.
