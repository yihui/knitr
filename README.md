# knitr

[![Build Status](https://travis-ci.org/yihui/knitr.png)](https://travis-ci.org/yihui/knitr)

The R package **knitr** is a general-purpose literate programming engine,
with lightweight API's designed to give users full control of the output
without heavy coding work. It combines many features into one package with
slight tweaks motivated from my everyday use of Sweave. See the package
[homepage](http://yihui.name/knitr) for details and examples. See
[FAQ's](https://github.com/yihui/knitr/blob/master/FAQ.md) for a list of
frequently asked questions (including where to ask questions).

## Installation

You can install the stable version on
[CRAN](http://cran.r-project.org/package=knitr):

```r
install.packages('knitr', dependencies = TRUE)
```

You can also install the development version from
[RForge](http://rforge.net/knitr/), which provides daily build of **knitr**:

```r
install.packages('knitr', repos = 'http://www.rforge.net/', type = 'source')
```

If you know GIT and `R CMD build`, here is another way:

```bash
git clone https://github.com/yihui/knitr.git
R CMD build knitr
R CMD INSTALL knitr_*.tar.gz
```

## Motivation

While Sweave and related add-on packages like
[**cacheSweave**](http://cran.r-project.org/package=cacheSweave) and
[**pgfSweave**](http://cran.r-project.org/package=pgfSweave) are fairly good
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
  [**ggplot2**](http://cran.r-project.org/package=ggplot2) and a simple
  `qplot(x, y)` would just give me a plot in Sweave
- I wished users would never need instructions on `Sweave.sty` or run into
  troubles due to the fact that LaTeX cannot find `Sweave.sty`
- I wished **cacheSweave** could print the results of a code chunk even if
  it was cached
- I wished [**brew**](http://cran.r-project.org/package=brew) could support
  graphics
- I wished [**R2HTML**](http://cran.r-project.org/package=R2HTML) could
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

If options are not explicitly specified, **knitr** will try to guess
reasonable default settings. A few manuals are available such as the [main
manual](https://bitbucket.org/stat/knitr/downloads/knitr-manual.pdf), the
[graphics
manual](https://bitbucket.org/stat/knitr/downloads/knitr-graphics.pdf), and
the [themes
manual](https://bitbucket.org/stat/knitr/downloads/knitr-themes.pdf).

## Contributing

For very simple changes such as fixing typos, you can just edit the file by
clicking the button `Edit` after you open the file online. For more
complicated changes, you will have to manually create a [pull
request](https://help.github.com/articles/using-pull-requests) after
[forking](https://help.github.com/articles/fork-a-repo) this repository.

To make sure you did not break anything, you need to run tests, which are
done through the [**testit**](http://cran.r-project.org/package=testit)
package. If you added any features, add your own tests in `tests/testit/`.
You can run tests using `make`, e.g.

```bash
cd knitr
make check
```

If you are lazy or do not understand what I said above, just forget about
it. The simplest thing to do is to look at the `Commits` panel after you
sent the pull request -- if you see green check marks ✔ on your commits, you
are fine. [Travis
CI](http://yihui.name/en/2013/04/travis-ci-general-purpose/) will run the
tests automatically for me. If your pull request passes the tests, you see
green check marks.

## License

This package is free and open source software, licensed under GPL.
