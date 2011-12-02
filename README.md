# knitr

<span style="float:right;"><a href="https://twitter.com/share" class="twitter-share-button" data-count="horizontal" data-via="xieyihui">Tweet</a>

<script type="text/javascript" src="//platform.twitter.com/widgets.js"></script>

<g:plusone size="small"></g:plusone>

<script type="text/javascript">
  (function() {
  var po = document.createElement('script'); po.type = 'text/javascript'; po.async = true;
  po.src = 'https://apis.google.com/js/plusone.js';
  var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(po, s);
  })();
</script>

</span>
The R package **knitr** is a general-purpose literate programming engine, with lightweight API's designed to give users full control of the output without heavy coding work. It combines many features into one package with slight tweaks motivated from my everyday use of Sweave. See the [package homepage](http://yihui.github.com/knitr) for  details and examples.

## Installation

This package is not on CRAN yet. Currently you can either check out the repository with GIT and run `R CMD INSTALL`, or use the **devtools** package to install:

```r
library(devtools); install_github('knitr', 'yihui')
```

Note Windows users have to first install [Rtools](http://www.murdoch-sutherland.com/Rtools/). After the package is released (hopefully in the very near future), you will be able to use the normal approach to install it:

```r
install.packages('knitr')
```

## Motivation

While Sweave and related add-on packages like [**cacheSweave**](http://cran.r-project.org/package=cacheSweave) and [**pgfSweave**](http://cran.r-project.org/package=pgfSweave) are fairly good engines for literate programming in R, but I often feel my hands are tied, for example:

- I stared at the source code of Sweave and wished for hundreds of times that *if only I could easily insert* `[width=.8\textwidth]` *between* `\includegraphics` *and* `{my-plot.pdf}` (the official way in Sweave is `\setkeys{Gin}` but it is setting a global width, which is unrealistic since we often have to set widths individually; yes, you can use `\setkeys{Gin}` for many times, but why not just provide an option for each chunk?)
- I wished for many times that *if only I could use graphics devices other than PDF and postscript*; now the dream has come true in the official R, but what I was hoping for was an option as simple as `dev = png` or `dev = CairoJPEG`
- I wished multiple plots in a code chunk could be recorded instead of only the last one
- I wished there was a way to round the numbers in `\Sexpr{}` other than writing expressions like `\Sexpr{round(x, 3)}` for *each single* `\Sexpr{}`
- I wished I did not have to `print()` plots from [**ggplot2**](http://cran.r-project.org/package=ggplot2) and a simple `qplot(x, y)` would just give me a plot in Sweave
- I wished users would never need instructions on `Sweave.sty` or run into troubles due to the fact that LaTeX cannot find `Sweave.sty`
- I wished **cacheSweave** could print the results of a code chunk even if it was cached
- I wished [**brew**](http://cran.r-project.org/package=brew) could support graphics
- I wished [**R2HTML**](http://cran.r-project.org/package=R2HTML) could support R code highlighting
- ...

The package **knitr** was designed to give the user access to every part of the process of dealing with a literate programming document, so there is no need to hack at any core components if you want more freedom. I have gone through the souce code of **pgfSweave** and **cacheSweave** for a couple of times, and I often feel uncomfortable with the large amount of code copied from official R, especially when R has a new version released (I will begin to worry if the add-on packages are still up-to-date with the official Sweave).

## Usage

```r
library(knitr)
?knit
knit(input)
```

If options are not explicitly specified, **knitr** will try to guess reasonable default settings.

As I'm still working on more features, please feel free to give me feedback on this package or share your experience of literate programming with R at the [issues page](https://github.com/yihui/knitr/issues).
