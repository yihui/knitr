# knitr

The R package **knitr** is a general-purpose literate programming engine, with lightweight API's designed to give users full control of the output without heavy coding work. It combines many features into one package with slight tweaks motivated from my everyday use of Sweave. See the [package homepage](http://yihui.github.com/knitr) for  details and examples. Feedback can be sent to the [mailing list](https://groups.google.com/group/knitr), and bugs can be reported to the [issues page](https://github.com/yihui/knitr/issues).

## Installation

You can install the stable version on CRAN:

```r
install.packages('knitr', dependencies = TRUE)
```

Or check out `git://github.com/yihui/knitr.git` with GIT and run `R CMD INSTALL`, or use the **devtools** package to install the development version:

```r
## this package depends on R >= 2.14.1
## you may also need to update your packages: 
## options(repos = c(CRAN = 'http://cran.r-project.org'))
## update.packages()
library(devtools); install_github('knitr', 'yihui')
```

Note Windows users have to first install [Rtools](http://www.murdoch-sutherland.com/Rtools/).

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

If options are not explicitly specified, **knitr** will try to guess reasonable default settings. A few manuals are available such as the [main manual](https://github.com/downloads/yihui/knitr/knitr-manual.pdf), the [graphics manual](https://github.com/downloads/yihui/knitr/knitr-graphics.pdf), and the [themes manual](https://github.com/downloads/yihui/knitr/knitr-themes.pdf).

## FAQ

The FAQ's are compiled from the [issues](https://github.com/yihui/knitr/issues) and messages I received from blog posts and emails, etc.

1. **knitr** does not work...
  - Please first update all your R packages (use `update.packages()`) and probably R itself ([what is the current R version?](http://cran.r-project.org/)), then see if it works; if not, file an [issue](https://github.com/yihui/knitr/issues) to me (email is fine, too).
1. Oh, the tons of arguments like `Sweave(..., prefix.string=abc, keep.source=FALSE, foo=bar)` are really flexible; why `knit()` only has so few arguments?
  - Because I believe putting these arguments in `knit()` breaks the principle of reproducibility, and so does using environmental variables (see [#19](https://github.com/yihui/knitr/issues/19) for details).
1. I love RStudio and Sweave is sweet there; is **knitr** going to work with RStudio?
  - I love RStudio too; although I use Emacs most of the time, I recommend RStudio to other users who do not use Emacs. Unfortunately the function behind that `compile` button was hard-coded and only supports Sweave, so you may want to pursuade RStudio developers providing an option to choose between Sweave and **knitr**. By the way, I actually recommend LyX over RStudio if you want to use Sweave or **knitr**.
1. You mentioned LyX so many times, so what the heck is LyX?
  - It is an intelligent wrapper for LaTeX; see http://www.lyx.org for details. I would like to define it as a software package that can both increase the productivity of an _experienced_ LaTeX user by 300%, and decrease it by 500% for a LaTeX novice. Don't use it simply because its GUI is so tempting; it is not MS Word. I have added support for **knitr** in LyX; see [the lyx demo page](http://yihui.github.com/knitr/demo/lyx/).
1. Where are those prompt characters `>` and `+`? I feel uncomfortable reading R output without them.
  - They are removed by default, because I believe they make no sense. This is the reason why I dislike books on R which used `>` and `+`; they twist my mind and make my eyes bleed when I read the R code in the books. For those who really want to read R code like `> 1+1` instead of `1 + 1`, you have the [chunk option](http://yihui.github.com/knitr/options) `prompt`.
1. Can I change my working directory?
  - Yes, you can use `setwd()` freely in your code, which will bring you troubles in Sweave but not in **knitr**. The default working directory is the directory where your input document comes from. I need to warn you, however, that this is a bad practice in general. See [#38](https://github.com/yihui/knitr/issues/38) for a discussion. You should also try to avoid absolute directories whenever possible (use relative directories instead), because it makes things less reproducible.
1. The gray (shading) box is too narrow for my output.
  - No, it is not because the box is too narrow (the box uses the current line width); it is because your output is too wide. Use a smaller `width` option to avoid output exceeding the page margin, e.g. `options(width = 60)`; see [#44](https://github.com/yihui/knitr/issues/44).
1. I have done something cool with **knitr**; could you add a link in your website?
  - Sure! I'd love to; just let me know.
1. What can I do for you?
  - Many things, e.g. donate me zillions of money (well, I'm kidding), buy me beer, [tweet](https://twitter.com/xieyihui) my [links](http://yihui.github.com/knitr), mention **knitr** on [Google+](https://plus.google.com/u/0/109653178371807724268/posts) or Facebook, or fork this repository and contribute code, or just say hello to me somewhere
