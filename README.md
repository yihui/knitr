# knit

The R package **knit** is a general-purpose literate programming engine, with lightweight API's designed to give users full control of the output without heavy coding work. It combines many features into one package with slight tweaks motivated from my everyday use of Sweave. See the [package homepage](http://yihui.github.com/knit) for  details and examples.

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

The package **knit** was designed to give the user access to every part of the process of dealing with a literate programming document, so there is no need to hack at any core components if you want more freedom. I have gone through the souce code of **pgfSweave** and **cacheSweave** for a couple of times, and I often feel uncomfortable with the large amount of code copied from official R, especially when R has a new version released (I will begin to worry if the add-on packages are still up-to-date with the official Sweave).

## Features

The ideas are borrowed from other packages, and some of them are re-implemented in a different way (like cache). A selected list of features include:

- *faithful* output: using [**evaluate**](http://cran.r-project.org/package=evaluate) as the backend to evaluate R code, **knit** writes everything that you see in an R terminal into the output by default, even including warnings, messages and errors; the rationale is simple -- they should not be ignored in serious computations, especially warnings
  - a minor issue is that for grid-based graphics packages like **ggplot2** or **lattice**, users often forget to `print()` the plot objects, because they can get the output in an R terminal without really `print()`ing; why we have to code differently when doing literate programming?
- built-in cache: ideas like **cacheSweave** but **knit** only uses simple `.RData` to store cached objects, and a cached chunk can still 'print' results (in **cacheSweave**, cached chunks no longer have any output, even you explicitly `print()` an object)
- formatting R code: the [**formatR** package](https://github.com/yihui/formatR/wiki) is used to reformat R code automatically (wrap long lines, add spaces and indent, etc), without sacraficing comments as `keep.source = FALSE` does
- built-in definitions of graphics devices: with `dev = CairoPNG` in the chunk options, you can switch to the `CairoPNG()` device in [**Cairo**](http://cran.r-project.org/package=Cairo) in a second; with `dev = tikz`, the `tikz()` device in [**tikzDevice**](http://cran.r-project.org/package=tikzDevice) is used; how things can be even easier?
  - these built-in devices (strictly speaking, wrappers) use inches as units, even for bitmap devices (pixels are converted to inches by the option `dpi`, which defaults to 72)
- more flexibility on graphics
  - width and height in the output document of plots can be additionally specified (the `width` option is for the graphics device, and `out.width` is for the output document; think `out.width=.8\textwidth`)
  - locations of plots can be rearranged: they can either appear in the place where they are created, or go to the end of a chunk together (option `fig.hold=TRUE`)
  - multiple plots per code chunk are recorded, unless you really  want to keep the last plot only (option `fig.last=TRUE`)
- for power users, further customization is still possible
  - the regular expressions to parse R code can be defined, i.e., you do not have to use `<<>>=` and `@` or `\Sexpr{}`; if you like, you can use any patterns, e.g., `%% begin.rcode` and `%% end.rcode`
  - hooks can be defined to control the output; e.g. you may want to put errors in red bold texts, or you want the source code to be italic, etc; hooks can also be defined to be executed before or after a code chunk, e.g. when `fig=TRUE`, you may want to automatically run `par(mar = c(4, 4, 1, 1))` instead of doing it in every code chunk

## Usage

The story is long, and the usage is short:

```r
library(knit)
knit(input)
```

If options are not explicitly specified, **knit** will try to guess reasonable default settings.

As I'm still working on more features, please feel free to give me feedback on this package or share your experience of literate programming with R at the [issues page](https://github.com/yihui/knit/issues).