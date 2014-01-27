---
layout: default
title: knitr
subtitle: Elegant, flexible and fast dynamic report generation with R
---

## Overview

The **knitr** package was designed to be a transparent engine for dynamic
report generation with R, solve some long-standing problems in Sweave, and
combine features in other add-on packages into one package (**knitr**
&asymp; Sweave + cacheSweave + pgfSweave + weaver + `animation::saveLatex` +
`R2HTML::RweaveHTML` + `highlight::HighlightWeaveLatex` + 0.2 * brew + 0.1 *
SweaveListingUtils + more).

[<img src="http://i.imgur.com/R6DSHDE.jpg" align="right" />](http://amzn.com/1482203537)

- Transparency means that the user has full access to every piece of the
  input and output, e.g., `1 + 2` produces `[1] 3` in an R terminal, and 
  **knitr** can let the user decide whether to put `1 + 2` between
  `\begin{verbatim}` and `\end{verbatim}`, or `<div class="rsource">` and
  `</div>`, and put `[1] 3` in `\begin{Routput}` and `\end{Routput}`; see
  the [hooks](hooks) page for details
- **knitr** tries to be consistent with users' expections by running R code as
  if it were pasted in an R terminal, e.g., `qplot(x, y)` directly produces
  the plot (no need to `print()` it), and *all* the plots in a code chunk
  will be written to the output by default
- Packages like **pgfSweave** and **cacheSweave** have added useful features to
  Sweave (high-quality tikz graphics and cache), and **knitr** has
  simplified the implementations
- The design of **knitr** allows any input languages (e.g. R, Python and awk)
  and any output markup languages (e.g. LaTeX, HTML, Markdown, AsciiDoc, and
  reStructuredText)

This package is developed on  [GitHub](https://github.com/yihui/knitr); for
installation instructions and [FAQ's](http://bit.ly/knitr-faq), see
[README](https://github.com/yihui/knitr#readme). This website serves as the
full documentation of **knitr**, and you can find the [main
manual](http://bit.ly/117OLVl), the [graphics manual](http://bit.ly/114GNdP)
and other [demos](/knitr/demos) /
[examples](https://github.com/yihui/knitr-examples) here. For a more
organized reference, see the [knitr book](https://github.com/yihui/knitr-book).

<div style="text-align: center;"><iframe src="http://www.screenr.com/embed/qcv8" width="650" height="396" frameborder="0"></iframe></div>

## Motivation

One of the difficulties with extending Sweave is we have to copy a large
amount of code from the **utils** package (the file `SweaveDrivers.R` has
more than 700 lines of R code), and this is what the two packages mentioned
above have done. Once the code is copied, the package authors have to pay
close attention to what is changing in the version in official R --
apparently an extra burden. The **knitr** package tried to modularize the
whole process of weaving a document into small manageable functions, so it
is hopefully easier to maintain and extend (e.g. easy to support HTML
output); on the other hand, **knitr** has many built-in features and it
should not be the case to have to hack at the core components of this
package. By the way, several FAQ's in the Sweave manual are solved in
**knitr** directly.

> Let us change our traditional attitude to the construction of programs:
> Instead of imagining that our main task is to instruct a computer what to
> do, let us concentrate rather on explaining to humans what we want the
> computer to do. <cite>-- Donald E. Knuth, Literate Programming, 1984</cite>

## Features

The ideas are borrowed from other packages, and some of them are
re-implemented in a different way (like cache). A selected list of features
include:

- *faithful* output: using
  [**evaluate**](http://cran.r-project.org/package=evaluate) as the backend
  to evaluate R code, **knitr** writes everything that you see in an R
  terminal into the output by default, including printed results, plots and
  even warnings, messages as well as errors (they should not be ignored in
  serious computations, especially warnings)
  - a minor issue is that for grid-based graphics packages like **ggplot2** or
    **lattice**, users often forget to `print()` the plot objects, because
    they can get the output in an R terminal without really `print()`ing; in
    **knitr**, what you get is what you expected
- built-in cache: ideas like **cacheSweave** but **knitr** directly uses base R
  functions to fulfill cache and lazy loading, and another significant
  difference is that a cached chunk can still have output (in **cacheSweave**,
  cached chunks no longer have any output, even you explicitly `print()`
  an object; **knitr** actually caches the chunk output as well)
- formatting R code: the [**formatR**](https://github.com/yihui/formatR/wiki)
  package is used to reformat R code automatically (wrap long lines, add
  spaces and indent, etc), without sacrificing comments as
  `keep.source=FALSE` does
- more than 20 graphics devices are directly supported: with `dev='CairoPNG'`
  in the chunk options, you can switch to the `CairoPNG()` device in
  [**Cairo**](http://cran.r-project.org/package=Cairo) in a second; with
  `dev='tikz'`, the `tikz()` device in
  [**tikzDevice**](http://cran.r-project.org/package=tikzDevice) is used;
  Could anything be easier than that? These built-in devices (strictly speaking,
  wrappers) use inches as units, even for bitmap devices (pixels are
  converted to inches by the option `dpi`, which defaults to 72)
- even more flexibility on graphics:
  - width and height in the output document of plots can be additionally
    specified (the `fig.width` option is for the graphics device, and
    `out.width` is for the output document; think `out.width='.8\\textwidth'`)
  - locations of plots can be rearranged: they can either appear exactly in the
    place where they are created, or go to the end of a chunk together
    (option `fig.show='hold'`)
  - multiple plots per code chunk are recorded, unless you really  want to keep
    the last plot only (option `fig.keep='last'`)
- R code not only can come from code chunks in the input document, but also may
  be from an external R script, which makes it easier to run the code as you
  write the document (this will especially benefit LyX)
- for power users, further customization is still possible:
  - the regular expressions to parse R code can be defined, i.e., you do not
    have to use `<<>>=` and `@` or `\Sexpr{}`; if you like, you can use any
    patterns, e.g., `%% begin.rcode` and `%% end.rcode`
  - hooks can be defined to control the output; e.g. you may want to put errors
    in red bold texts, or you want the source code to be italic, etc; hooks
    can also be defined to be executed before or after a code chunk, and
    there are infinite possibilities to extend the power of this package by
    hooks (e.g. animations, rgl 3D plots, ...)

Lots of efforts have been made to producing beautiful output and enhancing
readability by default. For example, code chunks are highlighted and put in
a shaded environment in LaTeX with a very light gray background (the
[**framed**](http://www.ctan.org/pkg/framed) package), so they can stand out
a little bit from other texts. The reading experience is hopefully better
than the `verbatim` or `Verbatim` environments. The leading characters `>`
and `+` (called prompts) in the output are *not* added by default (you can
bring them back by `prompt=TRUE`, though). I find them really annoying in
the output when I read the output document, because it is so very
inconvenient to copy and run the code which is messed up by these characters.

## Acknowledgements

I thank the authors of Sweave, pgfSweave, cacheSweave, brew, decumar,
R2HTML, tikzDevice, highlight, digest, evaluate, roxygen2 and of course, R,
for the many inspiring ideas and tools. I really appreciate the
[feedback](https://github.com/yihui/knitr/issues) from many early beta
testers. This package was initiated based on the design of decumar.

## FOAS

<img src="http://www.foastat.org/images/foas.png" style="float: right;" />
`knitr` is proudly affiliated with the [Foundation for Open Access
Statistics](http://www.foastat.org/) (FOAS). FOAS is a nonprofit public
benefit corporation with a worldwide mission to promote free software, open
access publishing, and reproducible research in statistics.

## Misc

Obviously the package name `knitr` was coined with `weave` in mind, and it
also aims to be *neater*. I thank [Hadley](http://had.co.nz),
[Di](http://dicook.public.iastate.edu) and
[Andrew](http://www.stat.tamu.edu/~aredd/site/) for discussions on this neat
name.
