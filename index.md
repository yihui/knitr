---
layout: default
title: knit
subtitle: A general-purpose literate programming engine for R
---

## Overview

The **knit** package was designed to be a transparent engine for literate programming with R, solve some long-lasting problems in Sweave, and combine features in other add-on packages into one package. 

- Transparency means that the user has full access to every piece of the input and output, e.g., `1 + 2` produces `[1] 3` in an R terminal, and **knit** can let the user decide whether to put `1 + 2` between `\begin{verbatim}` and `\end{verbatim}`, or `<div class="rsource">` and `</div>`, and put `[1] 3` in `\begin{Routput}` and `\end{Routput}`; this kind of freedom even applies to warning messages, errors and plots (e.g. decorate error messages with red bold fonts); see the [hooks](/knit/hooks) page for details
- **knit** tries to be consistent with users' expections by running R code as if it were pasted in an R terminal, e.g., `qplot(x, y)` directly produces the plot (no need to `print()` it), and *all* the plots in a code chunk will be written to the output by default; **knit** also added options like `out.width` to set the width of plots in the output document (think `.8\textwidth` in LaTeX), so we no longer need to hack in LaTeX
- Packages like **pgfSweave** and **cacheSweave** have added useful features to Sweave (high-quality tikz graphics and cache), and **knit** has simplified the implementations

One of the difficulties with extending Sweave is we have to copy a large amount of code from the **utils** package (the file `SweaveDrivers.R` has more than 700 lines of R code), and this is what the two packages mentioned above have done. Once the code is copied, the package authors have to pay close attention to what is changing in the version in official R -- apparently an extra burden. The **knit** package tried to modularize the whole process of weaving a document into small manageable functions, so it is hopefully easier to maintain and extend (e.g. easy to support HTML output); on the other hand, **knit** has many built-in features and it should not be the case to have to hack at the core components of this package.

> Let us change our traditional attitude to the construction of programs: Instead of imagining that our main task is to instruct a computer what to do, let us concentrate rather on explaining to humans what we want the computer to do.
>
> <cite>-- Donald E. Knuth, [Literate Programming](http://www.literateprogramming.com/knuthweb.pdf), 1984</cite>

## Features

## The Process

## Comparison

Some features of Sweave were dropped in **knit** and some were changed, including

- `concordance` was dropped: I have never used it
- `keep.source` was merged into a more flexible option `tidy`
- `print` was dropped: whether an R expression is going to be printed is consistent with your experince of using R (e.g., `x <- 1` will not be printed, while `1:10` will; just imagine you are typing the commands in an R console/terminal); if you really want the output of an expression to be invisible, you may use the function `invisible()`
- `term` was dropped (think `term = TRUE`)
- `split` was dropped: by default the results are written into the output document, but since you have access to them, you can decide where they should go using hooks
- `stripe.white` was dropped (rarely used, and can be achieved by hooks)
- `prefix` was dropped (think `prefix = TRUE`; `prefix.string` is always used for figure filenames)
- `include` was dropped (use hooks instead if you want it)
- `eps` and `pdf` were dropped: use the new option `dev`, which has 20 predefined graphical devices (if that is not enough, please let me know, or set it in chunk options by yourself)
- chunk reference using `<<chunk-label>>` was dropped: I feel this should not have been invented, since R has **functions**, so chunk references might be an indication of a lack of modularization of R code; if you want to reuse a chunk, write a function instead of explicitly repeating code

Besides, the LaTeX style file `Sweave.sty` was dropped as well; it has brought too much confusion to users since it is shipped with R instead of LaTeX; **knit** has built-in styles, and users are free to change them using hooks.

It also has some differences with **pgfSweave** and **cacheSweave**:

- For the former, `dev = tikz` in **knit** means `tikz = TRUE` in **pgfSweave**, and `external = TRUE` was implemented differently -- the cache of tikz graphics is moved to the R level instead of relying on the LaTeX package **tikz** (if a tikz plot is externalized, **knit** will try to compile it to PDF immediately and use `\includegraphics{filename}` to insert it into the output; `external = FALSE` uses `\input{filename.tikz}`); this frees the users from the `make` utility and understanding tikz externalization.
- For the latter, `cache` is implemented with functions in base R (e.g., `lazyLoad()`) and does not rely on other add-on packages; for cached chunks, the results from the last run will be written into the output, and this is more consistent with the default behavior of R code (users may wonder why `print(x)` does not produce any output for cached chunks; plots in cached chunks will still be in the output as well)

## Misc

Obviously the package name `knit` was coined with `weave` in mind, and it also aims to be *neat*.