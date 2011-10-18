# knit

The R package **knit** is a general-purpose literate programming engine, with lightweight API's designed to give users full control of the output without heavy coding work. It combines many features into one package with slight tweaks motivated from my everyday use of Sweave, for example:

- *faithful* output: using **evaluate** as the backend to evaluate R code, **knit** writes everything that you see in an R terminal into the output by default, even including warnings, messages and errors; the rationale is simple -- they should not be ignored in serious computations, especially warnings
  - a minor issue is that for grid-based graphics packages like **ggplot2** or **lattice**, users often forget to `print()` the plot objects, because they can get the output in an R terminal without really `print()`ing; why we have to code differently when doing literate programming?
- built-in cache: ideas like **cacheSweave** but **knit** only uses simple `.RData` to store cached objects, and a cached chunk can still 'print' results (in **cacheSweave**, cached chunks no longer have any output, even you explicitly `print()` an object)
- formatting R code: the [**formatR** package](https://github.com/yihui/formatR/wiki) is used to reformat R code automatically (wrap long lines, add spaces and indent, etc), without sacraficing comments as `keep.source = FALSE` does
- built-in definitions of graphics devices: with `dev = CairoPNG` in the chunk options, you can switch to the `CairoPNG()` device in **Cairo** in a second; with `dev = tikz`, the `tikz()` device in **tikzDevice** is used; how things can be even easier?
  - these built-in devices (strictly speaking, wrappers) use inches as units, even for bitmap devices (pixels are converted to inches by the option `dpi`, which defaults to 72)
- more flexibility on graphics
  - width and height in the output document of plots can be additionally specified (the `width` option is for the graphics device, and `out.width` is for the output document; think `out.width=.8\textwidth`)
  - locations of plots can be rearranged: they can either appear in the place where they are created, or go to the end of a chunk together (option `fig.hold=TRUE`)
  - multiple plots per code chunk are recorded, unless you really  want to keep the last plot only (option `fig.last=TRUE`)
- for power users, further customization is still possible
  - the regular expressions to parse R code can be defined, i.e., you do not have to use `<<>>=` and `@` or `\Sexpr{}`; if you like, you can use any patterns, e.g., `%% begin.rcode` and `%% end.rcode`
  - hooks can be defined to control the output; e.g. you may want to put errors in red bold texts, or you want the source code to be italic, etc; hooks can also be defined to be executed before or after a code chunk, e.g. when `fig=TRUE`, you may want to automatically run `par(mar = c(4, 4, 1, 1))` instead of doing it in every code chunk