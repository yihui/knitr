---
layout: default
title: Objects
subtitle: Objects to manipulate options, patterns and hooks
---

The **knitr** package uses a special object to control options and settings (denoted as `obj` below); it has the following methods:

- `obj$get(name)`: returns an option named `name` or a list of several options if `name` is a character vector of length greater than 1, and it returns all the options if `name` not provided
- `obj$set(...)`: permanently changes options; the argument `...` can be of the form `tag = value` or a list of options `list(opt1 = value1, opt2 = value2)`
- `obj$merge(values)`: temporarily merges a list of new options into the current list and returns the merged list (original list not changed)
- `obj$restore()`: restores the object

Four such objects are visible to users in **knitr**:

- [`opts_chunk`](options#chunk_options): manages **opt**ions for code **c**hunks
- [`opts_knit`](options#package_options): manages **opt**ions for the **k**nitr package
- [`knit_hooks`](hooks): manages hook functions
- [`knit_patterns`](patterns): manages regular expressions to extract R code from the input document

Except `knit_patterns`, all other three objects are initialized with default values, and `knit_patterns` will be automatically determined according to the type of input document if not provided. The `knit_hooks` object is supposed to be used most frequently, and the other three are usually not to be used directly. For example, `opts_chunk` is usually set in the input document rather than using the command line directly.

It is recommended to get the settings done in the first chunk with `echo = FALSE` and `results = hide` like this (we may call this chunk as the configuration chunk):

{% highlight r %}
<<knitr-config, echo=FALSE, results=hide>>=
library(knitr)
opts_knit$set(header = c('\\usepackage{listings}', '\\lstset{language=R}'))
## change the header information to use the LaTeX package listings
## you need to change hooks accordingly, of course
@
{% endhighlight %}

As a technical note, these objects are similar to closures -- they consist of a list of functions returned by a function. For details, see the unexported function `knitr:::new_defaults`. The chunk options are also managed by closures.
