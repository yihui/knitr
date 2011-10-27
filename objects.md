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

Three such objects are visible to users in **knitr**:

- [`optc`](options#chunk_options): manages **opt**ions for code **c**hunks
- [`optk`](options#package_options): manages **opt**ions for the **k**nitr package
- [`hooks`](hooks): manages hook functions
- [`kpat`](patterns): manages regular expressions to extract R code from the input document

Except `kpat`, all other three objects are initialized with default values, and `kpat` will be automatically determined according to the type of input document if not provided.

It is recommended to get the settings done in the first chunk with `echo = FALSE` and `results = FALSE` like this (we may call this chunk as the configuration chunk):

{% highlight r %}
<<knitr-config, echo=FALSE, results=FALSE>>=
library(knitr)
kopt$set(header = c('\\usepackage{listings}', '\\lstset{language=R}'))
## change the header information to use the LaTeX package listings
## you need to change hooks accordingly, of course
@
{% endhighlight %}

As a technical note, these objects are similar to closures -- they consist of a list of functions returned by a function. For details, see the unexported function `knitr:::new_defaults`. The chunk options are also managed by closures.