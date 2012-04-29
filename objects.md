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

These objects are visible to users in **knitr**:

- [`opts_chunk`](options#chunk_options) and `opts_current`: manages **opt**ions for code **chunk**s
- [`opts_knit`](options#package_options): manages **opt**ions for the **knit**r package
- [`knit_hooks`](hooks): manages hook functions
- [`knit_patterns`](patterns): manages regular expressions to extract R code from the input document
- `knit_engines`: functions to deal with other languages

Except `knit_patterns`, all other objects are initialized with default values, and `knit_patterns` will be automatically determined according to the type of input document if not provided. The `knit_hooks` object is supposed to be used most frequently, and the other three are usually not to be used directly. For example, `opts_chunk` is usually set in the input document rather than using the command line directly.

It is recommended to get the settings done in the first chunk with `cache = FALSE` and `include = FALSE` like this (we may call this chunk as the configuration chunk):

{% highlight r %}
<<setup, cache=FALSE, include=FALSE>>=
opts_knit$set(upload.fun = imgur_upload, self.contained = FALSE)
@
{% endhighlight %}

As a technical note, these objects are similar to closures -- they consist of a list of functions returned by a function. For details, see the unexported function `knitr:::new_defaults`. The chunk options are also managed by closures.
