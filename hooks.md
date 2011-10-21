---
layout: default
title: Hooks
subtitle: Customizable functions to run before/after a code chunk and tweak the output of knit
---

- [Chunk hooks](#chunk_hooks)
- [Output hooks](#output_hooks)

The object `hooks` in the **knit** package is used to set hooks; the basic usage is `hooks$set(param = FUN)` (see [objects](/knit/objects) for details) where `param` is the name of a chunk option (can be arbitrary), and `FUN` is a function. There are two types of hooks: chunk hooks and output hooks. Hook functions may have different forms, depending what they are designed to do.

## Chunk hooks

Chunk hooks are functions to be called before or after a code chunk when the chunk option is `TRUE`, and they should be defined with three arguments:

{% highlight r %}
foo_hook = function(before, options, envir) {
    if (before) {
        ## code to be run before a chunk
    } else {
        ## code to be run after a chunk
    }
}
{% endhighlight %}

When **knit** is processing the document, `foo_hook(before = TRUE)` will be called before a code chunk is executed (unless the chunk is cached or set not to be evaluated), and `foo_hook(before = FALSE)` is called after a chunk; the argument `options` is a list of [options](/knit/options) in the current chunk (e.g. `options$label` is the label of the current chunk), and `envir` is the environment in which the code chunk is evaluated. The latter two arguments can be optionally used in a chunk hook. For example, if we set a hook for the `small.mar` option as:

{% highlight r %}
hooks$set(small.mar = function(before, options, envir) {
    if (before) par(mar = c(4, 4, .1, .1))  # mar can be smaller when we do not need a main title
})
{% endhighlight %}

Then this function will be called for a chunk like this:

{% highlight r %}
<<myplot, fig=TRUE, small.mar=TRUE>>=
hist(rnorm(100), main = '')  # no main title
@
{% endhighlight %}

To sum up, two conditions must be satisfied for a hook function associated with the option, say, `foo`, to run:

1. the chunk option `foo` is `TRUE` for this chunk;
2. the hook has been set in `hooks` by `hooks$set(foo = FUN)`

## Output hooks