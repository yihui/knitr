---
layout: default
title: Chunk Reference/Macro
subtitle: How to reuse chunks
category: demo
---

Sweave has the feature of chunk reference to reuse chunks with the syntax `<<chunk-label>>` (without `=` compared to `<<>>=`), e.g.

{% highlight r %}
<<chunk1>>=
1 + 1
@

<<chunk2>>=
<<chunk1>>
@
{% endhighlight %}

In `chunk2`, the code in `chunk1` will be inserted. This feature is also available in **knitr**, but note **knitr** supports arbitrary (finite) levels of recursion in chunk references (Sweave only supports one level), i.e. one chunk can reference another chunk which references yet another chunk.

There are still other approaches to reuse chunks in **knitr**.

1. use the same label as the previous chunk to be reused
1. use the chunk option `ref.label` to reference another chunk

## Use the same label

An example for the first approach:

{% highlight r %}
<<chunk1, echo=TRUE, results='hide'>>=
1 + 1
@

<<chunk1, echo=FALSE, results='markup'>>=
@
{% endhighlight %}

The second chunk is empty, so **knitr** will look for another chunk with the same label but is not empty, and use the code from that chunk. The key is to leave a chunk empty to use code from other chunks. One problem with this approach is that you cannot cache both chunks since their MD5 digests are different, and **knitr** only allows one set of cache files per label.

## Use chunk option `ref.label`

An example for the second approach:

{% highlight r %}
<<chunk1, echo=TRUE, results='hide'>>=
1 + 1
@

<<chunk2, ref.label='chunk1', echo=FALSE, results='markup'>>=
@
{% endhighlight %}

The second chunk uses a different label, so cache is no longer a problem. Obviously the second approach is a more general solution.

This feature enables us to separate R code and R output in the output document. For instance, we can use `echo=FALSE` in the body of an article to hide R code, and use chunk references in the appendix to show R code (with `eval=FALSE, ref.label=...`).
