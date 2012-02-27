---
layout: default
title: Chunk Reference
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

There are still three additional approaches to reuse chunks in **knitr**.

1. use the same label as the previous chunk to be reused
1. use the chunk option `ref.label` to reference another chunk
1. use the function `run_chunk()` to run the code from other chunks

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

## Use `run_chunk()`

Both above approaches only allow reusing complete chunks, i.e. you cannot reuse one chunk inside another one as a _part_ of it. To solve this problem, we introduced `run_chunk('foo-label')`, which acts as if the code were copied and pasted here from another chunk with label `foo-label`. The code will be evaluated in the environment in which `run_chunk()` is put (for technical details, see `?parent.frame`).

Here is an exampel of three chunks labelled `a`, `b` and `c`:

{% highlight r %}
<<a>>=
x = 1
x
run_chunk('b')
x
@
<<b>>=
x = 2
@
<<c>>=
run_chunk('a')
x
@
{% endhighlight %}

Note the code in chunk `c` is actually a recursive call: `c` calls `a` which in turn calls `b`. Like the `<<>>` approach, the level of recursion can be arbitrary, but you have to make sure there is no infinite recursion (e.g. `a` calls `b` and `b` also calls `a`).

Guess what are the values of `x` in each chunk?

One drawback of this approach is that we are unable to show the real source code in the place of `run_chunk()`; this function can only evaluate code. However, the previous two approaches can insert the real source code from other chunks to the current chunk.
