---
layout: default
title: Code Externalization
subtitle: Use an external R script with your document
category: demo
---

You do not have to put the R code in the input document; with **knitr**, you can separate your input document with the R script (think the reverse of Stangle).

There are several advantages of separating the main document with the R script(s), e.g., R code can be reusable across several documents, and you can run the R code continuously in a separate file (if you embed the code in the document, you often have to jump through texts); this feature is especially useful for LyX users, and it saves a huge amount of time since you do not have to re-compile the whole document to see the results; instead, you can tune your R code freely in another R session.

The function `read_chunk()` was designed for this feature. For example, if the R code is in [Stat579-HW8-Xie.R](https://bitbucket.org/stat/knitr/downloads/Stat579-HW8-Xie.R), you can use `read_chunk('Stat579-HW8-Xie.R')` to read the code into the input document in an early chunk. In the R script, you have to annotate the code with comments of the form `## ---- label` or `## @knitr label`, where `label` is the chunk label that you use in the report, e.g. if you have a line `## ---- test` in the script, you should also have a chunk `<<test>>=` in the report, and **knitr** will match the labels and insert external code.

It is possible to use multiple external R scripts in an input document (just call `read_chunk()` for multiple times), or share a single script across multiple input documents (all of them read this script).

Note this function should only be used in an _uncached_ chunk, e.g.

{% highlight r %}
<<external-code, cache=FALSE>>=
read_chunk('foo-bar.R')
@
{% endhighlight %}

Some examples:

- A homework solution
  - Rnw source: [Stat579-HW8-Xie.Rnw](https://bitbucket.org/stat/knitr/downloads/Stat579-HW8-Xie.Rnw)
  - R source: [Stat579-HW8-Xie.R](https://bitbucket.org/stat/knitr/downloads/Stat579-HW8-Xie.R)
  - PDF output: [Stat579-HW8-Xie.pdf](https://bitbucket.org/stat/knitr/downloads/Stat579-HW8-Xie.pdf)
  - LyX source: [Stat579-HW8-Xie.lyx](https://bitbucket.org/stat/knitr/downloads/Stat579-HW8-Xie.lyx)
- Another homework
  - Rnw source: [Stat546-HW4-Xie.Rnw](https://bitbucket.org/stat/knitr/downloads/Stat546-HW4-Xie.Rnw)
  - R source: [Stat546-HW4-Xie.R](https://bitbucket.org/stat/knitr/downloads/Stat546-HW4-Xie.R)
  - data source: [GOOG2010.csv](https://bitbucket.org/stat/knitr/downloads/GOOG2010.csv)
  - PDF output: [Stat546-HW4-Xie.pdf](https://bitbucket.org/stat/knitr/downloads/Stat546-HW4-Xie.pdf)
  - LyX source: [Stat546-HW4-Xie.lyx](https://bitbucket.org/stat/knitr/downloads/Stat546-HW4-Xie.lyx)
