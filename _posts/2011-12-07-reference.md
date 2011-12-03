---
layout: default
title: Code Reference
subtitle: Use an external R script with your document
category: demo
---

You do not have to put the R code in the input document; with **knitr**, you can separate your input document with the R script (think the reverse of Stangle).

There are several advantages of separating the main document with the R script(s), e.g., R code can be reusable across several documents, and you can run the R code continuously in a separate file (if you embed the code in the document, you often have to jump through texts); this feature is especially useful for LyX users, and it saves a huge amount of time since you do not have to re-compile the whole document to see the results; instead, you can tune your R code freely in another R session.

The key to use code reference is the `ref` option; it specifies a filename, e.g. `ref=Stat579-HW8-Xie.R`. See the [options](/knitr/options) page for details. Since this is a chunk option, it is possible for chunks to use different R scripts.

- A homework solution
  - Rnw source: [Stat579-HW8-Xie.Rnw](https://github.com/downloads/yihui/knitr/Stat579-HW8-Xie.Rnw)
  - R source: [Stat579-HW8-Xie.R](https://github.com/downloads/yihui/knitr/Stat579-HW8-Xie.R)
  - PDF output: [Stat579-HW8-Xie.pdf](https://github.com/downloads/yihui/knitr/Stat579-HW8-Xie.pdf)
