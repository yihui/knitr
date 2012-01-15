---
layout: default
title: Code Externalization
subtitle: Use an external R script with your document
category: demo
---

You do not have to put the R code in the input document; with **knitr**, you can separate your input document with the R script (think the reverse of Stangle).

There are several advantages of separating the main document with the R script(s), e.g., R code can be reusable across several documents, and you can run the R code continuously in a separate file (if you embed the code in the document, you often have to jump through texts); this feature is especially useful for LyX users, and it saves a huge amount of time since you do not have to re-compile the whole document to see the results; instead, you can tune your R code freely in another R session.

The function `read_chunk()` was designed for this feature. For example, if the R code is in [Stat579-HW8-Xie.R](https://github.com/downloads/yihui/knitr/Stat579-HW8-Xie.R), you can use `read_chunk('Stat579-HW8-Xie.R')` to read the code into the input document in an early chunk. It is possible to use multiple external R scripts (just call `read_chunk()` for multiple times).

- A homework solution
  - Rnw source: [Stat579-HW8-Xie.Rnw](https://github.com/downloads/yihui/knitr/Stat579-HW8-Xie.Rnw)
  - R source: [Stat579-HW8-Xie.R](https://github.com/downloads/yihui/knitr/Stat579-HW8-Xie.R)
  - PDF output: [Stat579-HW8-Xie.pdf](https://github.com/downloads/yihui/knitr/Stat579-HW8-Xie.pdf)
- Another homework
  - Rnw source: [Stat546-HW4-Xie.Rnw](https://github.com/downloads/yihui/knitr/Stat546-HW4-Xie.Rnw)
  - R source: [Stat546-HW4-Xie.R](https://github.com/downloads/yihui/knitr/Stat546-HW4-Xie.R)
  - data source: [GOOG2010.csv](https://github.com/downloads/yihui/knitr/GOOG2010.csv)
  - PDF output: [Stat546-HW4-Xie.pdf](https://github.com/downloads/yihui/knitr/Stat546-HW4-Xie.pdf)
