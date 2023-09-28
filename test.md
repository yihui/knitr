---
title: "Example"
format: pdf
engine: knitr
editor: visual
---


```r
# install.packages(c("DBI", "RSQLite"))
db = DBI::dbConnect(RSQLite::SQLite(), ":memory:")
```



