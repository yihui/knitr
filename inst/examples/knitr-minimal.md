This is a minimal example of using **knitr** with in HTML pages. I am actually
using markdown here since it is more convenient in GitHub.

First, the input file was named as `knitr-minimal.Rmd`
([source](https://github.com/yihui/knitr/blob/master/inst/examples/knitr-minimal.Rmd)),
and **knitr** will automatically determine the output filename to be
`knitr-minimal.md` (`*.Rmd --> *.md`).


```r
# set global chunk options: images will be 5x5 inches
opts_chunk$set(fig.width = 5, fig.height = 5)
```


Now we write some code chunks in this markdown file:


```r
## a simple calculator
(x <- 1 + 1)
```

```
## [1] 2
```

```r
## boring random numbers
set.seed(123)
rnorm(5)
```

```
## [1] -0.56048 -0.23018  1.55871  0.07051  0.12929
```


We can also produce plots:


```r
library(ggplot2)
qplot(hp, mpg, data = mtcars) + geom_smooth()
```

![plot of chunk md-cars-scatter](https://github.com/yihui/knitr/raw/master/inst/examples/md-cars-scatter.png) 


Inline R code is also supported, e.g. the value of `x` is `2`, and 2 times pi
= `6.2832`.

So no more hesitation on using GitHub and **knitr**! You just write a minimal
amount of code to get beautiful output on the web.
