
This is a minimal example of using **knitr** with in HTML pages. I am actually
using markdown here since it is more convenient in GitHub.

First, the input file was named as `knitr-minimal.Rmd`
([source](https://github.com/yihui/knitr/blob/master/inst/examples/knitr-minimal.Rmd)),
and **knitr** will automatically determine the output filename to be
`knitr-minimal.md` (`*.Rmd --> *.md`).

I used the code below to make sure **knitr** writes correct URL's for my images.



```r
opts_knit$set(base.url = "https://github.com/yihui/knitr/raw/master/inst/examples/")
```




Now we write some code chunks in this markdown file:



```r
## a simple calculator
1 + 1
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


So no more hesitation on using GitHub and **knitr**! You just write a minimal
amount of code to get beautiful output on the web.
