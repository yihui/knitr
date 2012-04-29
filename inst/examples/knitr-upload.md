
This is an example of using **knitr** with extended markdown (e.g.
[GFM](http://github.github.com/github-flavored-markdown/)) and uploading images
to imgur.com automatically. Note you should set the graphical device to create
images that can be displayed in the web browser, e.g. `dev = 'png'` (it is the
default for markdown output) works but `'pdf'` does not.

First, the input file was named as `knitr-upload.Rmd`
([source](https://github.com/yihui/knitr/blob/master/inst/examples/knitr-upload.Rmd)),
and **knitr** will automatically determine the output filename to be
`knitr-upload.md`. 

I used the code below to make sure **knitr** will upload images.



```r
opts_knit$set(upload.fun = imgur_upload)  # upload all images to imgur.com
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




We can also produce plots which are uploaded to imgur.com:



```r
library(ggplot2)
qplot(hp, mpg, data = mtcars) + geom_smooth()
```

![plot of chunk md-cars](http://i.imgur.com/mBRVS.png) 

```r
ggpcp(mtcars) + geom_line()
```

![plot of chunk md-cars](http://i.imgur.com/mSEgC.png) 


So **knitr** is ready with GitHub with a single markdown file.
