<!--roptions dev='png', fig.width=5, fig.height=5 -->
This is a miminal example of using **knitr** with in HTML pages. I am actually using markdown here since it is more convenient in GitHub.

First, the input file was named as `knitr-minimal_knit_.md` ([click to view its source](https://github.com/yihui/knitr/raw/master/inst/examples/knitr-minimal_knit_.md)), which contains `_knit_` so that **knitr** will automatically determine the output filename to be `knitr-minimal.md`. 

I used the code below to make sure **knitr** uses the correct hooks to wrap my output, and writes correct URL's for my images.

<!--begin.rcode setup
render_gfm() # use GFM hooks for output
opts_knit$set(base.url='https://github.com/yihui/knitr/raw/master/inst/examples/')
end.rcode-->

Now we write some code chunks in this markdown file:

<!--begin.rcode
## a simple calculator
1+1
## boring random numbers
set.seed(123)
rnorm(5)
end.rcode-->

We can also produce plots:

<!--begin.rcode md-cars-scatter, message=FALSE
library(ggplot2)
qplot(hp, mpg, data=mtcars)+geom_smooth()
end.rcode-->

So no more hesitation on using GitHub and **knitr**! You just write a minimal amount of code to get beautiful output on the web.
