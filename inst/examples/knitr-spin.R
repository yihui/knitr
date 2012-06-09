#' This is a special R script which can be used to generate a report. You can
#' write normal text in roxygen comments.
#' 
#' First we set up some options (you do not have to do this):

#+ setup, include=FALSE
library(knitr)
opts_chunk$set(fig.path='figure/silk-')

#' The report begins here.

#+ test-a, cache=FALSE
# boring examples as usual
set.seed(123)
x = rnorm(5)
mean(x)

#' Now we continue writing the report. We can draw plots as well.

#+ test-b, fig.width=5, fig.height=5
par(mar = c(4, 4, .1, .1)); plot(x)

#' Actually you do not have to write chunk options, in which case knitr will use
#' default options. For example, the code below has no options attached:

var(x)
quantile(x)

#' And you can also write two chunks successively like this:

#+ test-chisq5
sum(x^2) # chi-square distribution with df 5
#+ test-chisq4
sum((x - mean(x))^2) # df is 4 now

#' Done. Call spin('knitr-spin.R') to make silk from sow's ear now and knit a
#' lovely purse.