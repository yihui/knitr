library(testit)

knitr::opts_chunk$append(list(fig.path=c('mypath/')))

assert(
  'knitr::opts_chunk$append() returns appended chunk option',
  identical(knitr::opts_chunk$get('fig.path'),c('figure/','mypath/'))
)
