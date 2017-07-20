library(testit)

opts_chunk$append(list(fig.path=c('mypath/')))

assert(
  'opts_chunk$append() returns appended chunk option',
  identical(opts_chunk$get('fig.path'),c('figure/','mypath/'))
)

opts_chunk$restore()
