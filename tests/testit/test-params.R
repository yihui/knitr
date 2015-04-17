
library(testit)

# helper function to convert raw src to params list
read_params <- function(src) {
  lines <- strsplit(src, "\n")[[1]]
  knit_params(lines)
}


## test basic parameter parsing

params <- read_params('
---
params:
  x: 10
  y: 20
---
'
)
assert(params[[1]]$name == 'x')
assert(params[[1]]$value == 10)
assert(params[[2]]$name == 'y')
assert(params[[2]]$value == 20)

