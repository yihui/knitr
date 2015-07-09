library(testit)

# helper function to convert raw src to params list
read_params <- function(src) {
  lines <- strsplit(src, "\n")[[1]]
  knit_params(lines)
}


## test basic parameter parsing --------------------------------------------

params <- read_params('
---
params:
  a: 10
  b: 20
---
'
)
assert(params[[1]]$name == 'a')
assert(params[[1]]$value == 10)
assert(params[[2]]$name == 'b')
assert(params[[2]]$value == 20)

assert(identical(flatten_params(params), list(a = 10L, b = 20L)))

## test date custom type ---------------------------------------------------

params <- read_params('
---
params:
  start: !date 2015-01-01
  end: !datetime 2015-01-01 12:30:00
---
'
)
assert(params[[1]]$name == 'start')
assert(params[[1]]$type == 'date')
assert(params[[1]]$value == as.Date("2015-01-01"))
assert(params[[2]]$name == 'end')
assert(params[[2]]$type == 'datetime')
assert(params[[2]]$value == as.POSIXct("2015-01-01 12:30:00", tz = "GMT"))


## test file custom type ---------------------------------------------------

params <- read_params('
---
params:
  file: !file data.csv
---
'
)
assert(params[[1]]$name == 'file')
assert(params[[1]]$type == 'file')
assert(params[[1]]$value == 'data.csv')


## test specifying value in sub-object and type at object level ------------

params <- read_params('
---
params:
  file1:
    value: !file data1.csv
  file2: !file
    value: data2.csv
---
'
)
assert(params[[1]]$name == 'file1')
assert(params[[1]]$type == 'file')
assert(params[[1]]$value == 'data1.csv')
assert(params[[2]]$name == 'file2')
assert(params[[2]]$type == 'file')
assert(params[[2]]$value == 'data2.csv')


## test parameters with length(value) > 1 ----------------------------------

params <- read_params('
---
params:
  regions:
    value: [North, South]
---
'
)
assert(length(params[[1]]$value) == 2)
assert(params[[1]]$value[[2]] == 'South')


## test including additional parameter attributes --------------------------

params <- read_params('
---
params:
  regions:
    value: [North, South]
    choices: [North, South, East, West]
    label: "Select Regions"
---
'
)
assert(identical(params[[1]]$choices, c('North', 'South', 'East', 'West')))
assert(params[[1]]$label == "Select Regions")


## test y/Y/n/N ------------------------------------------------------------

params <- read_params('
---
params:
  x: 1
  y: 2
  z: 3
  n: 4
  Y: 5
  N: 6
---
'
)

assert(
  'y/Y/n/N are not converted to booleans',
  identical(unlist(lapply(params, `[[`, 'name')), c('x', 'y', 'z', 'n', 'Y', 'N'))
)


## test handling of expressions --------------------------------------------

params <- read_params('
---
params:
  today: !expr Sys.Date()
  now: !expr Sys.time()
  datevar: !date 2015-07-09
---
'
)
assert(params[[1]]$expr)
assert(params[[1]]$type == "date")
assert(params[[2]]$expr)
assert(params[[2]]$type == "datetime")
assert(!params[[3]]$expr)
assert(params[[1]]$type == "date")





