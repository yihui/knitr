library(testit)

# helper function to convert raw src to params list
read_params = function(src, evaluate = TRUE) {
  lines = strsplit(src, "\n")[[1]]
  knit_params(lines, evaluate = evaluate)
}

# helper function to convert raw src yaml to params list
read_params_yaml = function(src, evaluate = TRUE) {
  knit_params_yaml(src, evaluate = evaluate)
}

params = read_params('
---
params:
  a: 10
  b: 20
---
'
)
assert('basic params parsing works', {
  (params[[1]]$name %==% 'a')
  (params[[1]]$value %==% 10L)
  (params[[2]]$name %==% 'b')
  (params[[2]]$value %==% 20L)
  (flatten_params(params) %==% list(a = 10L, b = 20L))
})

# test date custom type (these deprecated and here for backwards compt) --
params = read_params('
---
params:
  start: !date 2015-01-01
  end: !datetime 2015-01-01 12:30:00
---
'
)
assert('date/time params parsing works', {
  (params[[1]]$name %==% 'start')
  (inherits(params[[1]]$value, 'Date'))
  (params[[1]]$value %==% as.Date("2015-01-01"))
  (params[[2]]$name %==% 'end')
  (inherits(params[[2]]$value, 'POSIXct'))
  (params[[2]]$value %==% as.POSIXct("2015-01-01 12:30:00", tz = "GMT"))
})

params = read_params('
---
params:
  file1:
    value: data1.csv
---
'
)
assert('sub-option params parsing works', {
  (params[[1]]$name %==% 'file1')
  (params[[1]]$value %==% 'data1.csv')
})

params = read_params('
---
params:
  regions:
    value: [North, South]
---
'
)
assert('prams with length > 1 works', {
  (length(params[[1]]$value) %==% 2L)
  (params[[1]]$value[[2]] %==% 'South')
})

## test including additional parameter attributes --------------------------

params = read_params('
---
params:
  regions:
    value: [North, South]
    choices: [North, South, East, West]
    label: "Select Regions"
---
'
)
assert('other types of params can be parsed', {
  (params[[1]]$choices %==% c('North', 'South', 'East', 'West'))
  (params[[1]]$label %==% "Select Regions")
})

params = read_params('
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

assert('y/Y/n/N are not converted to booleans', {
  (unname(unlist(lapply(params, `[[`, 'name'))) %==% c('x', 'y', 'z', 'n', 'Y', 'N'))
})


params = read_params('
---
params:
  today: !r Sys.Date()
  now: !expr Sys.time()
  x: 10
---
'
)
assert('params as expressions can be parsed', {
  (!is.null(params[[1]]$expr))
  (inherits(params[[1]]$value, 'Date'))
  (!is.null(params[[2]]$expr))
  (inherits(params[[2]]$value, 'POSIXct'))
  (is.null(params[[3]]$expr))
})

params = read_params('
---
params:
  today: !r Sys.Date()
---
', evaluate = FALSE)
assert('unevaluated expressions in params can be parsed', {
  (params$today$expr %==% "Sys.Date()")
  (class(params$today$value) %==% "expression")
})

params = read_params_yaml('
params:
  x: 1
  today: !r Sys.Date()
  posixlt: !r strptime("2015-01-01", format = "%Y-%m-%d")
  list: [1,2,3]
#  map: { a: 1, b: 2 }
  map: { value: { a: 1, b: 2 } }
')
# The direct map value is not supported; an explicit value field is necessary
assert('yaml parameters can be handled', {
  (params$x$value %==% 1L)
  (params$today$expr %==% "Sys.Date()")
  (inherits(params$today$value, 'Date'))
  (params$posixlt$expr %==% 'strptime("2015-01-01", format = "%Y-%m-%d")')
  (inherits(params$posixlt$value, 'POSIXlt'))
})
