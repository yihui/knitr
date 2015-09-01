library(testit)

knit('knit-math.Rnw', quiet=TRUE)

lines <- readLines('knit-math.tex')
envBounds <- which(grepl('bmatrix', lines) | grepl('align', lines))
assert('no extra newlines',
       tail(diff(envBounds),n=1) == 1)

file.remove('knit-math.tex')
