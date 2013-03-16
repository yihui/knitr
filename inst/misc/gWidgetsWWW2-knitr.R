library(gWidgetsWWW2)
w = gwindow('An R Notebook')
sb = gstatusbar('Powered by Rook, gWidgetsWWW2 and knitr', container = w)
g = gframe('An R Notebook based on knitr', use.scrollwindow = TRUE,
           horizontal = FALSE, container = w)
gbutton('Knit', container = g, handler = function(h, ...) {
  library(knitr)
  svalue(g2) = try(knit2html(text = svalue(g1), fragment.only = TRUE))
})
g0 = ggroup(container = g)
code = readLines(system.file('examples', 'knitr-minimal.Rmd', package = 'knitr'))
# g1 = gtext(code, container = g, use.codemirror = TRUE, width = 500)
g1 = gtext(code, container = g0, height = 500, expand = TRUE)
g2 = ghtml('results here', container = g0, expand = TRUE)
