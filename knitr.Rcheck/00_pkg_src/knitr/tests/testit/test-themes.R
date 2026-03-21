library(testit)

# getting and setting themes

formats = c("markdown", "latex")
for (frm in formats) {
  opts_knit$set(out.format = frm)
  thm = knit_theme$get('acid')
  knit_theme$set(thm)
  assert('knit_theme$get returns theme(s)', {
    (identical(length(knit_theme$get()),
              length(dir(system.file("themes", package = "knitr")))))
    (!is.null(thm))
  })

  assert('knit_theme$set updates highlight and background', {
    (thm$highlight %==% as.character(opts_knit$get('header')[1]))
    (thm$background %==% opts_chunk$get('background'))
  })
  opts_knit$restore()
  opts_chunk$restore()
}

# style -> theme

tl = theme2list('acid.style')
assert('theme2list parsing from .style to CSS', {
  (tl$Description %==% "Acid")
  (tl$Canvas$Colour %==% "#eeeeee")
  (isTRUE(tl$Number$Bold))
  (is.null(tl$Keywords))
})

# theme -> css

css = list2css(tl)
assert('css is generated for a theme', {
  (grepl(tl$Canvas$Colour, css[1]))
  (grepl("font-weight: bold;", css[2]))
  (grepl(tl$String$Colour, css[3]))
})

fdir = file.path(tempdir(), "themes")
dir.create(fdir)
res = lapply(1:2, function(i) {
  fn = paste0(fdir, "/acid", i, '.theme', collapse = "")
  writeLines(readLines('acid.style'), con = fn)
  return()
})

cssdir = file.path(tempdir(), "css")
dir.create(cssdir)
suppressMessages(themes2css(fdir, cssdir))
assert('themes2css creates two css files from test data', {
  (length(dir(cssdir)) == 2)
  (dir(cssdir) %==% c("acid1.css", "acid2.css"))
})
