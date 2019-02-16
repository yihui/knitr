library(testit)

# getting and setting themes

formats <- c("markdown", "latex")
for (format in formats) {
  opts_knit$set(out.format = format)
  thm <- knit_theme$get('acid')
  knit_theme$set(thm)
  assert(
    'knit_theme$get returns theme(s)',
    identical(length(knit_theme$get()),
              length(dir(system.file("themes", package = "knitr")))),
    !is.null(thm)
  )

  assert(
    'knit_theme$set updates highlight and background',
    thm$highlight %==% as.character(opts_knit$get('header')[1]),
    thm$background %==% opts_chunk$get('background')
  )
}

# style -> theme

acid <- "\nDescription=\"Acid\"\n\nDefault        = { Colour=\"#000000\" }\nCanvas         = { Colour=\"#eeeeee\" }\nNumber         = { Colour=\"#800080\", Bold=true }\nEscape         = { Colour=\"#ff00ff\", Bold=true }\nString         = { Colour=\"#a68500\" }\nStringPreProc  = { Colour=\"#0000ff\" }\nBlockComment   = { Colour=\"#ff8000\" }\nLineComment    = { Colour=\"#f27900\" }\nPreProcessor   = { Colour=\"#0080c0\", Bold=true }\nLineNum        = { Colour=\"#303030\" }\nOperator       = { Colour=\"#ff0080\", Bold=true }\nInterpolation  = { Colour=\"#9E5DFF\" }\n\nKeywords = {\n  { Colour= \"#bb7977\", Bold=true },\n  { Colour= \"#8080c0\", Bold=true },\n  { Colour= \"#0080c0\" },\n  { Colour= \"#004466\" },\n}\n"
fn <- tempfile(fileext = '.style')
writeLines(acid, con = fn)
tl <- theme2list(fn)
assert(
  'theme2list parsing from .style to CSS',
  tl$Description %==% "Acid",
  tl$Canvas$Colour %==% "#eeeeee",
  isTRUE(tl$Number$Bold),
  is.null(tl$Keywords)
)

# theme -> css

css <- list2css(tl)
assert(
  'css is generated for a theme',
  grepl(tl$Canvas$Colour, css[1]),
  grepl("font-weight: bold;", css[2]),
  grepl(tl$String$Colour, css[3])
)

dir.create(fdir <- file.path(tempdir(), "themes"))
res <- lapply(1:2, function(i) {
  fn <- paste0(fdir, "/acid", i, '.theme', collapse = "")
  writeLines(acid, con = fn)
  return()
})

dir.create(cssdir <- file.path(tempdir(), "css"))
suppressMessages(themes2css(fdir, cssdir))
assert(
  length(dir(cssdir)) == 2,
  dir(cssdir) %==% c("acid1.css", "acid2.css")
)
