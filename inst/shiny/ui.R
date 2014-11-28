library(shiny)

# div nbSrc takes source code from div notebook (via Ace editor), and div nbOut
# holds results from knitr
shinyUI(
  bootstrapPage(
    tags$head(
      tags$title('An R Notebook in Shiny'),
      tags$script(src = 'http://ajaxorg.github.io/ace/build/src-min-noconflict/ace.js',
                  type = 'text/javascript', charset = 'utf-8'),
      tags$link(rel = 'stylesheet', type = 'text/css', href = 'ace-shiny.css'),
      tags$script(src = '//cdnjs.cloudflare.com/ajax/libs/highlight.js/8.3/highlight.min.js', type = 'text/javascript'),
      tags$script(src = '//cdnjs.cloudflare.com/ajax/libs/highlight.js/8.3/languages/r.min.js', type = 'text/javascript'),
      tags$link(rel = 'stylesheet', type = 'text/css',
                href = '//cdnjs.cloudflare.com/ajax/libs/highlight.js/8.3/styles/github.min.css'),
      tags$script(src = '//cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML',
                  type = 'text/javascript')
    ),
    div(id = 'notebook', title = 'Compile notebook: F4\nInsert chunk: Ctrl+Alt+I',
        paste(c('This is an example taken from the **knitr** package. Press `F4` or `Ctrl+Shift+H` to compile it, and `Ctrl+Alt+I` to insert a code chunk.', '',
                readLines(system.file('examples', 'knitr-minimal.Rmd', package = 'knitr'))), collapse = '\n')),
    tags$textarea(id = 'nbSrc', style = 'display: none;'),
    tags$script(src = 'ace-shiny.js', type = 'text/javascript'),
    htmlOutput('nbOut'),
    div(id = 'proxy', submitButton('Knit Notebook'))
  )
)
