library(shiny)
options(device.ask.default = FALSE)

shinyServer(function(input, output) {

  output$nbOut = reactive(function() {
    src = input$nbSrc
    library(knitr)
    if (length(src) == 0L || src == '')
      return('Nothing to show yet...')
    on.exit(unlink('figure/', recursive = TRUE)) # do not need the figure dir
    paste(try(knit2html(text = src, fragment.only = TRUE)),
          '<script>',
          '// highlight code blocks',
          "$('#nbOut pre code').each(function(i, e) {hljs.highlightBlock(e)});",
          'MathJax.Hub.Typeset(); // update MathJax expressions',
          '</script>', sep = '\n')
  })
})
