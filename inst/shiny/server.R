library(shiny)
library(knitr)
options(device.ask.default = FALSE)

shinyServer(function(input, output) {

  output$nbOut = reactive({
    src = input$nbSrc
    if (length(src) == 0L || src == '') return('Nothing to show yet...')
    owd = setwd(tempdir()); on.exit(setwd(owd))
    opts_knit$set(root.dir = owd)

    paste(knit2html(text = src, fragment.only = TRUE, quiet = TRUE),
          '<script>',
          '// highlight code blocks',
          "$('#nbOut pre code').each(function(i, e) {hljs.highlightBlock(e)});",
          'MathJax.Hub.Queue(["Typeset", MathJax.Hub]); // update MathJax expressions',
          '</script>', sep = '\n')
  })
})
