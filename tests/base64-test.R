require(knitr)
require(markdown)
require(RCurl)

test_file <- "run-all.R"
system.time(result.markdown <- knitr:::base64_encoder.wrapper("markdown")(test_file))
system.time(result.RCurl <- knitr:::base64_encoder.wrapper("RCurl")(test_file))
system.time(result.utils <- knitr:::base64_encoder.wrapper("utils")(test_file))
system.time(result <- knitr:::base64_encode(test_file))
if (result.markdown != result.RCurl || result.markdown != result.utils || result.markdown != result) {
  stop("Inconsistant base64 encoding")
}

opts_knit$set(upload.fun = knitr:::base64_encode )
test.Rmd <- "Title
========================================================

This is an R Markdown document. Markdown is a simple formatting syntax for authoring web pages (click the **MD** toolbar button for help on Markdown).

When you click the **Knit HTML** button a web page will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r}
summary(cars)
```

You can also embed plots, for example:

```{r fig.width=7, fig.height=6,}
plot(cars)
```

"
cat(test.Rmd,file="test.Rmd")
knit("test.Rmd","test.md")
if (length(grep("unnamed-chunk-2.png",readLines("test.md"),fixed=TRUE))) {
  stop("Does not return HTML image tag")
}
markdownToHTML('test.md', 'test.html')
