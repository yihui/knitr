\docType{data}
\name{knit_patterns}
\alias{knit_patterns}
\title{Patterns to match and extract R code in a document}
\format{List of 4
 $ get    :function (name, default = FALSE)  
 $ set    :function (...)  
 $ merge  :function (values)  
 $ restore:function ()}
\usage{
knit_patterns
}
\description{
  Patterns are regular expressions and will be used in
  functions like \code{\link[base]{grep}} to extract R code
  and chunk options. The object \code{knit_patterns}
  controls the patterns currently used; see the references
  and examples for usage.
}
\examples{
library(knitr)
opat = knit_patterns$get()  # old pattern list (to restore later)

apats = opts_knit$get("all.patterns")  # a list of all built-in patterns
str(apats)
knit_patterns$set(apats[["rnw"]])  # set pattern list from apats

knit_patterns$get(c("chunk.begin", "chunk.end", "inline.code"))

## a customized pattern list; has to empty the original
#   patterns first!
knit_patterns$restore()
## we may want to use this in an HTML document
knit_patterns$set(list(chunk.begin = "<!--helloR\\\\s+(.*)", 
    chunk.end = "^byeR-->"))
str(knit_patterns$get())

knit_patterns$set(opat)  # put the old patterns back
}
\references{
  Usage: \url{http://yihui.name/knitr/objects}

  Components in \code{knit_patterns}:
  \url{http://yihui.name/knitr/patterns}
}
\keyword{datasets}
