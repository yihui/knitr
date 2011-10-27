\docType{data}
\name{kpat}
\alias{kpat}
\title{Patterns to match and extract R code in a document}
\format{List of 4
 $ get    :function (name, default = FALSE)  
 $ set    :function (...)  
 $ merge  :function (values)  
 $ restore:function ()}
\usage{
kpat
}
\description{
  Patterns are regular expressions and will be used in
  functions like \code{\link[base]{grep}} to extract R code
  and chunk options. The object \code{kpat} controls the
  patterns currently used; see the references and examples
  for usage.
}
\examples{
library(knitr)
opat = kpat$get()  # old pattern list (to restore later)

apats = optk$get("all.patterns")  # a list of all built-in patterns
str(apats)
kpat$set(apats[["rnw"]])  # set pattern list from apats

kpat$get(c("chunk.begin", "chunk.end", "inline.code"))

## a customized pattern list; has to empty the original
#   patterns first!
kpat$restore()
## we may want to use this in an HTML document
kpat$set(list(chunk.begin = "<!--helloR\\\\s+(.*)", 
    chunk.end = "^byeR-->"))
str(kpat$get())

kpat$set(opat)  # put the old patterns back
}
\references{
  Usage: \url{http://yihui.github.com/knitr/objects}

  Components in \code{kpat}:
  \url{http://yihui.github.com/knitr/patterns}
}
\keyword{dataset}
