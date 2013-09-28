\docType{data}
\name{knit_hooks}
\alias{knit_hooks}
\title{Hooks for R code chunks, inline R code and output}
\format{List of 4
 $ get    :function (name, default = FALSE)  
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 4 9 9 3 9 3 4 9
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x0000000009590e88> 
 $ set    :function (...)  
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 10 9 17 3 9 3 10 17
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x0000000009590e88> 
 $ merge  :function (values)  
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 18 11 18 55 11 55 18 18
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x0000000009590e88> 
 $ restore:function (target = value)  
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 19 13 19 56 13 56 19 19
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x0000000009590e88>}
\usage{
  knit_hooks
}
\description{
  A hook is a function of a pre-defined form (arguments)
  that takes values of arguments and returns desired
  output. The object \code{knit_hooks} is used to access or
  set hooks in this package.
}
\examples{
knit_hooks$get('source'); knit_hooks$get('inline')
}
\references{
  Usage: \url{http://yihui.name/knitr/objects}

  Components in \code{knit_hooks}:
  \url{http://yihui.name/knitr/hooks}
}
\keyword{datasets}

