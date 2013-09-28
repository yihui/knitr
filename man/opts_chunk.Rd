\docType{data}
\name{opts_chunk}
\alias{opts_chunk}
\alias{opts_current}
\title{Default and current chunk options}
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
  opts_chunk

  opts_current
}
\description{
  Options for R code chunks. When running R code, the
  object \code{opts_chunk} (default options) is not
  modified by chunk headers (local chunk options are merged
  with default options), whereas \code{opts_current}
  (current options) changes with different chunk headers
  and it always reflects the options for the current chunk.
}
\details{
  Normally we set up the global options once in the first
  code chunk in a document using \code{opts_chunk$set()},
  so that all \emph{latter} chunks will use these options.
  Note the global options set in one chunk will not affect
  the options in this chunk itself, and that is why we
  often need to set global options in a separate chunk.
}
\examples{
opts_chunk$get('prompt'); opts_chunk$get('fig.keep')
}
\references{
  Usage: \url{http://yihui.name/knitr/objects}

  A list of available options:
  \url{http://yihui.name/knitr/options#chunk_options}
}
\keyword{datasets}

