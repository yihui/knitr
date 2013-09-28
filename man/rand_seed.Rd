\docType{data}
\name{rand_seed}
\alias{rand_seed}
\title{An unevaluated expression to return .Random.seed if exists}
\format{length 2 {  if (exists(".Random.seed", envir = globalenv())) ; get(".Random.seed", envir = globalenv()) }
 - attr(*, "srcref")=List of 2
 - attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x000000000645d618> 
 - attr(*, "wholeSrcref")=Class 'srcref'  atomic [1:8] 1 0 184 1 0 1 1 184
  .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x000000000645d618>}
\usage{
  rand_seed
}
\description{
  This expression returns \code{.Random.seed} when
  \code{eval(rand_seed)} and \code{NULL} otherwise.
}
\details{
  It is designed to work with
  \code{opts_knit$set(cache.extra = rand_seed)} for
  reproducibility of chunks that involve with random number
  generation. See references.
}
\examples{
eval(rand_seed)
rnorm(1) # .Random.seed is created (or modified)
eval(rand_seed)
}
\references{
  \url{http://yihui.name/knitr/demo/cache/}
}
\keyword{datasets}

