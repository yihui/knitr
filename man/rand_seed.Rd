\docType{data}
\name{rand_seed}
\alias{rand_seed}
\title{An unevaluated expression to return .Random.seed if exists}
\format{language {  if (exists(".Random.seed", envir = globalenv())) ; get(".Random.seed", envir = globalenv()) }}
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
rnorm(1)  # .Random.seed is created (or modified)
eval(rand_seed)
}
\references{
  \url{http://yihui.name/knitr/demo/cache/}
}
\keyword{datasets}
