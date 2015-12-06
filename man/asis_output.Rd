% Please edit documentation in R/output.R
\name{asis_output}
\alias{asis_output}
\title{Mark an R object with a special class}
\usage{
asis_output(x, meta = NULL, cacheable = length(meta) == 0)
}
\arguments{
\item{x}{an R object (typically a character string, or can be converted to a
character string via \code{\link{as.character}()})}

\item{meta}{additional metadata of the object to be printed (the metadata
will be collected when the object is printed, and accessible via
\code{knit_meta()})}

\item{cacheable}{a logical value indicating if this object is cacheable}
}
\description{
This is a convenience function that assigns the input object a class named
\code{knit_asis}, so that \pkg{knitr} will treat it as is (the effect is the
same as the chunk option \code{results = 'asis'}) when it is written to the
output.
}
\details{
This function is normally used in a custom S3 method based on the printing
function \code{\link{knit_print}()}.

For the \code{cacheable} argument, you need to be careful when printing the
object involves non-trivial side effects, in which case it is strongly
recommended to use \code{cacheable = FALSE} to instruct \pkg{knitr} that this
object should not be cached using the chunk option \code{cache = TRUE},
otherwise the side effects will be lost the next time the chunk is knitted.
For example, printing a \pkg{shiny} input element in an R Markdown document
may involve registering metadata about some JavaScript libraries or
stylesheets, and the metadata may be lost if we cache the code chunk, because
the code evaluation will be skipped the next time.
}
\note{
This function only works in top-level R expressions, and it will not
  work when it is called inside another expression, such as a for-loop. See
  \url{https://github.com/yihui/knitr/issues/1137} for a discussion.
}
\examples{
# see ?knit_print
}
