\name{asis_output}
\alias{asis_output}
\title{Mark an R object with a special class}
\usage{
asis_output(x)
}
\arguments{
\item{x}{an R object (typically a character string, or can be converted to a
character string via \code{\link{as.character}()})}
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
}
\examples{
# see ?knit_print
}
