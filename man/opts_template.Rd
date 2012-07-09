\docType{data}
\name{opts_template}
\alias{opts_template}
\title{Template for creating reusable chunk options.
A label can be used to define a set of chunk options, which
can be reused by just referencing the label again in all
chunks that need it.}
\format{List of 4
 $ get    :function (name, default = FALSE)  
 $ set    :function (...)  
 $ merge  :function (values)  
 $ restore:function ()}
\usage{
  opts_template
}
\description{
  Template for creating reusable chunk options. A label can
  be used to define a set of chunk options, which can be
  reused by just referencing the label again in all chunks
  that need it.
}
\examples{
opts_template$set(list('myfigures' = list(fig.height = 4, fig. width = 4)))
}
\keyword{datasets}

