\docType{data}
\name{opts_template}
\alias{opts_template}
\title{Template for creating reusable chunk options}
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
  opts_template
}
\description{
  Creates a template binding a label to a set of chunk
  options. Every chunk that references the template label
  will have the specificed set of options applied to it.
}
\examples{
opts_template$set(myfigures = list(fig.height = 4, fig.width = 4))
# later you can reuse these chunk options by 'opts.label', e.g.

# <<foo, opts.label='myfigures'>>=

# the above is equivalent to <<foo, fig.height=4, fig.width=4>>=
}
\keyword{datasets}

