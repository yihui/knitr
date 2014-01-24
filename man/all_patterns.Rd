\docType{data}
\name{all_patterns}
\alias{all_patterns}
\title{All built-in patterns}
\format{\preformatted{List of 8
 $ rnw     :List of 7
  ..$ chunk.begin   : chr "^\\\\s*<<(.*)>>=.*$"
  ..$ chunk.end     : chr "^\\\\s*@\\\\s*(\%+.*|)$"
  ..$ inline.code   : chr "\\\\\\\\Sexpr\\\\\{([^\}]+)\\\\\}"
  ..$ inline.comment: chr "^\\\\s*\%.*"
  ..$ ref.chunk     : chr "^\\\\s*<<(.+)>>\\\\s*$"
  ..$ header.begin  : chr "(^|\\n)[^\%]*\\\\s*\\\\\\\\documentclass[^\}]+\\\\\}"
  ..$ document.begin: chr "\\\\s*\\\\\\\\begin\\\\\{document\\\\\}"
 $ brew    :List of 1
  ..$ inline.code: chr "<\%[=]\{0,1\}\\\\s+([^\%]+)\\\\s+[-]*\%>"
 $ tex     :List of 8
  ..$ chunk.begin   : chr "^\\\\s*\%+\\\\s*begin.rcode\\\\s*(.*)"
  ..$ chunk.end     : chr "^\\\\s*\%+\\\\s*end.rcode"
  ..$ chunk.code    : chr "^\%+"
  ..$ ref.chunk     : chr "^\%+\\\\s*<<(.+)>>\\\\s*$"
  ..$ inline.comment: chr "^\\\\s*\%.*"
  ..$ inline.code   : chr "\\\\\\\\rinline\\\\\{([^\}]+)\\\\\}"
  ..$ header.begin  : chr "(^|\\n)[^\%]*\\\\s*\\\\\\\\documentclass[^\}]+\\\\\}"
  ..$ document.begin: chr "\\\\s*\\\\\\\\begin\\\\\{document\\\\\}"
 $ html    :List of 5
  ..$ chunk.begin : chr "^\\\\s*<!--\\\\s*begin.rcode\\\\s*(.*)"
  ..$ chunk.end   : chr "^\\\\s*end.rcode\\\\s*-->"
  ..$ ref.chunk   : chr "^\\\\s*<<(.+)>>\\\\s*$"
  ..$ inline.code : chr "<!--\\\\s*rinline(.+?)-->"
  ..$ header.begin: chr "\\\\s*<head>"
 $ md      :List of 4
  ..$ chunk.begin: chr "^\\\\s*```+\\\\s*\\\\\{r(.*)\\\\\}\\\\s*$"
  ..$ chunk.end  : chr "^\\\\s*```+\\\\s*$"
  ..$ ref.chunk  : chr "^\\\\s*<<(.+)>>\\\\s*$"
  ..$ inline.code: chr "`r +([^`]+)\\\\s*`"
 $ rst     :List of 5
  ..$ chunk.begin: chr "^\\\\s*[.][.]\\\\s+\\\\\{r(.*)\\\\\}\\\\s*$"
  ..$ chunk.end  : chr "^\\\\s*[.][.]\\\\s+[.][.]\\\\s*$"
  ..$ chunk.code : chr "^[.][.]"
  ..$ ref.chunk  : chr "^\\\\.*\\\\s*<<(.+)>>\\\\s*$"
  ..$ inline.code: chr ":r:`([^`]+)`"
 $ asciidoc:List of 6
  ..$ chunk.begin   : chr "^//\\\\s*begin[.]rcode(.*)$"
  ..$ chunk.end     : chr "^//\\\\s*end[.]rcode\\\\s*$"
  ..$ chunk.code    : chr "^//+"
  ..$ ref.chunk     : chr "^\\\\s*<<(.+)>>\\\\s*$"
  ..$ inline.code   : chr "[+]r +([^+]+)\\\\s*[+]"
  ..$ inline.comment: chr "^//.*"
 $ textile :List of 5
  ..$ chunk.begin   : chr "^###[.]\\\\s+begin[.]rcode(.*)$"
  ..$ chunk.end     : chr "^###[.]\\\\s+end[.]rcode\\\\s*$"
  ..$ ref.chunk     : chr "^\\\\s*<<(.+)>>\\\\s*$"
  ..$ inline.code   : chr "@r +([^@]+)\\\\s*@"
  ..$ inline.comment: chr "^###[.].*"
}}
\usage{
all_patterns
}
\description{
This object is a named list of all built-in patterns.
}
\examples{
all_patterns$rnw
all_patterns$html

str(all_patterns)
}
\references{
Usage: \url{http://yihui.name/knitr/patterns}
}
\seealso{
\code{\link{knit_patterns}}
}
\keyword{datasets}
