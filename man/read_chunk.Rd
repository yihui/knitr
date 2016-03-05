% Please edit documentation in R/parser.R
\name{read_chunk}
\alias{read_chunk}
\alias{read_demo}
\title{Read chunks from an external script}
\usage{
read_chunk(path, lines = readLines(path, warn = FALSE), labels = NULL, from = NULL, 
    to = NULL, from.offset = 0L, to.offset = 0L)

read_demo(topic, package = NULL, ...)
}
\arguments{
\item{path}{the path to the R script}

\item{lines}{a character vector of the code lines (by default read from
\code{path})}

\item{labels}{a character vector of chunk labels (default \code{NULL})}

\item{from,  to}{a numeric vector specifying the starting/ending line numbers
of code chunks, or a character vector; see Details}

\item{from.offset,  to.offset}{an offset to be added to \code{from}/\code{to}}

\item{topic,  package}{name of the demo and the package see \code{\link[utils]{demo}}}

\item{...}{arguments to be passed to \code{\link{read_chunk}}}
}
\value{
As a side effect, code chunks are read into the current session so
  that future chunks can (re)use the code by chunk label references.
}
\description{
Chunks can be put in an external script, and this function reads chunks into
the current \pkg{knitr} session; \code{read_demo()} is a convenience function
to read a demo script from a package.
}
\details{
There are two approaches to read external code into the current session: (1)
Use a special separator of the from \code{## ---- chunk-label} (at least four
dashes before the chunk label) in the script; (2) Manually specify the
labels, starting and ending positions of code chunks in the script.

The second approach will be used only when \code{labels} is not \code{NULL}.
For this approach, if \code{from} is \code{NULL}, the starting position is 1;
if \code{to} is \code{NULL}, each of its element takes the next element of
\code{from} minus 1, and the last element of \code{to} will be the length of
\code{lines} (e.g. when \code{from = c(1, 3, 8)} and the script has 10 lines
in total, \code{to} will be \code{c(2, 7, 10)}). Alternatively, \code{from}
and \code{to} can be character vectors as regular expressions to specify the
positions; when their length is 1, the single regular expression will be
matched against the \code{lines} vector, otherwise each element of
\code{from}/\code{to} is matched against \code{lines} and the match is
supposed to be unique so that the numeric positions returned from
\code{grep()} will be of the same length of \code{from}/\code{to}. Note
\code{labels} always has to match the length of \code{from} and \code{to}.
}
\note{
This function can only be used in a chunk which is \emph{not} cached
  (chunk option \code{cache = FALSE}), and the code is read and stored in the
  current session \emph{without} being executed (to actually run the code,
  you have to use a chunk with a corresponding label).
}
\examples{
## put this in foo.R and read_chunk('foo.R')

## ---- my-label ----
1 + 1
lm(y ~ x, data = data.frame(x = 1:10, y = rnorm(10)))

## later you can use <<my-label>>= to reference this chunk

## the 2nd approach
code = c("#@a", "1+1", "#@b", "#@a", "rnorm(10)", "#@b")
read_chunk(lines = code, labels = "foo")  # put all code into one chunk named foo
read_chunk(lines = code, labels = "foo", from = 2, to = 2)  # line 2 into chunk foo
read_chunk(lines = code, labels = c("foo", "bar"), from = c(1, 4), to = c(3, 6))
# automatically figure out 'to'
read_chunk(lines = code, labels = c("foo", "bar"), from = c(1, 4))
read_chunk(lines = code, labels = c("foo", "bar"), from = "^#@a", to = "^#@b")
read_chunk(lines = code, labels = c("foo", "bar"), from = "^#@a", to = "^#@b", 
    from.offset = 1, to.offset = -1)

## later you can use, e.g., <<foo>>=
knitr:::knit_code$get()  # use this to check chunks in the current session
knitr:::knit_code$restore()  # clean up the session
}
\author{
Yihui Xie; the idea of the second approach came from Peter
  Ruckdeschel (author of the \pkg{SweaveListingUtils} package)
}
\references{
\url{http://yihui.name/knitr/demo/externalization/}
}
