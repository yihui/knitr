% Please edit documentation in R/utils.R
\name{combine_words}
\alias{combine_words}
\title{Combine multiple words into a single string}
\usage{
combine_words(words, sep = ", ", and = " and ")
}
\arguments{
\item{words}{a character vector}

\item{sep}{the separator to be inserted among words}

\item{and}{a character string to be prepended to the last word}
}
\value{
A character string.
}
\description{
When a value from an inline R expression is a character vector of multiple
elements, we may want to combine them into a phrase like \samp{a and b}, or
\code{a, b, and c}. That is what this a helper function does.
}
\details{
If the length of the input \code{words} is smaller than or equal to 1,
\code{words} is returned. When \code{words} is of length 2, the first word
and second word are combined using the \code{and} string. When the length is
greater than 2, \code{sep} is used to separate all words, and the \code{and}
string is prepended to the last word.
}
\examples{
combine_words("a")
combine_words(c("a", "b"))
combine_words(c("a", "b", "c"))
combine_words(c("a", "b", "c"), sep = " / ", and = "")
combine_words(c("a", "b", "c"), and = "")
}
