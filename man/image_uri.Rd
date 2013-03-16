\name{image_uri}
\alias{image_uri}
\title{Encode an image file to a data URI}
\usage{
image_uri(f)
}
\arguments{
  \item{f}{the path to the image file}
}
\value{
  a character string (the data URI)
}
\description{
  This function takes an image file and uses either the
  \pkg{markdown} package, or \pkg{RCurl} or the built-in
  function to encode it as a base64 string, which can be
  used in the \code{img} tag in HTML.
}
\examples{
uri = image_uri(file.path(R.home("doc"), "html", "logo.jpg"))
cat(sprintf("<img src=\"\%s\" />", uri), file = "logo.html")
if (interactive()) browseURL("logo.html")  # you can check its HTML source
}
\author{
  Wush Wu and Yihui Xie
}
\references{
  \url{http://en.wikipedia.org/wiki/Data_URI_scheme}
}
