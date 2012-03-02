\name{imgur_upload}
\alias{imgur_upload}
\title{Upload a image to imgur.com}
\usage{
imgur_upload(file, key = opts_knit$get("imgur.key"))
}
\arguments{
  \item{file}{the path to the image file to be uploaded}

  \item{key}{the API key for Imgur (by default uses a key
  created by Yihui Xie, which allows 50 uploads per hour
  per IP address)}
}
\value{
  A character string of the link to the image; this string
  carries an attribute named \code{XML} which is a list
  converted from the response XML file; see Imgur API in
  the references.
}
\description{
  This function uses the \pkg{RCurl} package to upload a
  image to \url{imgur.com}, and parses the XML response to
  a list with \pkg{XML} which contains information about
  the image in the Imgur website.
}
\details{
  When the output format from \code{\link{knit}()} is HTML
  or Markdown, this function will be used to upload local
  image files to Imgur when the package option
  \code{upload} is \code{TRUE}
  (\code{opts_knit$get('upload')}), so the output document
  is completely self-contained, i.e. it does not need
  external image files any more, and it is ready to be
  published online.
}
\note{
  Please register your own Imgur application to get your
  API key; you can certainly use mine, but this key is in
  the public domain so everyone has access to all images
  associated to it.
}
\examples{
f = tempfile(fileext = ".png")
png(f)
plot(rnorm(100), main = R.version.string)
dev.off()
\dontrun{
res = imgur_upload(f)
res  # link to original URL of the image
attr(res, "XML")  # all information
if (interactive()) 
    browseURL(res$links$imgur_page)  # imgur page
}
}
\author{
  Yihui Xie, adapted from the \pkg{imguR} package by Aaron
  Statham
}
\references{
  Imgur API: \url{http://api.imgur.com/}; a demo:
  \url{http://yihui.name/knitr/demo/upload/}
}
