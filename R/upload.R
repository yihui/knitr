#' Upload a image to imgur.com
#' 
#' This function uses the \pkg{RCurl} package to upload a image to 
#' \url{imgur.com}, and parses the XML response to a list with \pkg{XML} which 
#' contains information about the image in the Imgur website.
#' 
#' When the output format from \code{\link{knit}()} is HTML or Markdown, this 
#' function can be used to upload local image files to Imgur, e.g. set the 
#' package option \code{opts_knit$get(upload.fun = imgur_upload)}, so the output
#' document is completely self-contained, i.e. it does not need external image
#' files any more, and it is ready to be published online.
#' @param file the path to the image file to be uploaded
#' @param key the API key for Imgur (by default uses a key created by Yihui Xie,
#'   which allows 50 uploads per hour per IP address)
#' @return A character string of the link to the image; this string carries an 
#'   attribute named \code{XML} which is a list converted from the response XML
#'   file; see Imgur API in the references.
#' @author Yihui Xie, adapted from the \pkg{imguR} package by Aaron Statham
#' @note Please register your own Imgur application to get your API key; you can
#'   certainly use mine, but this key is in the public domain so everyone has 
#'   access to all images associated to it.
#' @references Imgur API: \url{http://api.imgur.com/}; a demo: 
#'   \url{http://yihui.name/knitr/demo/upload/}
#' @export
#' @examples f = tempfile(fileext = '.png')
#' png(f); plot(rnorm(100), main = R.version.string); dev.off()
#' \dontrun{
#' res = imgur_upload(f)
#' res  # link to original URL of the image
#' attr(res, 'XML')  # all information
#' if (interactive()) browseURL(res$links$imgur_page) # imgur page
#' 
#' ## to use your own key
#' opts_knit$get(upload.fun = function(file) imgur_upload(file, key = 'your imgur key'))
#' }
imgur_upload = function(file, key = '60e9e47cff8483c6dc289a1cd674b40f') {
  if (is.null(key) || nchar(key) != 32L)
    stop('The Imgur API Key must be a character string of length 32!')
  if (!file.exists(file)) stop(file, 'does not exist!')
  params = list(key = key, image = RCurl::fileUpload(file))
  res = XML::xmlToList(RCurl::postForm("http://api.imgur.com/2/upload.xml", .params = params))
  if (is.null(res$links$original)) stop('failed to upload ', file)
  structure(res$links$original, XML = res)
}
