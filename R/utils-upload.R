#' Upload an image to imgur.com
#'
#' This function uses the \pkg{httr} package to upload a image to
#' \url{https://imgur.com}, and parses the XML response to a list with
#' \pkg{xml2} which contains information about the image in the Imgur website.
#'
#' When the output format from \code{\link{knit}()} is HTML or Markdown, this
#' function can be used to upload local image files to Imgur, e.g. set the
#' package option \code{opts_knit$set(upload.fun = imgur_upload)}, so the output
#' document is completely self-contained, i.e. it does not need external image
#' files any more, and it is ready to be published online.
#' @param file Path to the image file to be uploaded.
#' @param key Client ID for Imgur. By default, this uses a client ID registered
#'   by Yihui Xie.
#' @return A character string of the link to the image; this string carries an
#'   attribute named \code{XML} which is a list converted from the response XML
#'   file; see Imgur API in the references.
#' @author Yihui Xie, adapted from the \pkg{imguR} package by Aaron Statham
#' @note Please register your own Imgur application to get your client ID; you
#'   can certainly use mine, but this ID is in the public domain so everyone has
#'   access to all images associated to it.
#' @references Imgur API version 3: \url{https://apidocs.imgur.com}; a demo:
#'   \url{https://yihui.name/knitr/demo/upload/}
#' @export
#' @examples \dontrun{
#' f = tempfile(fileext = '.png')
#' png(f); plot(rnorm(100), main = R.version.string); dev.off()
#'
#' res = imgur_upload(f)
#' res  # link to original URL of the image
#' attr(res, 'XML')  # all information
#' if (interactive()) browseURL(res)
#'
#' # to use your own key
#' opts_knit$set(upload.fun = function(file) imgur_upload(file, key = 'your imgur key'))
#' }
imgur_upload = function(file, key = '9f3460e67f308f6') {
  if (!is.character(key)) stop('The Imgur API Key must be a character string!')
  resp = httr::POST(
    "https://api.imgur.com/3/image.xml",
    config = httr::add_headers(Authorization = paste("Client-ID", key)),
    body = list(image = httr::upload_file(file))
  )
  httr::stop_for_status(resp, "upload to imgur")
  res = httr::content(resp, as = "raw")
  res = if (length(res)) xml2::as_list(xml2::read_xml(res))
  # Breaking change in xml2 1.2.0
  if (packageVersion('xml2') >= '1.2.0') res <- res[[1L]]
  if (is.null(res$link[[1]])) stop('failed to upload ', file)
  structure(res$link[[1]], XML = res)
}
