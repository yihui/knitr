#' Upload an image to imgur.com
#'
#' This function uses the \pkg{curl} package to upload a image to
#' \url{https://imgur.com}, and parses the XML response to a list with
#' \pkg{xml2}, which contains information about the image on Imgur.
#'
#' When the output format from \code{\link{knit}()} is HTML or Markdown, this
#' function can be used to upload local image files to Imgur, e.g. set the
#' package option \code{opts_knit$set(upload.fun = imgur_upload)}, so the output
#' document is completely self-contained, i.e. it does not need external image
#' files any more, and it is ready to be published online.
#' @param file Path to the image file to be uploaded.
#' @param key Client ID for Imgur. It can be set via either the global option
#'   \code{knitr.imgur.key} or the environment variable \code{R_KNITR_IMGUR_KEY}
#'   (see [xfun::env_option()]). If neither is set, this uses a client ID
#'   registered by Yihui Xie.
#' @return A character string of the link to the image; this string carries an
#'   attribute named \code{XML} which is a list converted from the response XML
#'   file; see Imgur API in the references.
#' @author Yihui Xie, adapted from the \pkg{imguR} package by Aaron Statham
#' @note Please register your own Imgur application to get your client ID; you
#'   can certainly use mine, but this ID is in the public domain so everyone has
#'   access to all images associated to it.
#' @references A demo: \url{https://yihui.org/knitr/demo/upload/}
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
#' options(knitr.imgur.key = 'your imgur key')
#' }
imgur_upload = function(file, key = xfun::env_option('knitr.imgur.key', '9f3460e67f308f6')) {
  if (!is.character(key)) stop('The Imgur API Key must be a character string!')
  h = curl::new_handle(httpheader = paste("Authorization: Client-ID", key))
  curl::handle_setform(h, image = curl::form_file(file))
  res = curl::curl_fetch_memory('https://api.imgur.com/3/image.xml', h)$content
  if (loadable('xml2')) {
    res = xml2::as_list(xml2::read_xml(res))
    link = res[[1]]$link[[1]]
  } else {
    res = rawToChar(res)
    link = xfun::grep_sub('.*<link>([^<]+)</link>.*', '\\1', res)
  }
  if (length(link) != 1) stop(
    'Failed to upload ', file, sprintf(' (reason: %s)', if (is.character(res)) {
      xfun::grep_sub('.*<error>([^<]+)</error>.*', '\\1', res)
    } else res[[1]]$error[[1]])
  )
  structure(link, XML = res)
}
