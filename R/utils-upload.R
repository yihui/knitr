#' Upload an image to imgur.com
#'
#' This function is an alias to \code{xfun::upload_imgur()}. It is kept in
#' \pkg{knitr} only for backward-compatibility reasons. You are recommended to
#' use \code{xfun::upload_imgur()} directly instead.
#' @param file,key,... See \code{xfun::\link[xfun]{upload_imgur}()}.
#' @export
#' @keywords internal
imgur_upload = function(file, key = xfun::env_option('knitr.imgur.key'), ...) {
  # TODO: remove this after xfun 0.51
  if (is.null(key)) key = paste(rev(strsplit('01a236c49e09142','')[[1]]), collapse = '')
  xfun::upload_imgur(file, key, ..., include_xml = TRUE)
}
