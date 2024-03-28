#' Upload an image to imgur.com
#'
#' This function is an alias to \code{xfun::upload_imgur()}. It is kept in
#' \pkg{knitr} only for backward-compatibility reasons. You are recommended to
#' use \code{xfun::upload_imgur()} directly instead.
#' @param file,key,... See \code{xfun::\link[xfun]{upload_imgur}()}.
#' @export
#' @keywords internal
imgur_upload = function(file, key = xfun::env_option('knitr.imgur.key', '9f3460e67f308f6'), ...) {
  xfun::upload_imgur(file, key, ..., include_xml = TRUE)
}
