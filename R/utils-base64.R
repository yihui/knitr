#' Encode an image file to a data URI
#'
#' This function takes an image file and uses the \pkg{markdown} package to
#' encode it as a base64 string, which can be used in the \code{img} tag in
#' HTML.
#' @param f the path to the image file
#' @return a character string (the data URI)
#' @author Wush Wu and Yihui Xie
#' @export
#' @references \url{http://en.wikipedia.org/wiki/Data_URI_scheme}
#' @examples uri = image_uri(file.path(R.home('doc'), 'html', 'logo.jpg'))
#' cat(sprintf('<img src="%s" />', uri), file = 'logo.html')
#' if (interactive()) browseURL('logo.html') # you can check its HTML source
image_uri = function(f) markdown:::.b64EncodeFile(f)

# alternative approaches to base64 encoding
image_uri2 = function(f) {
  content = readBin(f, what = 'raw', n = file.info(f)$size)
  uri = if (has_package('RCurl')) {
    paste(RCurl::base64Encode(content, 'character'), collapse = '')
  } else base64_encode(content)
  paste0('data:', mime_type(f), ';base64,', uri)
}

base64_table = c(LETTERS, letters, 0:9, '+', '/')

# base64 encode a raw string
base64_encode = function(raw.string) {
  n = length(s <- as.integer(raw.string))
  res = rep(NA, (n + 2) / 3 * 4)
  i = 0L  # index of res vector
  j = 1L  # index of base64_table
  while (n > 2L) {
    res[i <- i + 1L] = base64_table[s[j] %/% 4L + 1L]
    res[i <- i + 1L] = base64_table[16 * (s[j] %% 4L) + s[j + 1L] %/% 16 + 1L]
    res[i <- i + 1L] = base64_table[4L * (s[j + 1L] %% 16) + s[j + 2L] %/% 64L + 1L]
    res[i <- i + 1L] = base64_table[s[j + 2L] %% 64L + 1L]
    j = j + 3L
    n = n - 3L
  }
  if (n) {
    res[i <- i + 1L] = base64_table[s[j] %/% 4L + 1L]
    if (n > 1L) {
      res[i <- i + 1L] = base64_table[16 * (s[j] %% 4L) + s[j + 1L] %/% 16 + 1L]
      res[i <- i + 1L] = base64_table[4L * (s[j + 1L] %% 16) + 1L]
      res[i <- i + 1L] = '='
    } else {
      res[i <- i + 1L] = base64_table[16 * (s[j] %% 4L) + 1L]
      res[i <- i + 1L] = '='
      res[i <- i + 1L] = '='
    }
  }
  paste(res[!is.na(res)], collapse = '')
}

# lazy man's mime function
mime_type = function(file) {
  ext = tolower(file_ext(file))
  switch(ext, svg = 'image/svg+xml', jpg = 'image/jpeg',
         paste('image', ext, sep = '/'))
}
