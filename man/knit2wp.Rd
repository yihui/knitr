\name{knit2wp}
\alias{knit2wp}
\title{Knit an R Markdown document and post it to WordPress}
\usage{
knit2wp(input, title = "A post from knitr", ..., shortcode = FALSE, encoding = getOption("encoding"), 
    publish = TRUE)
}
\arguments{
  \item{input}{the filename of the Rmd document}

  \item{title}{the post title}

  \item{...}{other meta information of the post, e.g.
  \code{categories = c('R', 'Stats')} and \code{mt_keywords
  = c('knitr', 'wordpress')}, etc}

  \item{shortcode}{whether to use the shortcode
  \samp{[sourcecode lang='lang']} which can be useful to
  WordPress.com users for syntax highlighting of source
  code}

  \item{publish}{whether to publish the post immediately}

  \item{encoding}{the encoding of the input file; see
  \code{\link{file}}}
}
\description{
  This function is a wrapper around the \pkg{RWordPress}
  package. It compiles an R Markdown document to HTML and
  post the results to WordPress.
}
\note{
  This function will convert the encoding of the post and
  the title to UTF-8 internally. If you have additional
  data to send to WordPress (e.g.  keywords and
  categories), you may have to manually convert them to the
  UTF-8 encoding with the \code{\link{iconv}(x, to =
  'UTF-8')} function (especially when using Windows).
}
\examples{
# see the reference
}
\author{
  William K. Morris and Yihui Xie
}
\references{
  \url{http://yihui.name/knitr/demo/wordpress/}
}
