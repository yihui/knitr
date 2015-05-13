% Please edit documentation in R/plot.R
\name{plot_crop}
\alias{plot_crop}
\title{Crop a plot (remove the edges) using PDFCrop or ImageMagick}
\usage{
plot_crop(x, quiet = !opts_knit$get("progress"))
}
\arguments{
\item{x}{the plot filename}

\item{quiet}{whether to suppress standard output from the command line
utility}
}
\value{
The original filename.
}
\description{
The command \command{pdfcrop x x} is executed on a PDF plot file, and
\command{convert x -trim x} is executed for other types of plot files, where
\code{x} is the plot filename.
}
\details{
The utility \command{pdfcrop} is often shipped with a LaTeX distribution, and
\command{convert} is a command in ImageMagick (Windows users may have to put
the bin path of ImageMagick into the \var{PATH} variable).
}
\references{
PDFCrop: \url{http://pdfcrop.sourceforge.net}; the
  \command{convert} command in ImageMagick:
  \url{http://www.imagemagick.org/script/convert.php}
}
