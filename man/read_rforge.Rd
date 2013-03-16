\name{read_rforge}
\alias{read_rforge}
\title{Read source code from R-Forge}
\usage{
read_rforge(path, project, extra = "", base = "http://r-forge.r-project.org/scm/viewvc.php/*checkout*/pkg")
}
\arguments{
  \item{path}{relative path to the source script on
  R-Forge}

  \item{project}{name of the R-Forge project}

  \item{extra}{extra parameters to be passed to the URL
  (e.g. \code{extra = '&revision=48'} to check out the
  source of revision 48)}

  \item{base}{the base URL}
}
\value{
  A character vector of the source code.
}
\description{
  This function reads source code from the SVN repositories
  on R-Forge.
}
\examples{
\dontrun{
read_rforge("rgl/R/axes.R", project = "rgl")
read_rforge("rgl/R/axes.R", project = "rgl", extra = "&revision=519")
}
}
\author{
  Yihui Xie and Peter Ruckdeschel
}
