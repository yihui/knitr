% Please edit documentation in R/cache.R
\name{clean_cache}
\alias{clean_cache}
\title{Clean cache files that are probably no longer needed}
\usage{
clean_cache(clean = FALSE, path = opts_chunk$get("cache.path"))
}
\arguments{
\item{clean}{whether to remove the files}

\item{path}{the cache path}
}
\description{
If you remove or rename some cached code chunks, their original cache files
will not be automatically cleaned. You can use this function to identify
these possible files, and clean them if you are sure they are no longer
needed.
}
\note{
The identification is not guaranteed to be correct, especially when
  multiple documents share the same cache directory. You are recommended to
  call \code{clean_cache(FALSE)} and carefully check the list of files (if
  any) before you really delete them (\code{clean_cache(TRUE)}).

  This function must be called within a code chunk in a source document,
  since it needs to know all chunk labels of the current document to
  determine which labels are no longer present, and delete cache
  corresponding to these labels.
}
