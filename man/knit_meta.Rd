% Please edit documentation in R/output.R
\name{knit_meta}
\alias{knit_meta}
\alias{knit_meta_add}
\title{Metadata about objects to be printed}
\usage{
knit_meta(class = NULL, clean = TRUE)

knit_meta_add(meta, label = "")
}
\arguments{
\item{class}{optionally return only metadata entries that inherit from the
specified class; the default, \code{NULL}, returns all entries.}

\item{clean}{whether to clean the collected metadata; by default, the
metadata stored in \pkg{knitr} is cleaned up once retrieved, because we may
not want the metadata to be passed to the next \code{knit()} call; to be
defensive (i.e. not to have carryover metadata), you can call
\code{knit_meta()} before \code{knit()}}

\item{meta}{a metadata object to be added to the session}

\item{label}{a chunk label to indicate which chunk the metadata belongs to}
}
\value{
\code{knit_meta()} returns the matched metadata specified by
  \code{class}; \code{knit_meta_add()} returns all current metadata.
}
\description{
As an object is printed, \pkg{knitr} will collect metadata about it (if
available). After knitting is done, all the metadata is accessible via this
function. You can manually add metadata to the \pkg{knitr} session via
\code{knit_meta_add()}.
}
