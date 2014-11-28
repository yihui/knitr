\name{knit_meta}
\alias{knit_meta}
\title{Metadata about objects to be printed}
\usage{
knit_meta(class = NULL, clean = TRUE)
}
\arguments{
\item{class}{optionally return only metadata entries that inherit from the
specified class; the default, \code{NULL}, returns all entries.}

\item{clean}{whether to clean the collected metadata; by default, the
metadata stored in \pkg{knitr} is cleaned up once retrieved, because we may
not want the metadata to be passed to the next \code{knit()} call; to be
defensive (i.e. not to have carryover metadata), you can call
\code{knit_meta()} before \code{knit()}}
}
\description{
As an object is printed, \pkg{knitr} will collect metadata about it (if
available). After knitting is done, all the metadata is accessible via this
function.
}
