\docType{methods}
\name{compare}
\alias{compare}
\alias{compare,validator-method}
\title{Compare similar data sets}
\usage{
compare(x, ...)

\S4method{compare}{validator}(x, how = c("to_first", "sequential"),
  .list = NULL, ...)
}
\arguments{
  \item{x}{An R object (usually a \code{\link{validator}}
  or \code{\link{indicator}}}

  \item{...}{(named) data sets (\emph{e.g.} data.frames)}

  \item{how}{how to compare}

  \item{.list}{Optional list of data sets, will be
  concatenated with \code{...}.}
}
\description{
The purpose of this function is to compare different
versions of the same dataset with respect to predifined
indicators. It is expected that the number and order of
columns and rows are the identical for each dataset.
}

