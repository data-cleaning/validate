\docType{methods}
\name{calls,confrontation-method}
\alias{calls}
\alias{calls,confrontation-method}
\alias{calls,validatorValue-method}
\alias{calls,verifier-method}
\title{Retrieve calls from object}
\usage{
\S4method{calls}{confrontation}(x, ...)

\S4method{calls}{validatorValue}(x, ...)

calls(x, ...)

\S4method{calls}{verifier}(x, ..., expand = TRUE, vectorize = TRUE,
  replace_dollar = TRUE)
}
\arguments{
  \item{x}{An R object}

  \item{...}{arguments to be passed to other methods}

  \item{expand}{Expand groups?}

  \item{vectorize}{Vectorize if-statements?}

  \item{replace_dollar}{Replace dollar with bracket index?}
}
\value{
A \code{list} of calls
}
\description{
This function is exported mostly as a utility for other
packages depending on \code{validate}.
}

