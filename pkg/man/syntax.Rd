\name{syntax}
\alias{L}
\alias{V}
\alias{fraction_missing}
\alias{number_missing}
\alias{syntax}
\title{Syntax to define validation rules}
\usage{
number_missing(...)

fraction_missing(...)

V(rule, impact = NULL, severity = NULL)

L(linrule, p = 2)
}
\arguments{
  \item{...}{comma-separated list of variable names (not
  character). If no variables are specified, the number of
  missings over all data is counted.}

  \item{rule}{R expression: a validation rule. Must result
  in a (vector of) logical(s).}

  \item{impact}{R expression: an expression. Must result in
  a numeric.}

  \item{severity}{R expression: an expression. Must result
  in a numeric.}

  \item{linrule}{A \emph{linear} validating expression}

  \item{p}{Lp-norm to use (default is the Euclidean norm)}
}
\value{
For \code{number_missing}, the total number of missings
over all specified variables.

For \code{fraction_missing}, the fraction of missings over
all specified variables

For \code{V} a \code{list} containing the return values of
\code{rule}, \code{impact} and \code{severity}

For \code{L}, a \code{list} containing the validator value,
the impact function and the severity function
}
\description{
The functions mentioned in this help file should only be
used in the context of defining a \code{\link{validator}}
or \code{\link{indicator}} object.
}

