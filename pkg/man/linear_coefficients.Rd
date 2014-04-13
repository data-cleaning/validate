\docType{methods}
\name{linear_coefficients,validator-method}
\alias{linear_coefficients}
\alias{linear_coefficients,validator-method}
\title{Extract linear coeffiecients from linear expressions}
\usage{
\S4method{linear_coefficients}{validator}(x, normalize = TRUE, ...)

linear_coefficients(x, ...)
}
\arguments{
  \item{normalize}{Bring all equations in the \eqn{<} or
  \eqn{\leq} form.}

  \item{x}{An R object}

  \item{...}{Arguments to be passed to other methods}
}
\value{
A list, containing matrix \eqn{\boldsymbol{A}}, and where
possible matrix \eqn{\boldsymbol{b}} and a vector with
comparison operators.
}
\description{
Extract linear coeffiecients from linear expressions
}
\section{Details}{
  Linear expressions are expressions of the form
  \eqn{\boldsymbol{Ay}} or
  \eqn{\boldsymbol{Ay}\odot\boldsymbol{b}}, where
  \eqn{\odot\in\{<,\leq,=,\geq,>\}}. This function uses
  \code{\link{is_linear}} to find linear expressions in
  \code{x} and returns the corresponding coefficients and
  possibly the operators.
}

