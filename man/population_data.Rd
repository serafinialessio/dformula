\name{population_data}
\alias{population_data}
\docType{data}
\title{World population}
\description{World population and countries are}

\usage{data("population_data")}
\format{
  A data frame with 159 observations on the following 3 variables.
  \describe{
    \item{\code{Country}}{a character vector with countries names}
    \item{\code{Population}}{a numeric vector with population}
    \item{\code{Area}}{a numeric vector with area of the counties}
  }
}

\source{
\url{https://www.worldometers.info}
}

\examples{
data(population_data)
str(population_data)
}
\keyword{datasets}
