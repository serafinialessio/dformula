\name{select}
\alias{select}

\title{Select a subset}
\description{
Selects the row and the varaibles by specifing a condition using a formula.}
\usage{
select(from, formula, na.remove = FALSE, ...)
}

\arguments{
  \item{from}{a data.frame object with variables}
  \item{formula}{a formula indicating the operation to create new varibles. Look at the detail section for explanantion.}
 \item{na.remove}{a logical value indicating whether NA values should be removed.}

  \item{\dots}{further arguments}
}
\details{
If \code{na.remove} is set to \code{TRUE}, after the subsetting the observations with missing are removed.
}

\value{
Returns a data.frame object containing the selected elements.
}


\author{Alessio Serafini}


\examples{
data("airquality")
dt <- airquality

## Selects columns and filter rows

select(from = dt, formula = .~ I(Ozone > 10 & Wind > 10))
select(from = dt, formula = Ozone ~ I(Wind > 10))
select(from = dt, formula = Ozone + Wind~ I(Ozone > 10))

## All rows and filter columns

select(from = dt, formula = Ozone ~ .)
select(from = dt, formula = Ozone + Wind ~ NULL)

##

select(from = dt, formula = .~ Ozone)


}

