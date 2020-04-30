\name{remove}
\alias{remove}

\title{Remove a subset}

\description{Selects the row and the varaibles to remove by specifing a condition using a formula.}


\usage{
remove(from, formula, na.remove = FALSE, ...)
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
Returns a data.frame object without the selected elements.

}



\author{Alessio Serafini}


\examples{
head(remove(from = dt, formula = .~ I(Ozone > 10)))
head(remove(from = dt, formula = .~ I(Ozone > 10), na.remove = TRUE))
head(remove(from = dt, formula = Ozone ~ .))

head(remove(from = dt, formula = Ozone~ I(Ozone > 10)))
head(remove(from = dt, formula = Ozone + Wind~ I(Ozone > 10)))

head(remove(from = dt, formula = Ozone + . ~ I(Ozone > 10)))
head(remove(from = dt, formula = Ozone + NULL ~ I(Ozone > 10)))


}
