\name{remove}
\alias{remove}

\title{Remove a subset}

\description{Selects the row and the varaibles to remove by specifing a condition using a formula.}


\usage{
remove(from, formula = .~., na.remove = FALSE, ...)
}

\arguments{
  \item{from}{a data.frame object with variables}
  \item{formula}{a formula indicating the operation to create new varibles. Look at the detail section for explanantion.}
 \item{na.remove}{a logical value indicating whether NA values should be removed.}

  \item{\dots}{further arguments}
}
\details{

The formula is composed of two part:

\code{column_names ~ rows_conditions}

the left-hand side are the names of the column to remove, and the right-hand the operation to remove the rows, using the \code{I()} function.

For example:

\code{column_names1 + column_names2 ~ I(column_names1 == "a") + I(column_names2 > 4)}

first the row are selected to be removed if the observation in the \code{column_names1} are equal to \code{a} and if the observation in the \code{column_names2} are biggers than \code{4}, then the \code{column_names1} and \code{column_names2} are removed and the other varaibles are returned.


If \code{na.remove} is set to \code{TRUE}, after the subsetting the observations with missing are removed.
}

\value{
Returns a data.frame object without the selected elements.
}



\author{Alessio Serafini}


\examples{

data("airquality")
dt <- airquality

head(remove(from = dt, formula = .~ I(Ozone > 10)))
head(remove(from = dt, formula = .~ I(Ozone > 10), na.remove = TRUE))
head(remove(from = dt, formula = Ozone ~ .))

head(remove(from = dt, formula = Ozone~ I(Ozone > 10)))
head(remove(from = dt, formula = Ozone + Wind~ I(Ozone > 10)))

head(remove(from = dt, formula = Ozone + . ~ I(Ozone > 10)))
head(remove(from = dt, formula = Ozone + NULL ~ I(Ozone > 10)))


}
